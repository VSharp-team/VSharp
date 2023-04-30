import concurrent.futures
import logging
import queue
from collections import defaultdict
from contextlib import suppress
from itertools import product
from typing import Callable, Optional, TypeAlias

import multiprocessing_logging
import tqdm
import websocket

from agent.n_agent import NAgent, get_train_maps, get_validation_maps
from common.constants import Constant
from common.game import GameMap, MoveReward
from common.utils import covered, get_states
from displayer.tables import create_pivot_table
from displayer.utils import append_to_tables_file
from ml.model_wrappers.protocols import Mutable, Predictor
from selection.classes import (
    GameMapsModelResults,
    ModelResultsOnGameMaps,
    MutableResult,
    MutableResultMapping,
)
from selection.utils import invert_mapping_gmmr_mrgm

NewGenProviderFunction: TypeAlias = Callable[[ModelResultsOnGameMaps], list[Mutable]]


def generate_games(models: list[Predictor], maps: list[GameMap]):
    # cartesian product
    return product(models, maps)


def play_map(
    with_agent: NAgent, with_model: Predictor, steps: int
) -> MutableResultMapping:
    cumulative_reward = MoveReward(0, 0)
    steps_count = 0
    last_step_covered = None

    try:
        for _ in range(steps):
            game_state = with_agent.recv_state_or_throw_gameover()
            predicted_state_id = with_model.predict(game_state)
            logging.debug(
                f"<{with_model.name()}> step: {steps_count}, available states: {get_states(game_state)}, predicted: {predicted_state_id}"
            )

            with_agent.send_step(
                next_state_id=predicted_state_id,
                predicted_usefullness=42.0,  # left it a constant for now
            )

            reward = with_agent.recv_reward_or_throw_gameover()
            cumulative_reward += reward.ForMove
            steps_count += 1
            last_step_covered = reward.ForMove.ForCoverage

        _ = with_agent.recv_state_or_throw_gameover()  # wait for gameover
        steps_count += 1
    except NAgent.GameOver:
        pass

    factual_coverage, vertexes_in_zone = covered(game_state)
    factual_coverage += last_step_covered

    coverage_percent = factual_coverage / vertexes_in_zone * 100

    if coverage_percent != 100 and steps_count != steps:
        logging.error(
            f"<{with_model.name()}>: not all steps exshausted with non-100% coverage. steps: {steps_count}, coverage: {coverage_percent}"
        )

    model_result: MutableResultMapping = MutableResultMapping(
        with_model,
        MutableResult(
            cumulative_reward,
            steps_count,
            coverage_percent,
        ),
    )

    return model_result


def _use_user_steps(provided_steps_field: Optional[int]):
    return provided_steps_field != None


def play_game(model, game_map, max_steps, proxy_ws_queue: queue.Queue):
    ws_url = proxy_ws_queue.get()
    ws = websocket.create_connection(ws_url)
    agent = NAgent(ws, map_id_to_play=game_map.Id, steps=max_steps)
    logging.info(f"<{model}> is playing {game_map.MapName}")
    mutable_result_mapping = play_map(
        with_agent=agent, with_model=model, steps=max_steps
    )
    ws.close()
    proxy_ws_queue.put(ws_url)

    return model.name(), game_map, mutable_result_mapping


# вот эту функцию можно параллелить (внутренний for, например)
def r_learn_iteration(
    models: list[Predictor],
    maps: list[GameMap],
    user_max_steps: Optional[int],
    tqdm_desc: str,
    ws_urls: queue.Queue(),
) -> ModelResultsOnGameMaps:
    games = list(generate_games(models, maps))

    model_results_on_map: GameMapsModelResults = defaultdict(list)
    futures_queue = queue.Queue()

    with tqdm.tqdm(
        total=len(games), desc=tqdm_desc, **Constant.TQDM_FORMAT_DICT
    ) as pbar:

        def done_callback(x):
            model_name, game_map, mutable_result_mapping = x.result()
            logging.info(
                f"<{model_name}> finished map {game_map.MapName} "
                f"in {mutable_result_mapping.mutable_result.steps_count} steps, "
                f"coverage: {mutable_result_mapping.mutable_result.coverage_percent:.2f}%, "
                f"reward.ForVisitedInstructions: {mutable_result_mapping.mutable_result.move_reward.ForVisitedInstructions}"
            )
            pbar.update(1)

        with concurrent.futures.ProcessPoolExecutor(
            max_workers=3, initializer=multiprocessing_logging.install_mp_handler
        ) as executor:
            for model, game_map in games:
                max_steps = (
                    user_max_steps
                    if _use_user_steps(user_max_steps)
                    else game_map.MaxSteps
                )
                future = executor.submit(play_game, model, game_map, max_steps, ws_urls)
                future.add_done_callback(done_callback)
                futures_queue.put(future)
            executor.shutdown()

    while not futures_queue.empty():
        _, game_map, mutable_result_mapping = futures_queue.get().result()
        model_results_on_map[game_map].append(mutable_result_mapping)

    inverted: ModelResultsOnGameMaps = invert_mapping_gmmr_mrgm(model_results_on_map)

    return inverted


def r_learn(
    epochs: int,
    train_max_steps: int,
    models: list[Predictor],
    new_gen_provider_function: NewGenProviderFunction,
    epochs_to_verify: list[int],
    ws_urls: queue.Queue,
) -> None:
    def launch_verification(epoch):
        return epoch in epochs_to_verify

    def is_last_epoch(epoch):
        return epoch == epochs - 1

    def dump_survived(models):
        survived = ""
        for model in models:
            survived += str(model) + "\n"
        append_to_tables_file(f"survived models: {survived}")

    for epoch in range(epochs):
        epoch_string = f"Epoch {epoch + 1}/{epochs}"

        logging.info(epoch_string)
        print(epoch_string)
        map_socket_url = ws_urls.get()
        map_socket = websocket.create_connection(map_socket_url)
        train_maps = get_train_maps(map_socket)
        map_socket.close()
        ws_urls.put(map_socket_url)
        train_game_maps_model_results = r_learn_iteration(
            models=models,
            maps=train_maps,
            user_max_steps=train_max_steps,
            tqdm_desc="Train",
            ws_urls=ws_urls,
        )
        append_to_tables_file(epoch_string + "\n")
        append_to_tables_file(
            f"Train: \n" + create_pivot_table(train_game_maps_model_results) + "\n"
        )

        if launch_verification(epoch):
            map_socket_url = ws_urls.get()
            map_socket = websocket.create_connection(map_socket_url)
            validation_maps = get_validation_maps(map_socket)
            map_socket.close()
            ws_urls.put(map_socket_url)
            validation_game_maps_model_results = r_learn_iteration(
                models=models,
                maps=validation_maps,
                user_max_steps=None,
                tqdm_desc="Validation",
                ws_urls=ws_urls,
            )
            append_to_tables_file(
                f"Validation: \n"
                + create_pivot_table(validation_game_maps_model_results)
                + "\n"
            )

        dump_survived(models)
        if not is_last_epoch(epoch):
            models = new_gen_provider_function(train_game_maps_model_results)
