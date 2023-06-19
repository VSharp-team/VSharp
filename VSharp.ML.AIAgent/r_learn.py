import logging
import queue
from collections import defaultdict
from contextlib import closing
from typing import Callable, TypeAlias

import pygad.torchga
import torch
import tqdm
import websocket

from agent.n_agent import NAgent
from agent.utils import MapsType, get_maps
from common.constants import SOCKET_URLS, Constant
from common.game import MoveReward
from common.utils import covered, get_states
from displayer.tables import create_pivot_table, table_to_string
from displayer.utils import append_to_tables_file
from ml.model_wrappers.nnwrapper import NNWrapper
from ml.model_wrappers.protocols import Mutable, Predictor
from ml.utils import load_model_with_last_layer
from selection.classes import AgentResultsOnGameMaps, GameResult, Map2Result
from selection.scorer import minkowski_superscorer

NewGenProviderFunction: TypeAlias = Callable[[AgentResultsOnGameMaps], list[Mutable]]


def play_map(with_agent: NAgent, with_model: Predictor) -> GameResult:
    cumulative_reward = MoveReward(0, 0)
    steps_count = 0
    last_step_covered = None
    game_state = None
    actual_coverage = None
    steps = with_agent.steps

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
            last_step_covered = reward.ForMove.ForVisitedInstructions

        _ = with_agent.recv_state_or_throw_gameover()  # wait for gameover
        steps_count += 1
    except NAgent.GameOver as gameover:
        if game_state is None:
            logging.error(
                f"<{with_model.name()}>: immediate GameOver on {with_agent.map.MapName}"
            )
            return GameResult(
                move_reward=MoveReward(ForCoverage=0, ForVisitedInstructions=0),
                steps_count=steps,
                coverage_percent=0,
            )
        if gameover.actual_coverage is not None:
            actual_coverage = gameover.actual_coverage

    computed_coverage, vertexes_in_zone = covered(game_state)
    computed_coverage += last_step_covered

    coverage_percent = computed_coverage / vertexes_in_zone * 100

    actual_report = (
        f", actual coverage: {actual_coverage:.2f}"
        if actual_coverage is not None
        else ""
    )
    if (
        actual_if_exists(actual_coverage, coverage_percent) != 100
        and steps_count != steps
    ):
        logging.error(
            f"<{with_model.name()}>: not all steps exshausted on {with_agent.map.MapName} with non-100% coverage"
            f"steps taken: {steps_count}, coverage: {coverage_percent:.2f}"
            f"{actual_report}"
        )
        steps_count = steps

    model_result = GameResult(
        cumulative_reward,
        steps_count,
        coverage_percent,
        actual_coverage,
    )

    return model_result


def create_socket_queue():
    # manager = mp.Manager()
    ws_urls = queue.Queue()
    for ws_url in SOCKET_URLS:
        ws_urls.put(ws_url)
    return ws_urls


socket_queue = create_socket_queue()


loss_function = torch.nn.L1Loss()
info_for_tables: AgentResultsOnGameMaps = defaultdict(list)


def on_generation(ga_instance):
    global info_for_tables
    print(f"Generation = {ga_instance.generations_completed};")
    print(f"Fitness    = {ga_instance.best_solution()[1]};")

    ga_pop_inner_hashes = [tuple(item).__hash__() for item in ga_instance.population]
    info_for_tables_filtered = {
        k: v for k, v in info_for_tables.items() if k._hash in ga_pop_inner_hashes
    }

    pivot, stats = create_pivot_table(info_for_tables_filtered)
    append_to_tables_file(table_to_string(pivot) + "\n")
    append_to_tables_file(table_to_string(stats) + "\n")


def fitness_function_with_steps(ga_inst, solution, solution_idx, max_steps) -> float:
    global socket_queue, info_for_tables
    maps_type = MapsType.TRAIN

    #################### MODEL ####################
    # model = nn.Sequential(
    #     StateModelEncoder(hidden_channels=64, out_channels=8),
    #     nn.Linear(in_features=8, out_features=1),
    # )
    model = load_model_with_last_layer(
        Constant.IMPORTED_DICT_MODEL_PATH, [1 for _ in range(8)]
    )
    model_weights_dict = pygad.torchga.model_weights_as_dict(
        model=model, weights_vector=solution
    )

    model.load_state_dict(model_weights_dict)
    predictor = NNWrapper(model, weights_flat=solution)

    ###############################################
    if info_for_tables[predictor] != []:
        rst = [map2result.game_result for map2result in info_for_tables[predictor]]
        return minkowski_superscorer(rst, k=2)

    ws_url = socket_queue.get()
    maps = get_maps(ws_string=ws_url, type=maps_type)

    rst: list[GameResult] = []
    with closing(websocket.create_connection(ws_url)) as ws, tqdm.tqdm(
        total=len(maps),
        desc=f"{predictor.name():20}: {maps_type.value}",
        **Constant.TQDM_FORMAT_DICT,
    ) as pbar:
        for game_map in maps:
            logging.info(f"<{predictor.name()}> is playing {game_map.MapName}")

            agent = NAgent(ws, game_map, max_steps)
            game_result = play_map(with_agent=agent, with_model=predictor)
            rst.append(game_result)

            actual_report = (
                f"actual coverage: {game_result.actual_coverage_percent:.2f}, "
                if game_result.actual_coverage_percent is not None
                else ""
            )
            logging.info(
                f"<{predictor.name()}> finished map {game_map.MapName} "
                f"in {game_result.steps_count} steps, "
                f"coverage: {game_result.coverage_percent:.2f}%, "
                f"{actual_report}"
                f"reward.ForVisitedInstructions: {game_result.move_reward.ForVisitedInstructions}"
            )
            pbar.update(1)
            info_for_tables[predictor].append(Map2Result(game_map, game_result))
    socket_queue.put(ws_url)

    return minkowski_superscorer(rst, k=2)


def actual_if_exists(actual_coverage, manual_coverage):
    if actual_coverage is not None:
        return actual_coverage
    return manual_coverage
