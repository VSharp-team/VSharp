import json
import logging
from collections import defaultdict
from contextlib import closing

import pygad.torchga
import tqdm
import websocket

from agent.n_agent import NAgent
from agent.utils import MapsType, get_maps
from common.constants import DEVICE, MAX_STEPS, Constant
from common.game import MoveReward
from common.utils import covered, get_states
from config import FeatureConfig
from epochs_statistics.tables import create_pivot_table, table_to_string
from epochs_statistics.utils import append_to_tables_file, create_epoch_subdir
from ml.model_wrappers.nnwrapper import NNWrapper
from ml.model_wrappers.protocols import Predictor
from ml.utils import load_model_with_last_layer
from selection.classes import AgentResultsOnGameMaps, GameResult, Map2Result
from selection.scorer import minkowski_superscorer
from ws_source.classes import Agent2ResultsOnMaps
from ws_source.requests import recv_game_result_list, send_game_results
from ws_source.ws_source import WebsocketSource


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


info_for_tables: AgentResultsOnGameMaps = defaultdict(list)


def get_n_best_weights_in_last_generation(ga_instance, n: int):
    population = ga_instance.population
    population_fitnesses = ga_instance.last_generation_fitness

    assert n <= len(
        population
    ), f"asked for {n} best when population size is {len(population)}"

    sorted_population = sorted(
        zip(population, population_fitnesses), key=lambda x: x[1], reverse=True
    )

    return list(map(lambda x: x[0], sorted_population))[:n]


def on_generation(ga_instance):
    game_results_raw = json.loads(recv_game_result_list())
    game_results_decoded = [
        Agent2ResultsOnMaps.from_json(item) for item in game_results_raw
    ]

    for full_game_result in game_results_decoded:
        info_for_tables[full_game_result.agent] = full_game_result.results

    print(f"Generation = {ga_instance.generations_completed};")
    epoch_subdir = create_epoch_subdir(ga_instance.generations_completed)

    for weights in get_n_best_weights_in_last_generation(
        ga_instance, FeatureConfig.N_BEST_SAVED_EACH_GEN
    ):
        with open(epoch_subdir / f"{sum(weights)}.txt", "w+") as weights_file:
            weights_file.write(str(weights.tolist()))

    ga_pop_inner_hashes = [tuple(item).__hash__() for item in ga_instance.population]
    info_for_tables_filtered = {
        k: v for k, v in info_for_tables.items() if k._hash in ga_pop_inner_hashes
    }

    pivot, stats = create_pivot_table(info_for_tables_filtered)
    append_to_tables_file(f"Generation = {ga_instance.generations_completed}" + "\n")
    append_to_tables_file(table_to_string(pivot) + "\n")
    append_to_tables_file(table_to_string(stats) + "\n")


def fitness_function(ga_inst, solution, solution_idx) -> float:
    maps_type = MapsType.TRAIN
    max_steps = MAX_STEPS

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
    model.to(DEVICE)
    model.eval()
    predictor = NNWrapper(model, weights_flat=solution)

    ###############################################
    # if info_for_tables[predictor] != []:
    #     rst = [map2result.game_result for map2result in info_for_tables[predictor]]
    #     return minkowski_superscorer(rst, k=2)

    with closing(WebsocketSource()) as ws_source:
        ws_url = ws_source.websocket
        maps = get_maps(ws_string=ws_url, type=maps_type)

        rst: list[GameResult] = []
        list_of_map2result: list[Map2Result] = []
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
                list_of_map2result.append(Map2Result(game_map, game_result))

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
    send_game_results(Agent2ResultsOnMaps(predictor, list_of_map2result))

    return minkowski_superscorer(rst, k=2)


def actual_if_exists(actual_coverage, manual_coverage):
    if actual_coverage is not None:
        return actual_coverage
    return manual_coverage
