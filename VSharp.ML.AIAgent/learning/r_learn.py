import copy
import json
import logging
import random
from collections import defaultdict
from os import getpid
from statistics import StatisticsError
from time import perf_counter
from typing import TypeAlias

import pygad.torchga
import tqdm

import ml
from agent.n_agent import NAgent
from agent.utils import MapsType, get_maps
from common.constants import DEVICE, TQDM_FORMAT_DICT
from common.utils import get_states
from config import FeatureConfig, GeneralConfig
from conn.classes import Agent2ResultsOnMaps
from conn.requests import recv_game_result_list, send_game_results
from conn.socket_manager import game_server_socket_manager
from epochs_statistics.tables import create_pivot_table, table_to_csv, table_to_string
from epochs_statistics.utils import (
    append_to_tables_file,
    create_epoch_subdir,
    rewrite_best_tables_file,
)
from ml.fileop import save_model
from ml.model_wrappers.nnwrapper import NNWrapper
from selection.classes import AgentResultsOnGameMaps, GameResult, Map2Result
from selection.scorer import straight_scorer
from timer.resources_manager import manage_map_inference_times_array
from timer.stats import compute_statistics
from timer.utils import (
    create_temp_epoch_inference_dir,
    dump_and_reset_epoch_times,
    get_map_inference_times,
    load_times_array,
)

TimeDuration: TypeAlias = float


def play_map(
    with_agent: NAgent, with_model: NNWrapper
) -> tuple[GameResult, TimeDuration]:
    steps_count = 0
    game_state = None
    actual_coverage = None
    steps = with_agent.steps

    start_time = perf_counter()

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

            _ = with_agent.recv_reward_or_throw_gameover()
            steps_count += 1

        _ = with_agent.recv_state_or_throw_gameover()  # wait for gameover
        steps_count += 1
    except NAgent.GameOver as gameover:
        if game_state is None:
            logging.error(
                f"<{with_model.name()}>: immediate GameOver on {with_agent.map.MapName}"
            )
            return GameResult(
                steps_count=steps,
                tests_count=0,
                errors_count=0,
                actual_coverage_percent=0,
            ), perf_counter() - start_time
        if gameover.actual_coverage is not None:
            actual_coverage = gameover.actual_coverage

        tests_count = gameover.tests_count
        errors_count = gameover.errors_count

    end_time = perf_counter()

    if (
        FeatureConfig.DUMP_BY_TIMEOUT.enabled
        and end_time - start_time > FeatureConfig.DUMP_BY_TIMEOUT.timeout_seconds
    ):
        save_model(
            GeneralConfig.MODEL_INIT(),
            to=FeatureConfig.DUMP_BY_TIMEOUT.save_path
            / f"{sum(with_model.weights)}.pth",
            weights=with_model.weights,
        )

    if actual_coverage != 100 and steps_count != steps:
        logging.error(
            f"<{with_model.name()}>: not all steps exshausted on {with_agent.map.MapName} with non-100% coverage"
            f"steps taken: {steps_count}, actual coverage: {actual_coverage:.2f}"
        )
        steps_count = steps

    model_result = GameResult(
        steps_count=steps_count,
        tests_count=tests_count,
        errors_count=errors_count,
        actual_coverage_percent=actual_coverage,
    )

    with manage_map_inference_times_array():
        try:
            map_inference_times = get_map_inference_times()
            mean, std = compute_statistics(map_inference_times)
            logging.info(
                f"Inference stats for <{with_model.name()}> on {with_agent.map.MapName}: {mean=}ms, {std=}ms"
            )
        except StatisticsError:
            logging.info(
                f"<{with_model.name()}> on {with_agent.map.MapName}: too few samples for stats count"
            )

    return model_result, end_time - start_time


info_for_tables: AgentResultsOnGameMaps = defaultdict(list)
leader_table: AgentResultsOnGameMaps = defaultdict(list)


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
        save_model(
            GeneralConfig.MODEL_INIT(),
            to=epoch_subdir / f"{sum(weights)}.pth",
            weights=weights,
        )

    ga_pop_inner_hashes = [
        tuple(individual).__hash__() for individual in ga_instance.population
    ]
    info_for_tables_filtered = {
        nnwrapper: res
        for nnwrapper, res in info_for_tables.items()
        if nnwrapper._hash in ga_pop_inner_hashes
    }

    best_solution_hash = tuple(
        ga_instance.best_solution(pop_fitness=ga_instance.last_generation_fitness)[0]
    ).__hash__()
    best_solution_nnwrapper, best_solution_results = next(
        filter(
            lambda item: item[0]._hash == best_solution_hash,
            info_for_tables_filtered.items(),
        )
    )

    append_to_tables_file(
        f"Generations completed: {ga_instance.generations_completed}" + "\n"
    )

    if best_solution_nnwrapper in leader_table.keys():
        best_wrapper_copy = copy.copy(best_solution_nnwrapper)
        best_wrapper_copy._hash += random.randint(0, 10**3)
        leader_table[best_wrapper_copy] = best_solution_results
    else:
        leader_table[best_solution_nnwrapper] = best_solution_results

    _, stats, _ = create_pivot_table(leader_table, sort=False)
    rewrite_best_tables_file(table_to_string(stats) + "\n")

    pivot, stats, epoch_table = create_pivot_table(info_for_tables_filtered)
    if FeatureConfig.SAVE_EPOCHS_COVERAGES.enabled:
        path_to_save_to = (
            FeatureConfig.SAVE_EPOCHS_COVERAGES.save_path
            / f"{ga_instance.generations_completed}.csv"
        )
        table_to_csv(epoch_table, path=path_to_save_to)
    append_to_tables_file(table_to_string(pivot) + "\n")
    append_to_tables_file(table_to_string(stats) + "\n")
    mean, std = compute_statistics(load_times_array())
    print(f"Gen#{ga_instance.generations_completed} inference statistics:")
    print(f"{mean=}ms")
    print(f"{std=}ms")
    create_temp_epoch_inference_dir()


def fitness_function(ga_inst, solution, solution_idx) -> float:
    maps_type = MapsType.TRAIN
    max_steps = GeneralConfig.MAX_STEPS

    model = GeneralConfig.MODEL_INIT()
    model.forward(*ml.onnx.onnx_import.create_torch_dummy_input())
    model_weights_dict = pygad.torchga.model_weights_as_dict(
        model=model, weights_vector=solution
    )

    model.load_state_dict(model_weights_dict)
    model.to(DEVICE)
    model.eval()
    nnwrapper = NNWrapper(model, weights_flat=solution)

    with game_server_socket_manager() as ws:
        maps = get_maps(websocket=ws, type=maps_type)
        with tqdm.tqdm(
            total=len(maps),
            desc=f"{nnwrapper.name():20}: {maps_type.value}",
            **TQDM_FORMAT_DICT,
        ) as pbar:
            rst: list[GameResult] = []
            list_of_map2result: list[Map2Result] = []
            for game_map in maps:
                logging.info(f"<{nnwrapper.name()}> is playing {game_map.MapName}")

                game_result, time = play_map(
                    with_agent=NAgent(ws, game_map, max_steps), with_model=nnwrapper
                )
                rst.append(game_result)
                list_of_map2result.append(Map2Result(game_map, game_result))

                logging.info(
                    f"<{nnwrapper.name()}> finished map {game_map.MapName} "
                    f"in {game_result.steps_count} steps, {time} seconds, "
                    f"actual coverage: {game_result.actual_coverage_percent:.2f}"
                )
                pbar.update(1)
    send_game_results(Agent2ResultsOnMaps(nnwrapper, list_of_map2result))

    dump_and_reset_epoch_times(
        f"{nnwrapper.name()}_epoch{ga_inst.generations_completed}_pid{getpid()}"
    )
    return straight_scorer(rst)
