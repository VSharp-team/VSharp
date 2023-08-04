import copy
import json
import random
from collections import defaultdict
from os import getpid

import pygad.torchga

import ml
from common.classes import AgentResultsOnGameMaps
from config import FeatureConfig, GeneralConfig
from connection.broker_conn.classes import Agent2ResultsOnMaps
from connection.broker_conn.requests import recv_game_result_list, send_game_results
from connection.game_server_conn.utils import MapsType
from epochs_statistics.tables import create_pivot_table, table_to_csv, table_to_string
from epochs_statistics.utils import (
    append_to_tables_file,
    create_epoch_subdir,
    rewrite_best_tables_file,
)
from learning.selection.scorer import straight_scorer
from learning.timer.stats import compute_statistics
from learning.timer.utils import (
    create_temp_epoch_inference_dir,
    dump_and_reset_epoch_times,
    load_times_array,
)
from ml.fileop import save_model
from ml.model_wrappers.nnwrapper import NNWrapper

from .play_game import play_game

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

    for weights in ga_instance.population:
        save_model(
            GeneralConfig.EXPORT_MODEL_INIT(),
            to=epoch_subdir / f"{sum(weights)}.pth",
            weights=weights,
        )

    ga_pop_inner_hashes = [
        tuple(individual).__hash__() for individual in ga_instance.population
    ]
    info_for_tables_filtered = {
        nnwrapper: res
        for nnwrapper, res in info_for_tables.items()
        if nnwrapper.weights_hash in ga_pop_inner_hashes
    }

    best_solution_hash = tuple(
        ga_instance.best_solution(pop_fitness=ga_instance.last_generation_fitness)[0]
    ).__hash__()
    best_solution_nnwrapper, best_solution_results = next(
        filter(
            lambda item: item[0].weights_hash == best_solution_hash,
            info_for_tables_filtered.items(),
        )
    )

    append_to_tables_file(
        f"Generations completed: {ga_instance.generations_completed}" + "\n"
    )

    if best_solution_nnwrapper in leader_table.keys():
        best_wrapper_copy = copy.copy(best_solution_nnwrapper)
        best_wrapper_copy.weights_hash += random.randint(0, 10**3)
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
    model = GeneralConfig.EXPORT_MODEL_INIT()
    model.forward(*ml.onnx.onnx_import.create_torch_dummy_input())
    model_weights_dict = pygad.torchga.model_weights_as_dict(
        model=model, weights_vector=solution
    )

    model.load_state_dict(model_weights_dict)
    model.to(GeneralConfig.DEVICE)
    model.eval()
    nnwrapper = NNWrapper(model, weights_flat=solution)

    list_of_map2result = play_game(
        with_predictor=nnwrapper,
        max_steps=GeneralConfig.MAX_STEPS,
        maps_type=MapsType.TRAIN,
    )
    send_game_results(Agent2ResultsOnMaps(nnwrapper, list_of_map2result))

    dump_and_reset_epoch_times(
        f"{nnwrapper.name()}_epoch{ga_inst.generations_completed}_pid{getpid()}"
    )
    rst = map(lambda map2res: map2res.game_result, list_of_map2result)
    return straight_scorer(rst)
