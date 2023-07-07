import logging

import pygad
import pygad.torchga
from torch.multiprocessing import set_start_method

from epochs_statistics.utils import (
    init_epochs_best_dir,
    init_log_file,
    init_tables_file,
)
from selection.crossover_type import CrossoverType
from selection.mutation_type import MutationType
from selection.parent_selection_type import ParentSelectionType
from timer.resources_manager import manage_inference_stats

logging.basicConfig(
    level=logging.INFO,
    filename="app.log",
    filemode="a",
    format="%(asctime)s - p%(process)d: %(name)s - [%(levelname)s]: %(message)s",
)

import os

os.environ["NUMEXPR_NUM_THREADS"] = "1"

from .r_learn import fitness_function, on_generation


def run(
    server_count: int,
    num_generations: int,
    num_parents_mating: int,
    keep_parents: int,
    parent_selection_type: ParentSelectionType,
    crossover_type: CrossoverType,
    mutation_type: MutationType,
    mutation_percent_genes: float,
    random_mutation_max_val: float = 5.0,
    random_mutation_min_val: float = -5.0,
    initial_population: list = None,
):
    set_start_method("spawn")
    init_tables_file()
    init_log_file()
    init_epochs_best_dir()

    ga_instance = pygad.GA(
        num_generations=num_generations,
        num_parents_mating=num_parents_mating,
        initial_population=initial_population,
        fitness_func=fitness_function,
        on_generation=on_generation,
        parallel_processing=["process", server_count],
        parent_selection_type=parent_selection_type,
        keep_parents=keep_parents,
        crossover_type=crossover_type,
        mutation_type=mutation_type,
        mutation_percent_genes=mutation_percent_genes,
        random_mutation_max_val=random_mutation_max_val,
        random_mutation_min_val=random_mutation_min_val,
    )

    with manage_inference_stats():
        ga_instance.run()

    ga_instance.save("./last_ga_instance")
