import logging
import os
import shutil
from contextlib import contextmanager

import pygad
import pygad.torchga
from torch.multiprocessing import set_start_method

from common.constants import APP_LOG_FILE, BASE_REPORT_DIR
from config import FeatureConfig, GeneralConfig
from epochs_statistics.utils import (
    create_report_dir,
    init_epochs_best_dir,
    init_log_file,
    init_tables_file,
)
from learning.selection.crossover_type import CrossoverType
from learning.selection.mutation_type import MutationType
from learning.selection.parent_selection_type import ParentSelectionType
from learning.timer.resources_manager import manage_inference_stats

logging.basicConfig(
    level=GeneralConfig.LOGGER_LEVEL,
    filename=APP_LOG_FILE,
    filemode="a",
    format="%(asctime)s - p%(process)d: %(name)s - [%(levelname)s]: %(message)s",
)

os.environ["NUMEXPR_NUM_THREADS"] = "1"

from .genetic_learning import fitness_function, on_generation


@contextmanager
def move_app_log():
    try:
        yield
    finally:
        shutil.move(APP_LOG_FILE, BASE_REPORT_DIR / APP_LOG_FILE)


def run(
    server_count: int,
    num_generations: int,
    num_parents_mating: int,
    keep_elitism: int,
    parent_selection_type: ParentSelectionType,
    crossover_type: CrossoverType,
    mutation_type: MutationType,
    mutation_percent_genes: float,
    random_mutation_max_val: float = 5.0,
    random_mutation_min_val: float = -5.0,
    initial_population: list = None,
):
    set_start_method("spawn")

    create_report_dir()
    init_tables_file()
    init_log_file()
    init_epochs_best_dir()
    FeatureConfig.DUMP_BY_TIMEOUT.create_save_path_if_not_exists()
    FeatureConfig.SAVE_EPOCHS_COVERAGES.create_save_path_if_not_exists()

    ga_instance = pygad.GA(
        num_generations=num_generations,
        num_parents_mating=num_parents_mating,
        initial_population=initial_population,
        fitness_func=fitness_function,
        on_generation=on_generation,
        parallel_processing=["process", server_count],
        parent_selection_type=parent_selection_type,
        keep_elitism=keep_elitism,
        crossover_type=crossover_type,
        mutation_type=mutation_type,
        mutation_percent_genes=mutation_percent_genes,
        random_mutation_max_val=random_mutation_max_val,
        random_mutation_min_val=random_mutation_min_val,
    )

    with manage_inference_stats(), move_app_log():
        ga_instance.run()

    ga_instance.save("./last_ga_instance")
