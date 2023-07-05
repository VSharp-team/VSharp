import logging

import pygad
import pygad.torchga
from torch.multiprocessing import set_start_method

from common.constants import BASE_NN_OUT_FEATURES_NUM, Constant
from epochs_statistics.utils import (
    clean_log_file,
    clean_tables_file,
    create_epochs_best_dir,
)
from ml.utils import (
    load_model_with_last_layer,
    model_weights_with_random_last_layer,
    random_model_weights,
)
from selection.crossover_type import CrossoverType
from selection.mutation_type import MutationType
from selection.parent_selection_type import ParentSelectionType

logging.basicConfig(
    level=logging.INFO,
    filename="app.log",
    filemode="a",
    format="%(asctime)s - p%(process)d: %(name)s - [%(levelname)s]: %(message)s",
)

import os

os.environ["NUMEXPR_NUM_THREADS"] = "1"

from r_learn import fitness_function, on_generation


def weights_vector(
    weights: list[float], model_path: str = Constant.IMPORTED_DICT_MODEL_PATH
):
    model = load_model_with_last_layer(
        model_path,
        weights,
    )
    assert len(weights) == BASE_NN_OUT_FEATURES_NUM
    return pygad.torchga.model_weights_as_vector(model)


def n_random_model_weights(
    n: int, low: float, hi: float, model_path: str = Constant.IMPORTED_DICT_MODEL_PATH
):
    rv = [
        random_model_weights(low=low, hi=hi, model_load_path=model_path)
        for _ in range(n)
    ]
    return rv


def n_random_last_layer_model_weights(
    n: int, low: float, hi: float, model_path: str = Constant.IMPORTED_DICT_MODEL_PATH
):
    rv = [
        model_weights_with_random_last_layer(low=low, hi=hi, model_load_path=model_path)
        for _ in range(n)
    ]
    return rv


from functools import wraps
from time import time


def timeit(func):
    @wraps(func)
    def wrapper(*args, **kwargs):
        start = time()
        rv = func(*args, **kwargs)
        end = time()
        print(
            "func:%r args:[%r, %r] took: %2.4f sec"
            % (wrapper.__name__, args, kwargs, end - start)
        )
        return rv

    return wrapper


from common.constants import BEST_MODEL_ONNX_SAVE_PATH
from ml.onnx.onnx_import import export_onnx_model
from ml.utils import create_model_from_weights_vector


@timeit
def main():
    set_start_method("spawn")
    clean_tables_file()
    clean_log_file()
    create_epochs_best_dir()

    server_count = 8

    num_models_with_random_last_layer = 8
    num_random_models = 4

    num_generations = 6
    num_parents_mating = 6
    keep_parents = 2
    parent_selection_type = ParentSelectionType.STEADY_STATE_SELECTION
    crossover_type = CrossoverType.SINGLE_POINT
    mutation_type = MutationType.RANDOM
    mutation_percent_genes = 30
    random_mutation_max_val = 5.0
    random_mutation_min_val = -5.0
    random_init_weights_max_val = 5.0
    random_init_weights_min_val = -5.0

    initial_weights = []

    pre_loaded_last_layer1 = weights_vector(
        [
            -0.7853140655460631,
            0.7524892603731441,
            0.2844810949678288,
            -0.6819831165289404,
            -0.0830326280153653,
            0.1779108098019602,
            0.95478059636744,
            0.27937866719070503,
        ],
    )
    initial_weights.append(pre_loaded_last_layer1)

    pre_loaded_last_layer2 = weights_vector(
        [
            -0.7853139452883172,
            0.752490045931864,
            0.2844807733073216,
            -0.6819766889604519,
            -0.08303258833890134,
            0.17791068654815034,
            0.9555442824877577,
            0.2793786892860371,
        ]
    )
    initial_weights.append(pre_loaded_last_layer2)

    with_random_last_layer_weights = n_random_last_layer_model_weights(
        n=num_models_with_random_last_layer,
        low=random_init_weights_min_val,
        hi=random_init_weights_max_val,
    )

    initial_weights += with_random_last_layer_weights

    with_random_weights = n_random_model_weights(
        n=num_random_models,
        low=random_init_weights_min_val,
        hi=random_init_weights_max_val,
    )

    initial_weights += with_random_weights

    ga_instance = pygad.GA(
        num_generations=num_generations,
        num_parents_mating=num_parents_mating,
        initial_population=initial_weights,
        fitness_func=fitness_function,
        on_generation=on_generation,
        save_solutions=True,
        parallel_processing=["process", server_count],
        parent_selection_type=parent_selection_type,
        keep_parents=keep_parents,
        crossover_type=crossover_type,
        mutation_type=mutation_type,
        mutation_percent_genes=mutation_percent_genes,
        random_mutation_max_val=random_mutation_max_val,
        random_mutation_min_val=random_mutation_min_val,
    )

    ga_instance.run()

    ga_instance.plot_fitness(save_dir="./ga_plots/1.png")
    ga_instance.plot_new_solution_rate(save_dir="./ga_plots/2.png")

    # Returning the details of the best solution.
    solution, solution_fitness, solution_idx = ga_instance.best_solution()
    print(
        "Fitness value of the best solution = {solution_fitness}".format(
            solution_fitness=solution_fitness
        )
    )
    print(
        "Index of the best solution : {solution_idx}".format(solution_idx=solution_idx)
    )
    ga_instance.save("./last_ga_instance")

    best_model = create_model_from_weights_vector(solution)
    export_onnx_model(best_model, BEST_MODEL_ONNX_SAVE_PATH)


if __name__ == "__main__":
    main()
