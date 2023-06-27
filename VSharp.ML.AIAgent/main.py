import logging
import random

import pygad
import pygad.torchga
from torch.multiprocessing import set_start_method

from common.constants import Constant
from displayer.utils import clean_log_file, clean_tables_file
from ml.utils import load_model_with_last_layer
from selection.crossover_type import CrossoverType
from selection.mutation_type import MutationType
from selection.parent_selection_type import ParentSelectionType

logging.basicConfig(
    level=logging.INFO,
    filename="app.log",
    filemode="a",
    format="%(asctime)s - p%(process)d: %(name)s - [%(levelname)s]: %(message)s",
)


from r_learn import fitness_function, on_generation


def main():
    set_start_method("spawn")
    clean_tables_file()
    clean_log_file()

    server_count = 8

    num_agents = 14
    num_generations = 6
    num_parents_mating = 6
    keep_parents = 2
    parent_selection_type = ParentSelectionType.STEADY_STATE_SELECTION
    crossover_type = CrossoverType.SINGLE_POINT
    mutation_type = MutationType.RANDOM
    mutation_percent_genes = 30
    random_mutation_max_val = 5.0
    random_mutation_min_val = -5.0

    initial_weights = []

    pre_loaded_last_layer1 = load_model_with_last_layer(
        Constant.IMPORTED_DICT_MODEL_PATH,
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

    pre_loaded_last_layer2 = load_model_with_last_layer(
        Constant.IMPORTED_DICT_MODEL_PATH,
        [
            -0.7853139452883172,
            0.752490045931864,
            0.2844807733073216,
            -0.6819766889604519,
            -0.08303258833890134,
            0.17791068654815034,
            0.9555442824877577,
            0.2793786892860371,
        ],
    )
    initial_weights.append(pre_loaded_last_layer2)

    with_random_weights = [
        load_model_with_last_layer(
            Constant.IMPORTED_DICT_MODEL_PATH,
            [2 * random.random() - 1 for _ in range(8)],
        )
        for _ in range(num_agents - len(initial_weights))
    ]

    initial_weights += with_random_weights

    initial_weights = [
        pygad.torchga.model_weights_as_vector(model) for model in initial_weights
    ]

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


if __name__ == "__main__":
    main()
