import logging
import random

import pygad
import pygad.torchga

from common.constants import SERVER_COUNT, Constant
from ml.utils import load_model_with_last_layer
from displayer.utils import clean_log_file, clean_tables_file
from ml.model_wrappers.genetic_learner import GeneticLearner
from selection.crossover_type import CrossoverType
from selection.mutation_type import MutationType
from selection.parent_selection_type import ParentSelectionType

logging.basicConfig(
    level=logging.INFO,
    filename="app.log",
    filemode="a",
    format="%(asctime)s - p%(process)d: %(name)s - [%(levelname)s]: %(message)s",
)


from r_learn import fitness_function


def callback_generation(ga_instance):
    print(
        "Generation = {generation}".format(generation=ga_instance.generations_completed)
    )
    print("Fitness    = {fitness}".format(fitness=ga_instance.best_solution()[1]))


def main():
    clean_tables_file()
    clean_log_file()

    model = load_model_with_last_layer(
        Constant.IMPORTED_DICT_MODEL_PATH, [1 for _ in range(8)]
    )
    tga = pygad.torchga.TorchGA(model=model, num_solutions=12)

    num_generations = 10
    num_parents_mating = 4
    initial_population = tga.population_weights

    parent_selection_type = ParentSelectionType.STEADY_STATE_SELECTION
    keep_parents = 1

    crossover_type = CrossoverType.SINGLE_POINT

    mutation_type = MutationType.RANDOM
    mutation_percent_genes = 10

    ga_instance = pygad.GA(
        num_generations=num_generations,
        num_parents_mating=num_parents_mating,
        initial_population=initial_population,
        fitness_func=fitness_function,
        on_generation=callback_generation,
        save_solutions=True,
        parallel_processing=SERVER_COUNT,
        parent_selection_type=parent_selection_type,
        keep_parents=keep_parents,
        crossover_type=crossover_type,
        mutation_type=mutation_type,
        mutation_percent_genes=mutation_percent_genes,
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

    # Fetch the parameters of the best solution.
    best_solution_weights = pygad.torchga.model_weights_as_dict(
        model=model, weights_vector=solution
    )
    print(best_solution_weights)
    ga_instance.save("./last_ga_instance")


if __name__ == "__main__":
    main()
