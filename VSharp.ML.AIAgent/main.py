import logging

import pygad
import pygad.torchga

from common.constants import DEVICE, Constant
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


from r_learn import on_generation, fitness_function


def main():
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

    model = load_model_with_last_layer(
        Constant.IMPORTED_DICT_MODEL_PATH, [1 for _ in range(8)]
    )
    model.to(DEVICE)
    model.eval()
    tga = pygad.torchga.TorchGA(model=model, num_solutions=num_agents)

    initial_population = tga.population_weights

    ga_instance = pygad.GA(
        num_generations=num_generations,
        num_parents_mating=num_parents_mating,
        initial_population=initial_population,
        fitness_func=fitness_function,
        on_generation=on_generation,
        save_solutions=True,
        parallel_processing=["process", server_count],
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
    ga_instance.save("./last_ga_instance")


if __name__ == "__main__":
    main()
