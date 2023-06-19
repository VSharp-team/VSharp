import logging

import pygad
import pygad.torchga

from common.constants import DEVICE, SERVER_COUNT, Constant
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


from r_learn import fitness_function_with_steps, on_generation


def with_concrete_steps(steps: int):
    def fitness(ga_inst, solution, solution_idx):
        return fitness_function_with_steps(ga_inst, solution, solution_idx, steps)

    return fitness


def main():
    clean_tables_file()
    clean_log_file()

    max_steps = 100
    num_agents = 14
    num_generations = 4
    num_parents_mating = 5
    keep_parents = 2
    parent_selection_type = ParentSelectionType.STEADY_STATE_SELECTION
    crossover_type = CrossoverType.SINGLE_POINT
    mutation_type = MutationType.RANDOM
    mutation_percent_genes = 10

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
        fitness_func=with_concrete_steps(max_steps),
        on_generation=on_generation,
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
    ga_instance.save("./last_ga_instance")


if __name__ == "__main__":
    main()
