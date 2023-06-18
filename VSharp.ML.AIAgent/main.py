import logging
import random

import pygad

from common.constants import SERVER_COUNT
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


def on_generation(ga_instance):
    print("on_generation()")


def main():
    clean_tables_file()
    clean_log_file()

    GeneticLearner.set_static_model()

    num_generations = 5
    num_parents_mating = 3

    parent_selection_type = ParentSelectionType.STEADY_STATE_SELECTION
    keep_parents = 1

    crossover_type = CrossoverType.SINGLE_POINT

    mutation_type = MutationType.RANDOM
    mutation_percent_genes = 30

    num_genes = 8
    num_agents = 8

    initial_population = [
        [random.random() * 2 - 1 for _ in range(num_genes)] for _ in range(num_agents)
    ]

    ga_instance = pygad.GA(
        num_generations=num_generations,
        num_parents_mating=num_parents_mating,
        fitness_func=fitness_function,
        num_genes=num_genes,
        initial_population=initial_population,
        parent_selection_type=parent_selection_type,
        keep_parents=keep_parents,
        crossover_type=crossover_type,
        mutation_type=mutation_type,
        mutation_percent_genes=mutation_percent_genes,
        gene_space={"low": -1.0, "high": 1.0},
        parallel_processing=SERVER_COUNT,
        on_generation=on_generation,
        save_solutions=True,
    )

    ga_instance.run()

    ga_instance.plot_fitness(save_dir="./ga_plots/1.png")
    ga_instance.plot_genes(save_dir="./ga_plots/2.png")
    ga_instance.plot_new_solution_rate(save_dir="./ga_plots/3.png")

    print(ga_instance.population)

    solution, solution_fitness, solution_idx = ga_instance.best_solution()

    print(f"Parameters of the best solution : {solution}, {sum(solution)=}")
    print(f"Fitness value of the best solution = {solution_fitness}")

    ga_instance.save("./last_ga_instance.pkt")


if __name__ == "__main__":
    main()
