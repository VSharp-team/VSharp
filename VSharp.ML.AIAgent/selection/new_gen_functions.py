from typing import Callable

import numpy.typing as npt
import numpy as np
import pygad

from common.constants import Constant
from ml.model_wrappers.last_layer_learner import LastLayerLearner
from ml.model_wrappers.protocols import Mutable
from selection import scorer, selectors
from selection.classes import ModelResultsOnGameMaps
from selection.crossover_type import CrossoverType
from selection.mutation_type import MutationType
from selection.parent_selection_type import ParentSelectionType


def keeps_duplicates(new_gen_func) -> Callable[[ModelResultsOnGameMaps], list[Mutable]]:
    """
    allows to create multiple instances of the same model
    """

    def wrapper(mr: ModelResultsOnGameMaps) -> list[Mutable]:
        selected_models = new_gen_func(mr)
        to_copy: tuple[Mutable, int] = []

        for unique_model in set(selected_models):
            if selected_models.count(unique_model) > 1:
                to_copy.append((unique_model, selected_models.count(unique_model) - 1))

        res: list[Mutable] = []

        for model, copy_count in to_copy:
            last_copied = model
            for _ in range(copy_count):
                new_model = last_copied.copy(last_copied.name() + "*")
                res.append(new_model)
                last_copied = new_model

        res += list(set(selected_models))

        assert len(res) == len(selected_models)
        return res

    return wrapper


def pyGAD_new_gen(mr: ModelResultsOnGameMaps) -> list[Mutable]:
    scores_for_mutables = dict()

    for mutable, results in mr.items():
        scores_for_mutables[tuple(mutable.weights)] = scorer.euclidean_distance(results)

    def fitness_function(ga_instance, vector: npt.NDArray, solution_idx):
        fitness = scores_for_mutables[tuple(vector)]
        assert fitness is not None
        return fitness

    num_generations = 1
    num_parents_mating = int(len(mr.keys()) / 3)

    num_genes = Constant.NUM_FEATURES
    parent_selection_type = ParentSelectionType.STEADY_STATE_SELECTION
    keep_parents = 1

    crossover_type = CrossoverType.SINGLE_POINT

    mutation_type = MutationType.RANDOM
    mutation_percent_genes = 30

    ga_instance = pygad.GA(
        num_generations=num_generations,
        num_parents_mating=num_parents_mating,
        fitness_func=fitness_function,
        num_genes=num_genes,
        initial_population=np.array([vec for vec in scores_for_mutables.keys()]),
        parent_selection_type=parent_selection_type,
        keep_parents=keep_parents,
        crossover_type=crossover_type,
        mutation_type=mutation_type,
        mutation_percent_genes=mutation_percent_genes,
        gene_space={"low": -1.0, "high": 1.0},
    )

    def find(weights):
        for agent in mr.keys():
            if tuple(agent.weights) == weights:
                return agent
        assert False

    solution, solution_fitness, solution_idx = ga_instance.best_solution()

    print(f"Parameters of the best solution : {find(tuple(solution))}")
    print(f"Fitness value of the best solution = {solution_fitness}")

    try:
        ga_instance.run()
    except KeyError as ke:
        print(str(ke))

    rst = []
    for solution_vector in ga_instance.population:
        if tuple(solution_vector) in scores_for_mutables.keys():
            print("append existing")
            rst.append(find(tuple(solution_vector)))
        else:
            print("append new")
            rst.append(LastLayerLearner(list(solution_vector)))

    return rst


@keeps_duplicates
def manual_new_gen(mr: ModelResultsOnGameMaps) -> list[Mutable]:
    """
    helper methods
    """

    def mutate(model: Mutable):
        return LastLayerLearner.mutate(
            model,
            mutation_volume=0.15,
            mutation_freq=0.2,
        )

    def average(models: list[Mutable]):
        return LastLayerLearner.average(models)

    """
    assemble generation
    """

    best_decart = selectors.select_k_best(scorer.decart_distance, mr, k=1)
    k_best_decart = selectors.select_k_best(scorer.decart_distance, mr, k=4)

    best_euclid = selectors.select_k_best(scorer.euclidean_distance, mr, k=1)
    k_best_euclid = selectors.select_k_best(scorer.euclidean_distance, mr, k=4)

    avg_best_tops = average(selectors.select_n_maps_tops(mr, n=1))

    avg_best_decart = average(k_best_decart)
    avg_best_euclid = average(k_best_euclid)

    return [
        *best_decart,
        *best_euclid,
        avg_best_decart,
        avg_best_euclid,
        avg_best_tops,
        *[mutate(avg_best_decart) for _ in range(4)],
        *[mutate(avg_best_euclid) for _ in range(4)],
        *[mutate(avg_best_tops) for _ in range(4)],
        *[mutate(best_decart[0]) for _ in range(4)],
        *[mutate(best_euclid[0]) for _ in range(4)],
    ]
