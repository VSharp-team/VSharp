"""The selection module: allows to select N models from model pool with different strategies"""


import random
from typing import Callable, TypeAlias

from ml.model_wrappers.protocols import Mutable

from .classes import AgentResultsOnGameMaps
from .scorer import ScorerFunction
from .utils import invert_mapping_mrgm_gmmr, sort_by_reward_asc_steps_desc

SelectorFunction: TypeAlias = Callable[[AgentResultsOnGameMaps], list[Mutable]]


def select_all_models(model_result_with_maps: AgentResultsOnGameMaps) -> list[Mutable]:
    return model_result_with_maps.keys()


def select_n_maps_tops(
    model_result_with_maps: AgentResultsOnGameMaps, n: int
) -> list[Mutable]:
    """Chooses n models from left to right, top to bottom on the "map leaderbord"""
    map_results_with_models = invert_mapping_mrgm_gmmr(model_result_with_maps)

    all_model_results: list[list[Mutable]] = []

    for model_result_mapping_list in map_results_with_models.values():
        all_model_results.append(
            sorted(model_result_mapping_list, key=sort_by_reward_asc_steps_desc)
        )

    result_array = []

    for i in range(n):
        result_array.append(all_model_results[i % len(all_model_results)].pop())

    return [mapping.mutable for mapping in result_array]


def select_k_best(
    with_scorer: ScorerFunction,
    model_result_with_maps: AgentResultsOnGameMaps,
    k: int,
) -> list[Mutable]:
    """Return k unique best models"""
    models = model_result_with_maps.keys()

    # reversed -> in decreasing order, [:k] takes k first
    return sorted(
        models, key=lambda m: with_scorer(model_result_with_maps[m]), reverse=True
    )[:k]


def select_p_percent_best(
    with_scorer: ScorerFunction,
    model_result_with_maps: AgentResultsOnGameMaps,
    p: float,
) -> list[Mutable]:
    assert p > 0 and p < 1
    # chooses from unique models
    models = model_result_with_maps.keys()

    elements_to_return_count = int(len(models) * p)
    if elements_to_return_count == 0:
        return []

    # reversed -> in decreasing order, [:k] takes k first
    return sorted(
        models, key=lambda m: with_scorer(model_result_with_maps[m]), reverse=True
    )[:elements_to_return_count]


def tournament_selection(
    model_result_with_maps: AgentResultsOnGameMaps,
    desired_population: int,
    n_comparisons: int,
    scorer: ScorerFunction,
) -> list[Mutable]:
    """Performs tournament selection

    [source paper](https://digitalcommons.olivet.edu/cgi/viewcontent.cgi?article=1004&context=csis_stsc)
    """
    selection_pool = []

    models = list(model_result_with_maps.keys())

    for _ in range(desired_population):
        current_best = models[0]

        for _ in range(n_comparisons):
            random_model = models[random.randint(0, len(models) - 1)]
            if scorer(model_result_with_maps[random_model]) > scorer(
                model_result_with_maps[current_best]
            ):
                current_best = random_model

        selection_pool.append(current_best)

    return selection_pool
