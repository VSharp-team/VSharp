import random
from abc import ABCMeta, abstractmethod
from typing import Any, Callable, TypeAlias, TypeVar

from ml.model_wrappers.protocols import Mutable

from .classes import GameMapsModelResults, MapResultMapping, ModelResultsOnGameMaps
from .utils import invert_mapping_mrgm_gmmr, sort_by_reward_asc_steps_desc


class Comparable(metaclass=ABCMeta):
    @abstractmethod
    def __lt__(self, other: Any) -> bool:
        ...


ComparableType = TypeVar("ComparableType", bound=Comparable)

SelectorFunction: TypeAlias = Callable[[GameMapsModelResults], list[Mutable]]
MultipleValScorerFunction: TypeAlias = Callable[
    [GameMapsModelResults, Mutable], tuple[ComparableType]
]
SingleValScorerFunction: TypeAlias = Callable[
    [GameMapsModelResults, Mutable], ComparableType
]


def select_all_models(model_result_with_maps: ModelResultsOnGameMaps) -> list[Mutable]:
    return model_result_with_maps.keys()


def select_n_maps_tops(
    model_result_with_maps: ModelResultsOnGameMaps, n: int
) -> list[Mutable]:
    map_results_with_models = invert_mapping_mrgm_gmmr(model_result_with_maps)

    # chooses n models from left to right, top to bottom on the "map leaderbord"
    all_model_results: list[list[Mutable]] = []

    for model_result_mapping_list in map_results_with_models.values():
        all_model_results.append(
            sorted(model_result_mapping_list, key=sort_by_reward_asc_steps_desc)
        )

    result_array = []

    for i in range(n):
        result_array.append(all_model_results[i % len(all_model_results)].pop())

    return [mapping.mutable for mapping in result_array]


def minkowski_scorer(
    model_result_with_maps: ModelResultsOnGameMaps, model: Mutable, k
) -> tuple[float, float, float]:
    results: list[MapResultMapping] = model_result_with_maps[model]
    dist = sum([abs(100 - res.mutable_result.coverage_percent) ** k for res in results])
    visited_instructions_sum = sum(
        [res.mutable_result.move_reward.ForVisitedInstructions for res in results]
    )
    steps_sum = sum([res.mutable_result.steps_count for res in results])

    # aim for:
    # decreasing dist
    # increasing visited_instructions_sum
    # decreasing steps_sum
    return (-dist, visited_instructions_sum, -steps_sum)


def decart_scorer(
    model_result_with_maps: ModelResultsOnGameMaps, model: Mutable
) -> tuple[float, float, float]:
    return minkowski_scorer(model_result_with_maps, model, 1)


def euclidean_scorer(
    model_result_with_maps: ModelResultsOnGameMaps, model: Mutable
) -> tuple[float, float, float]:
    return minkowski_scorer(model_result_with_maps, model, 2)


def select_k_best(
    with_scorer: MultipleValScorerFunction,
    model_result_with_maps: ModelResultsOnGameMaps,
    k: int,
) -> list[Mutable]:
    # chooses from unique models
    models = model_result_with_maps.keys()

    # reversed -> in decreasing order, [:k] takes k first
    return sorted(
        models, key=lambda m: with_scorer(model_result_with_maps, m), reverse=True
    )[:k]


def select_p_percent_best(
    with_scorer: MultipleValScorerFunction,
    model_result_with_maps: ModelResultsOnGameMaps,
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
        models, key=lambda m: with_scorer(model_result_with_maps, m), reverse=True
    )[:elements_to_return_count]


def tournament_selection(
    model_result_with_maps: ModelResultsOnGameMaps,
    desired_population: int,
    n_comparisons: int,
    scorer: MultipleValScorerFunction,
) -> list[Mutable]:
    selection_pool = []

    models = list(model_result_with_maps.keys())

    for _ in range(desired_population):
        current_best = models[0]

        for _ in range(n_comparisons):
            random_model = models[random.randint(0, len(models) - 1)]
            if scorer(model_result_with_maps, random_model) > scorer(
                model_result_with_maps, current_best
            ):
                current_best = random_model

        selection_pool.append(current_best)

    return selection_pool
