from abc import ABCMeta, abstractmethod
from collections import defaultdict
import random
from typing import Any, Callable, TypeAlias, TypeVar

from ml.model_wrappers.protocols import Mutable

from .classes import GameMapsModelResults, MapResultMapping, ModelResultsOnGameMaps
from .utils import sort_by_reward_asc_steps_desc


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


def select_all_models(model_results_on_map: GameMapsModelResults) -> list[Mutable]:
    return invert_mapping(model_results_on_map).keys()


def select_n_maps_tops(
    model_results_on_map: GameMapsModelResults, n: int
) -> list[Mutable]:
    # chooses n models from left to right, top to bottom on the "map leaderbord"
    all_model_results: list[list[Mutable]] = []

    for model_result_mapping_list in model_results_on_map.values():
        all_model_results.append(
            sorted(model_result_mapping_list, key=sort_by_reward_asc_steps_desc)
        )

    result_array = []

    for i in range(n):
        result_array.append(all_model_results[i % len(all_model_results)].pop())

    return [mapping.mutable for mapping in result_array]


def invert_mapping(
    model_results_on_map: GameMapsModelResults,
) -> ModelResultsOnGameMaps:
    inverse_mapping: ModelResultsOnGameMaps = defaultdict(list)

    for map, list_of_mutable_result_mappings in model_results_on_map.items():
        for mutable_result_mapping in list_of_mutable_result_mappings:
            mutable, result = (
                mutable_result_mapping.mutable,
                mutable_result_mapping.mutable_result,
            )
            inverse_mapping[mutable].append(MapResultMapping(map, result))

    return inverse_mapping


def minkowski_scorer(
    model_results_on_map: GameMapsModelResults, model: Mutable, k
) -> tuple[float, float, float]:
    model_results_on_game_maps = invert_mapping(model_results_on_map)

    results: list[MapResultMapping] = model_results_on_game_maps[model]
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
    model_results_on_map: GameMapsModelResults, model: Mutable
) -> tuple[float, float, float]:
    return minkowski_scorer(model_results_on_map, model, 1)


def euclidean_scorer(
    model_results_on_map: GameMapsModelResults, model: Mutable
) -> tuple[float, float, float]:
    return minkowski_scorer(model_results_on_map, model, 2)


def select_k_best(
    with_scorer: MultipleValScorerFunction,
    model_results_on_map: GameMapsModelResults,
    k: int,
) -> list[Mutable]:
    # chooses from unique models
    models = invert_mapping(model_results_on_map).keys()

    # reversed -> in decreasing order, [:k] takes k first
    return sorted(
        models, key=lambda m: with_scorer(model_results_on_map, m), reverse=True
    )[:k]


def select_p_percent_best(
    with_scorer: MultipleValScorerFunction,
    model_results_on_map: GameMapsModelResults,
    p: float,
) -> list[Mutable]:
    assert p > 0 and p < 1
    # chooses from unique models
    models = invert_mapping(model_results_on_map).keys()

    elements_to_return_count = int(len(models) * p)
    if elements_to_return_count == 0:
        return []

    # reversed -> in decreasing order, [:k] takes k first
    return sorted(
        models, key=lambda m: with_scorer(model_results_on_map, m), reverse=True
    )[:elements_to_return_count]


def tournament_selection(
    model_results_on_map: GameMapsModelResults,
    desired_population: int,
    n_comparisons: int,
    scorer: MultipleValScorerFunction,
) -> list[Mutable]:
    selection_pool = []

    model_results_on_game_maps = invert_mapping(model_results_on_map)
    models = list(model_results_on_game_maps.keys())

    for _ in range(desired_population):
        current_best = models[0]

        for _ in range(n_comparisons):
            random_model = models[random.randint(0, len(models) - 1)]
            if scorer(model_results_on_map, random_model) > scorer(
                model_results_on_map, current_best
            ):
                current_best = random_model

        selection_pool.append(current_best)

    return selection_pool
