from collections import defaultdict
from typing import Callable, TypeAlias

from ml.model_wrappers.protocols import Mutable
from .utils import sort_by_reward_asc_steps_desc

from .classes import (
    GameMapsModelResults,
    MapResultMapping,
    ModelResultsOnGameMaps,
)

SelectorFunction: TypeAlias = Callable[[GameMapsModelResults], list[Mutable]]


def select_all_models(model_results_on_map: GameMapsModelResults) -> list[Mutable]:
    return invert_mapping(model_results_on_map).keys()


def select_n_best(model_results_on_map: GameMapsModelResults, n: int) -> list[Mutable]:
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


def select_k_euclidean_best(
    model_results_on_map: GameMapsModelResults, k: int
) -> list[Mutable]:
    # chooses k euclidean closest from unique models
    model_results_on_game_maps = invert_mapping(model_results_on_map)

    def sorter_for_mutable(model: Mutable):
        results: list[MapResultMapping] = model_results_on_game_maps[model]
        euc_dist = sum(
            [(100 - res.mutable_result.coverage_percent) ** 2 for res in results]
        )
        visited_instructions_sum = sum(
            [res.mutable_result.move_reward.ForVisitedInstructions for res in results]
        )
        steps_sum = sum([res.mutable_result.steps_count for res in results])
        return (euc_dist, visited_instructions_sum, -steps_sum)

    models = model_results_on_game_maps.keys()

    return sorted(models, key=sorter_for_mutable)[:k]
