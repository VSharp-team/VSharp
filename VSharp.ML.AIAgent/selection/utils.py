from collections import defaultdict

from .classes import (
    GameMapsModelResults,
    Map2Result,
    ModelResultsOnGameMaps,
    Mutable2Result,
)


def sort_by_reward_asc_steps_desc(
    mutable_mapping: Mutable2Result,
):
    # sort by <MoveRewardReward, -StepsCount (less is better)>
    result = mutable_mapping.game_result
    return (
        result.move_reward.ForCoverage,
        result.move_reward.ForVisitedInstructions,
        -result.steps_count,
    )


def invert_mapping_gmmr_mrgm(
    model_results_on_map: GameMapsModelResults,
) -> ModelResultsOnGameMaps:
    inverse_mapping: ModelResultsOnGameMaps = defaultdict(list)

    for map, list_of_mutable_result_mappings in model_results_on_map.items():
        for mutable_result_mapping in list_of_mutable_result_mappings:
            mutable, result = (
                mutable_result_mapping.mutable,
                mutable_result_mapping.game_result,
            )
            inverse_mapping[mutable].append(Map2Result(map, result))

    return inverse_mapping


def invert_mapping_mrgm_gmmr(
    model_results_on_map: ModelResultsOnGameMaps,
) -> GameMapsModelResults:
    inverse_mapping: GameMapsModelResults = defaultdict(list)

    for mutable, list_of_map_result_mappings in model_results_on_map.items():
        for map_result_mapping in list_of_map_result_mappings:
            map, result = (map_result_mapping.map, map_result_mapping.game_result)

            inverse_mapping[map].append(Mutable2Result(mutable, result))

    return inverse_mapping
