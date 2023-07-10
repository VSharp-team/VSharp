from collections import defaultdict

from .classes import Agent2Result, AgentResultsOnGameMaps, GameMapsModelResults


def invert_mapping_mrgm_gmmr(
    model_results_on_map: AgentResultsOnGameMaps,
) -> GameMapsModelResults:
    inverse_mapping: GameMapsModelResults = defaultdict(list)

    for named_agent, list_of_map_result_mappings in model_results_on_map.items():
        for map_result_mapping in list_of_map_result_mappings:
            map, result = (map_result_mapping.map, map_result_mapping.game_result)

            inverse_mapping[map].append(Agent2Result(named_agent, result))

    return inverse_mapping
