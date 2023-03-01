from .game import GameState


def count_vertexes_in_coverage_zone(game_state: GameState):
    vertexes_ids: set[int] = set()
    for vertex in game_state.GraphVertices:
        if vertex.InCoverageZone:
            vertexes_ids.add(vertex.Id)

    return len(vertexes_ids)


def compute_coverage_percent(game_state: GameState, reward_for_coverage: int):
    print(count_vertexes_in_coverage_zone(game_state))
    return reward_for_coverage / count_vertexes_in_coverage_zone(game_state) * 100
