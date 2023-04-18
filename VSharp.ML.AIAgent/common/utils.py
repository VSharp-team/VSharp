from .game import GameState


def covered(state: GameState) -> tuple[int, int]:
    in_coverage_zone_vertexes_ids: set[int] = set()
    covered_by_test_vertexes_ids: set[int] = set()

    for vertex in state.GraphVertices:
        if vertex.InCoverageZone:
            in_coverage_zone_vertexes_ids.add(vertex.Id)
            if vertex.CoveredByTest:
                covered_by_test_vertexes_ids.add(vertex.Id)

    return (
        len(covered_by_test_vertexes_ids),
        len(in_coverage_zone_vertexes_ids),
    )


def get_states(game_state: GameState) -> set[int]:
    return {s.Id for s in game_state.States}
