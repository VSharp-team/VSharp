from .game import GameState


def covered(state: GameState) -> tuple[int, int]:
    in_coverage_zone_blocks = 0
    covered_by_test_blocks = 0

    for vertex in state.GraphVertices:
        if vertex.InCoverageZone:
            in_coverage_zone_blocks += vertex.BasicBlockSize
            if vertex.CoveredByTest:
                covered_by_test_blocks += vertex.BasicBlockSize

    if in_coverage_zone_blocks == 0:
        raise RuntimeError("Zero blocks in coverage zone!")
    return (
        covered_by_test_blocks,
        in_coverage_zone_blocks,
    )


def get_states(game_state: GameState) -> set[int]:
    return {s.Id for s in game_state.States}
