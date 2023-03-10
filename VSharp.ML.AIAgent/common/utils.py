from .game import GameState


def count_vertexes_in_coverage_zone(game_state: GameState):
    vertexes_ids: set[int] = set()
    in_coverage_zone = 0
    for edge in game_state.Map:
        for vertex in (edge.VertexFrom, edge.VertexTo):
            if vertex.Id not in vertexes_ids and vertex.InCoverageZone:
                in_coverage_zone += 1
                vertexes_ids.add(vertex.Id)

    return in_coverage_zone


def compute_coverage_percent(game_state: GameState, reward_for_coverage: int):
    return reward_for_coverage / count_vertexes_in_coverage_zone(game_state) * 100


def get_states(game_state: GameState) -> set[int]:
    states = set()
    for edge in game_state.Map:
        for state in edge.VertexFrom.States:
            states.add(state.Id)
        for state in edge.VertexTo.States:
            states.add(state.Id)

    return states
