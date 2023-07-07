from .game import GameState


def get_states(game_state: GameState) -> set[int]:
    return {s.Id for s in game_state.States}
