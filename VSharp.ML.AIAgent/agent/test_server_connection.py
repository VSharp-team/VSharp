import unittest


from common.game import GameState
from .n_agent import NAgent, get_server_maps

DEFAULT_URL = "ws://0.0.0.0:8080/gameServer"


def get_states(game_state: GameState) -> set[int]:
    states = set()
    for edge in game_state.Map:
        for state in edge.VertexFrom.States:
            states.add(state.Id)
        for state in edge.VertexTo.States:
            states.add(state.Id)

    return states


def choose_state_id(game_state: GameState) -> int:
    return get_states(game_state).pop()


class TestServerConnection(unittest.TestCase):
    def setUp(self):
        maps = get_server_maps(DEFAULT_URL)
        self.n_steps = 5
        self.agent = NAgent(DEFAULT_URL, map_id_to_play=maps[0].Id, steps=self.n_steps)

    def do_one_step(self):
        next_state = choose_state_id(self.agent.recv_state_from_server().MessageBody)
        self.agent.send_step(next_state_id=next_state, predicted_usefullness=42.0)
        _ = self.agent.recv_reward()

    def test_n_steps_doesnt_throw(self):
        for _ in range(self.n_steps):
            self.do_one_step()

    def test_n_plus_one_step_throws_gameover(self):
        for _ in range(self.n_steps):
            self.do_one_step()

        self.assertRaises(NAgent.GameOverError, self.do_one_step)
        self.agent.close_connection()
    
    def tearDown(self):
        self.agent.close_connection()
