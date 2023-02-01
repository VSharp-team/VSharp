from contextlib import closing
import unittest

from common.game import GameState, Reward
from .n_agent import NAgent, get_server_maps


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
        self.server_url = "ws://0.0.0.0:8080/gameServer"
        maps = get_server_maps(self.server_url)
        self.test_map_id = maps[0].Id
        self.n_steps = 5

    def do_one_step(self, with_agent: NAgent) -> Reward | NAgent.StepsCount:
        next_state = choose_state_id(with_agent.recv_state_from_server().MessageBody)
        with_agent.send_step(next_state_id=next_state, predicted_usefullness=42.0)
        return with_agent.recv_reward()

    def test_n_steps_doesnt_throw(self):
        with closing(
            NAgent(self.server_url, map_id_to_play=self.test_map_id, steps=self.n_steps)
        ) as agent:
            for _ in range(self.n_steps):
                match self.do_one_step(with_agent=agent):
                    case Reward():
                        pass
                    case NAgent.StepsCount:
                        break

    def test_n_plus_one_step_throws_gameover(self):
        with closing(
            NAgent(self.server_url, map_id_to_play=self.test_map_id, steps=self.n_steps)
        ) as agent:
            for _ in range(self.n_steps):
                match self.do_one_step(with_agent=agent):
                    case Reward():
                        pass
                    case NAgent.StepsCount:
                        break

            self.do_one_step(with_agent=agent)  # receive gameover

            self.assertRaises(
                NAgent.WrongAgentStateError, lambda: self.do_one_step(with_agent=agent)
            )
