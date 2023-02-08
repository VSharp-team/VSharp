from contextlib import closing
import unittest

from common.game import GameState, Reward
from .n_agent import NAgent, get_validation_maps
from .connection_manager import ConnectionManager


def get_states(game_state: GameState) -> set[int]:
    states = set()
    for edge in game_state.Map:
        for state in edge.VertexFrom.States:
            states.add(state.Id)
        for state in edge.VertexTo.States:
            states.add(state.Id)

    return states


def dumb_strategy(game_state: GameState) -> int:
    return get_states(game_state).pop()


class TestServerConnection(unittest.TestCase):
    @classmethod
    def setUpClass(cls):
        urls = ["ws://0.0.0.0:8080/gameServer"]

        cls.n_steps = 1
        cls.cm = ConnectionManager(urls)

        maps = get_validation_maps(cls.cm)
        cls.test_map_id = maps[0].Id

    def do_one_dumb_step(self, with_agent: NAgent) -> Reward:
        game_state = with_agent.recv_state_or_throw_gameover().MessageBody
        next_state = dumb_strategy(game_state)
        with_agent.send_step(next_state_id=next_state, predicted_usefullness=42.0)
        return with_agent.recv_reward_or_throw_gameover()

    def test_n_plus_one_step_throws_gameover(self):
        """
        N steps on map 0 with dumb strategy should throw gameover
        """
        with closing(
            NAgent(self.cm, map_id_to_play=self.test_map_id, steps=self.n_steps)
        ) as agent:
            for _ in range(self.n_steps):
                self.do_one_dumb_step(with_agent=agent)

            self.assertRaises(
                NAgent.GameOver, lambda: self.do_one_dumb_step(with_agent=agent)
            )  # fake step to check for gameover

    def test_multiple_agents(self):
        """
        Launching multible agents in a row should be possible
        """
        for _ in range(10):
            with closing(
                NAgent(
                    self.cm,
                    map_id_to_play=self.test_map_id,
                    steps=self.n_steps,
                )
            ) as agent:
                try:
                    for _ in range(self.n_steps):
                        self.do_one_dumb_step(with_agent=agent)
                    self.do_one_dumb_step(with_agent=agent)  # fake step

                except NAgent.GameOver:
                    pass

    @classmethod
    def tearDownClass(cls) -> None:
        cls.cm.close()
