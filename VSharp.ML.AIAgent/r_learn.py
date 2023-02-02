from contextlib import closing
from itertools import product
from collections import defaultdict

from common.game import GameMap, Reward, MoveReward
from agent.connection_manager import ConnectionManager
from agent.n_agent import NAgent
from ml.torch_model_wrapper import TorchModelWrapper
from ml.mutation_gen import MutatorConfig, Mutator
from ml.mutation_gen import IterationResults


def generate_games(models: list[TorchModelWrapper], maps: list[GameMap]):
    # cartesian product
    return product(models, maps)


# вот эту функцию можно параллелить (внутренний for, например)
def r_learn_iteration(models, maps, steps, ws) -> IterationResults:
    games = generate_games(models, maps)
    iteration_data: IterationResults = defaultdict(list)

    for model, map in games:
        with closing(NAgent(ws, map_id_to_play=map.Id, steps=steps, log=True)) as agent:
            cumulative_reward = MoveReward(0, 0)

            try:
                for _ in range(steps):
                    game_state = agent.recv_state_or_throw_gameover()
                    agent.send_step(
                        next_state_id=0,
                        predicted_usefullness=42.0,  # left it a constant for now
                    )

                    reward = agent.recv_reward_or_throw_gameover()
                    cumulative_reward += reward.ForMove
                _ = agent.recv_state_or_throw_gameover()  # wait for gameover

            except NAgent.GameOver:
                break

            iteration_data[map].append((model, cumulative_reward))

    return iteration_data


def r_learn(
    epochs,
    steps,
    models: list[TorchModelWrapper],
    maps,
    mutator_config: MutatorConfig,
    connection_manager: ConnectionManager,
):
    mutator = Mutator(mutator_config)
    for _ in range(epochs):
        iteration_data = r_learn_iteration(models, maps, steps, connection_manager)
        models = mutator.new_generation(iteration_data)
