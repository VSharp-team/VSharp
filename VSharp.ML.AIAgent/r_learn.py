from contextlib import closing, suppress
from itertools import product
from collections import defaultdict

from common.game import GameMap, MoveReward
from agent.connection_manager import ConnectionManager
from agent.n_agent import NAgent
from ml.model_wrappers.protocols import RLearner
from ml.mutation_gen import MutatorConfig, Mutator
from ml.mutation_gen import ModelResult, IterationResults


def generate_games(models: list[RLearner], maps: list[GameMap]):
    # cartesian product
    return product(models, maps)


# вот эту функцию можно параллелить (внутренний for, например)
def r_learn_iteration(models, maps, steps, cm) -> IterationResults:
    games = generate_games(models, maps)
    iteration_data: IterationResults = defaultdict(list)

    for model, map in games:
        with closing(NAgent(cm, map_id_to_play=map.Id, steps=steps, log=True)) as agent:
            cumulative_reward = MoveReward(0, 0)
            steps_count = 0

            with suppress(NAgent.GameOver):
                for _ in range(steps):
                    game_state = agent.recv_state_or_throw_gameover()
                    agent.send_step(
                        next_state_id=0,
                        predicted_usefullness=42.0,  # left it a constant for now
                    )

                    reward = agent.recv_reward_or_throw_gameover()
                    cumulative_reward += reward.ForMove
                    steps_count += 1

                _ = agent.recv_state_or_throw_gameover()  # wait for gameover
                steps_count += 1

        model_result: ModelResult = (model, (cumulative_reward, steps_count))
        iteration_data[map].append(model_result)

    return iteration_data


def r_learn(
    epochs: int,
    steps: int,
    models: list[RLearner],
    maps: list[GameMap],
    mutator_config: MutatorConfig,
    connection_manager: ConnectionManager,
) -> None:
    mutator = Mutator(mutator_config, RLearner)
    for _ in range(epochs):
        iteration_data = r_learn_iteration(models, maps, steps, connection_manager)
        models = mutator.new_generation(iteration_data)
