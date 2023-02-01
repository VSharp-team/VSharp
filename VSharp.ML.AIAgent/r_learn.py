from contextlib import closing
from itertools import product
from collections import defaultdict

from common.game import GameMap, Reward, MoveReward
from agent.n_agent import NAgent
from ml.torch_model_wrapper import TorchModelWrapper
from ml.mutation_gen import MutatorConfig, Mutator
from ml.mutation_gen import IterationResults


DEFAULT_URL = "ws://0.0.0.0:8080/gameServer"


def generate_games(models: list[TorchModelWrapper], maps: list[GameMap]):
    # cartesian product
    return product(models, maps)


# вот эту функцию можно параллелить (внутренний for, например)
def r_learn_iteration(models, maps, steps) -> IterationResults:
    games = generate_games(models, maps)
    iteration_data: IterationResults = defaultdict(list)

    for model, map in games:
        with closing(
            NAgent(url=DEFAULT_URL, map_id_to_play=map.Id, steps=steps, log=True)
        ) as agent:
            cumulative_reward = MoveReward(0, 0)

            for _ in range(steps):
                game_state = agent.recv_state_from_server()
                model.train(
                    game_state,
                    lambda state_id: agent.send_step(
                        next_state_id=state_id,
                        predicted_usefullness=42.0,  # пока оставить 42
                    ),
                )

                match agent.recv_reward():
                    case Reward() as reward:
                        cumulative_reward += reward.ForMove
                    case NAgent.StepsCount:
                        # if it should be accounted for when teaching NN
                        break

            iteration_data[map].append((model, cumulative_reward))

    return iteration_data


def r_learn(
    epochs,
    steps,
    models: list[TorchModelWrapper],
    maps,
    mutator_config: MutatorConfig,
):
    mutator = Mutator(mutator_config)
    for _ in range(epochs):
        iteration_data = r_learn_iteration(models, maps, steps)
        models = mutator.new_generation(iteration_data)
