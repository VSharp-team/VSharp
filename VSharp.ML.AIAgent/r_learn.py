from itertools import product
from collections import defaultdict

from common.game import GameMap, MoveReward
from agent.n_agent import NAgent
from ml.torch_model_wrapper import TorchModelWrapper
from ml.mutation_gen import MutationProportions, new_generation
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

        agent = NAgent(url=DEFAULT_URL, map_id_to_play=map.Id, steps=steps)
        game_state = agent.recv_state_from_server()

        cumulative_reward: MoveReward(0, 0)

        for _ in range(steps):
            model.train(
                game_state,
                lambda state: agent.send_step(
                    next_state_id=state.Id, predicted_usefullness=42.0  # ?
                ),
            )

            reward = agent.recv_reward()
            cumulative_reward += reward.ForMove

        iteration_data[map].append((model, cumulative_reward))

    return iteration_data


def r_learn(
    epochs,
    steps,
    models: list[TorchModelWrapper],
    maps,
    proportions: MutationProportions,
):
    for _ in range(epochs):
        iteration_data = r_learn_iteration(models, maps, steps)
        models = new_generation(iteration_data, proportions)
