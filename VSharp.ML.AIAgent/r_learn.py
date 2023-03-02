import logging
from collections import defaultdict
from contextlib import closing, suppress
from itertools import product

from agent.connection_manager import ConnectionManager
from agent.n_agent import NAgent
from common.game import GameMap, MoveReward
from common.utils import compute_coverage_percent
from constants import Constant
from ml.model_wrappers.protocols import Predictor
from ml.mutation_gen import IterationResults, ModelResult, Mutator

logger = logging.getLogger(Constant.Loggers.ML_LOGGER)


def generate_games(models: list[Predictor], maps: list[GameMap]):
    # cartesian product
    return product(models, maps)


# вот эту функцию можно параллелить (внутренний for, например)
def r_learn_iteration(models, maps, steps, cm) -> IterationResults:
    games = generate_games(models, maps)
    iteration_data: IterationResults = defaultdict(list)

    for model, map in games:
        with closing(NAgent(cm, map_id_to_play=map.Id, steps=steps)) as agent:
            cumulative_reward = MoveReward(0, 0)
            steps_count = 0

            with suppress(NAgent.GameOver):
                for _ in range(steps):
                    game_state = agent.recv_state_or_throw_gameover()
                    predicted_state_id = model.predict(game_state)
                    agent.send_step(
                        next_state_id=predicted_state_id,
                        predicted_usefullness=42.0,  # left it a constant for now
                    )

                    reward = agent.recv_reward_or_throw_gameover()
                    cumulative_reward += reward.ForMove
                    steps_count += 1

                _ = agent.recv_state_or_throw_gameover()  # wait for gameover
                steps_count += 1

        model_result: ModelResult = (model, (cumulative_reward, steps_count))
        coverage_percent = compute_coverage_percent(
            game_state, model_result[1][0].ForCoverage
        )
        logger.info(
            f"{model} finished: in {steps} steps, "
            f"with reward: {cumulative_reward} "
            f"and coverage: "
            f"{coverage_percent:.2f}"
        )
        iteration_data[map].append(model_result)

    return iteration_data


def r_learn(
    epochs: int,
    steps: int,
    models: list[Predictor],
    maps: list[GameMap],
    mutator: Mutator,
    connection_manager: ConnectionManager,
) -> None:
    for epoch in range(epochs):
        logger.info(f"epoch# {epoch}")
        iteration_data = r_learn_iteration(models, maps, steps, connection_manager)
        models = mutator.new_generation(iteration_data)

    survived = "\n"
    for model in models:
        survived += str(model) + "\n"
    logger.info(f"survived models: {survived}")
