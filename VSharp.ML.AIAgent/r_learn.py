import logging
from collections import defaultdict
from contextlib import closing, suppress
from itertools import product

from agent.connection_manager import ConnectionManager
from agent.n_agent import NAgent
from common.game import GameMap, MoveReward
from common.utils import compute_coverage_percent, get_states
from constants import Constant
from displayer.tables import display_pivot_table
from ml.model_wrappers.protocols import Predictor
from ml.mutation_gen import (
    GameMapsModelResults,
    MutableResult,
    MutableResultMapping,
    Mutator,
)

logger = logging.getLogger(Constant.Loggers.ML_LOGGER)


def generate_games(models: list[Predictor], maps: list[GameMap]):
    # cartesian product
    return product(models, maps)


# вот эту функцию можно параллелить (внутренний for, например)
def r_learn_iteration(
    models: list[Predictor], maps: list[GameMap], steps: int, cm: ConnectionManager
) -> GameMapsModelResults:
    games = generate_games(models, maps)
    game_maps_model_results: GameMapsModelResults = defaultdict(list)

    for model, map in games:
        with closing(NAgent(cm, map_id_to_play=map.Id, steps=steps)) as agent:
            cumulative_reward = MoveReward(0, 0)
            steps_count = 0

            with suppress(NAgent.GameOver):
                for _ in range(steps):
                    game_state = agent.recv_state_or_throw_gameover()
                    predicted_state_id = model.predict(game_state)
                    logger.debug(
                        f"<{model.name()}> step: {steps_count}, available states: {get_states(game_state)}, predicted: {predicted_state_id}"
                    )
                    agent.send_step(
                        next_state_id=predicted_state_id,
                        predicted_usefullness=42.0,  # left it a constant for now
                    )

                    reward = agent.recv_reward_or_throw_gameover()
                    cumulative_reward += reward.ForMove
                    steps_count += 1

                _ = agent.recv_state_or_throw_gameover()  # wait for gameover
                steps_count += 1

        coverage_percent = compute_coverage_percent(
            game_state, cumulative_reward.ForCoverage
        )
        model_result: MutableResultMapping = MutableResultMapping(
            model, MutableResult(cumulative_reward, steps_count, coverage_percent)
        )

        logger.info(
            f"<{model}> finished map {map.NameOfObjectToCover} in {steps} steps, "
            f"coverage: {coverage_percent:.2f}%, "
            f"reward.ForVisitedInstructions: {cumulative_reward.ForVisitedInstructions}"
        )
        game_maps_model_results[map].append(model_result)

    return game_maps_model_results


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
        game_maps_model_results = r_learn_iteration(
            models, maps, steps, connection_manager
        )
        models = mutator.new_generation(game_maps_model_results)
        display_pivot_table(game_maps_model_results)

    survived = "\n"
    for model in models:
        survived += str(model) + "\n"
    logger.info(f"survived models: {survived}")
