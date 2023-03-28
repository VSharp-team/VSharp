import logging
from collections import defaultdict
from contextlib import closing, suppress
from itertools import product
from typing import Callable, TypeAlias

from agent.connection_manager import ConnectionManager
from agent.n_agent import NAgent
from common.constants import Constant
from common.game import GameMap, MoveReward
from common.utils import compute_coverage_percent, get_states
from displayer.tables import display_pivot_table
from ml.model_wrappers.protocols import Mutable, Predictor
from ml.mutation.classes import (
    GameMapsModelResults,
    MutableResult,
    MutableResultMapping,
)

NewGenProviderFunction: TypeAlias = Callable[[GameMapsModelResults], list[Mutable]]

logger = logging.getLogger(Constant.Loggers.ML_LOGGER)


def generate_games(models: list[Predictor], maps: list[GameMap]):
    # cartesian product
    return product(models, maps)


def play_map(
    with_agent: NAgent, with_model: Predictor, steps: int
) -> MutableResultMapping:
    cumulative_reward = MoveReward(0, 0)
    steps_count = 0

    with suppress(NAgent.GameOver):
        for _ in range(steps):
            game_state = with_agent.recv_state_or_throw_gameover()
            predicted_state_id = with_model.predict(game_state)
            logger.debug(
                f"<{with_model.name()}> step: {steps_count}, available states: {get_states(game_state)}, predicted: {predicted_state_id}"
            )

            with_agent.send_step(
                next_state_id=predicted_state_id,
                predicted_usefullness=42.0,  # left it a constant for now
            )

            reward = with_agent.recv_reward_or_throw_gameover()
            cumulative_reward += reward.ForMove
            steps_count += 1

        _ = with_agent.recv_state_or_throw_gameover()  # wait for gameover
        steps_count += 1

    model_result: MutableResultMapping = MutableResultMapping(
        with_model,
        MutableResult(
            cumulative_reward,
            steps_count,
            compute_coverage_percent(game_state, cumulative_reward.ForCoverage),
        ),
    )

    return model_result


# вот эту функцию можно параллелить (внутренний for, например)
def r_learn_iteration(
    models: list[Predictor], maps: list[GameMap], steps: int, cm: ConnectionManager
) -> GameMapsModelResults:
    games = generate_games(models, maps)
    model_results_on_map: GameMapsModelResults = defaultdict(list)

    for model, map in games:
        with closing(NAgent(cm, map_id_to_play=map.Id, steps=steps)) as agent:
            mutable_result_mapping = play_map(
                with_agent=agent, with_model=model, steps=steps
            )

        model_results_on_map[map].append(mutable_result_mapping)

        logger.info(
            f"<{model}> finished map {map.NameOfObjectToCover} in {steps} steps, "
            f"coverage: {mutable_result_mapping.mutable_result.coverage_percent:.2f}%, "
            f"reward.ForVisitedInstructions: {mutable_result_mapping.mutable_result.move_reward.ForVisitedInstructions}"
        )

    return model_results_on_map


def r_learn(
    epochs: int,
    steps: int,
    models: list[Predictor],
    maps: list[GameMap],
    new_gen_provider_function: NewGenProviderFunction,
    connection_manager: ConnectionManager,
) -> None:
    for epoch in range(epochs):
        logger.info(f"epoch# {epoch}")
        game_maps_model_results = r_learn_iteration(
            models, maps, steps, connection_manager
        )
        models = new_gen_provider_function(game_maps_model_results)
        display_pivot_table(game_maps_model_results)

    survived = "\n"
    for model in models:
        survived += str(model) + "\n"
    logger.info(f"survived models: {survived}")
