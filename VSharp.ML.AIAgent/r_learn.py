import logging
from collections import defaultdict
from contextlib import closing, suppress
from itertools import product
from typing import Callable, TypeAlias

from agent.connection_manager import ConnectionManager
from agent.n_agent import NAgent
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
            logging.debug(
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
            compute_coverage_percent(game_state),
        ),
    )

    return model_result


cached: dict[tuple[Predictor, GameMap], MutableResultMapping] = {}


def invalidate_cache(predictors: list[Predictor]):
    for cached_predictor, cached_game_map in list(cached.keys()):
        if cached_predictor not in predictors:
            cached.pop((cached_predictor, cached_game_map))


# вот эту функцию можно параллелить (внутренний for, например)
def r_learn_iteration(
    models: list[Predictor], maps: list[GameMap], steps: int, cm: ConnectionManager
) -> GameMapsModelResults:
    games = generate_games(models, maps)
    model_results_on_map: GameMapsModelResults = defaultdict(list)

    for model, map in games:
        if (model, map) in cached.keys():
            mutable_result_mapping = cached[(model, map)]
            from_cache = True
        else:
            with closing(NAgent(cm, map_id_to_play=map.Id, steps=steps)) as agent:
                logging.info(f"{model.name()} is playing {map.NameOfObjectToCover}")
                mutable_result_mapping = play_map(
                    with_agent=agent, with_model=model, steps=steps
                )
            from_cache = False

        model_results_on_map[map].append(mutable_result_mapping)

        logging.info(
            f"{'[cached]' if from_cache else ''}<{model}> finished map {map.NameOfObjectToCover} in {steps} steps, "
            f"coverage: {mutable_result_mapping.mutable_result.coverage_percent:.2f}%, "
            f"reward.ForVisitedInstructions: {mutable_result_mapping.mutable_result.move_reward.ForVisitedInstructions}"
        )
        cached[(model, map)] = mutable_result_mapping

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
        logging.info(f"epoch# {epoch}")
        game_maps_model_results = r_learn_iteration(
            models, maps, steps, connection_manager
        )
        models = new_gen_provider_function(game_maps_model_results)
        display_pivot_table(game_maps_model_results)
        invalidate_cache(models)

    survived = "\n"
    for model in models:
        survived += str(model) + "\n"
    logging.info(f"survived models: {survived}")
