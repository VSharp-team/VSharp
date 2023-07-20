import logging
from statistics import StatisticsError
from time import perf_counter
from typing import TypeAlias

import tqdm

from agent.n_agent import NAgent
from agent.utils import MapsType, get_maps
from common.constants import TQDM_FORMAT_DICT
from common.utils import get_states
from config import FeatureConfig, GeneralConfig
from conn.socket_manager import game_server_socket_manager
from ml.fileop import save_model
from ml.model_wrappers.protocols import WeightedPredictor
from selection.classes import GameResult, Map2Result
from timer.resources_manager import manage_map_inference_times_array
from timer.stats import compute_statistics
from timer.utils import get_map_inference_times

TimeDuration: TypeAlias = float


def play_map(
    with_agent: NAgent, with_weighted_model: WeightedPredictor
) -> tuple[GameResult, TimeDuration]:
    steps_count = 0
    game_state = None
    actual_coverage = None
    steps = with_agent.steps

    start_time = perf_counter()

    try:
        for _ in range(steps):
            game_state = with_agent.recv_state_or_throw_gameover()
            predicted_state_id = with_weighted_model.predict(game_state)
            logging.debug(
                f"<{with_weighted_model.name()}> step: {steps_count}, available states: {get_states(game_state)}, predicted: {predicted_state_id}"
            )

            with_agent.send_step(
                next_state_id=predicted_state_id,
                predicted_usefullness=42.0,  # left it a constant for now
            )

            _ = with_agent.recv_reward_or_throw_gameover()
            steps_count += 1

        _ = with_agent.recv_state_or_throw_gameover()  # wait for gameover
        steps_count += 1
    except NAgent.GameOver as gameover:
        if game_state is None:
            logging.error(
                f"<{with_weighted_model.name()}>: immediate GameOver on {with_agent.map.MapName}"
            )
            return GameResult(
                steps_count=steps,
                tests_count=0,
                errors_count=0,
                actual_coverage_percent=0,
            )
        if gameover.actual_coverage is not None:
            actual_coverage = gameover.actual_coverage

        tests_count = gameover.tests_count
        errors_count = gameover.errors_count

    end_time = perf_counter()

    if (
        FeatureConfig.DUMP_BY_TIMEOUT.enabled
        and end_time - start_time > FeatureConfig.DUMP_BY_TIMEOUT.timeout_seconds
    ):
        save_model(
            GeneralConfig.MODEL_INIT(),
            to=FeatureConfig.DUMP_BY_TIMEOUT.save_path
            / f"{with_weighted_model.name()}.pth",
            weights=with_weighted_model.weights(),
        )

    if actual_coverage != 100 and steps_count != steps:
        logging.error(
            f"<{with_weighted_model.name()}>: not all steps exshausted on {with_agent.map.MapName} with non-100% coverage"
            f"steps taken: {steps_count}, actual coverage: {actual_coverage:.2f}"
        )
        steps_count = steps

    model_result = GameResult(
        steps_count=steps_count,
        tests_count=tests_count,
        errors_count=errors_count,
        actual_coverage_percent=actual_coverage,
    )

    with manage_map_inference_times_array():
        try:
            map_inference_times = get_map_inference_times()
            mean, std = compute_statistics(map_inference_times)
            logging.info(
                f"Inference stats for <{with_weighted_model.name()}> on {with_agent.map.MapName}: {mean=}ms, {std=}ms"
            )
        except StatisticsError:
            logging.info(
                f"<{with_weighted_model.name()}> on {with_agent.map.MapName}: too few samples for stats count"
            )

    return model_result, end_time - start_time


def play_game(
    weighted_predictor: WeightedPredictor, max_steps: int, maps_type: MapsType
):
    with game_server_socket_manager() as ws:
        maps = get_maps(websocket=ws, type=maps_type)
        with tqdm.tqdm(
            total=len(maps),
            desc=f"{weighted_predictor.name():20}: {maps_type.value}",
            **TQDM_FORMAT_DICT,
        ) as pbar:
            rst: list[GameResult] = []
            list_of_map2result: list[Map2Result] = []
            for game_map in maps:
                logging.info(
                    f"<{weighted_predictor.name()}> is playing {game_map.MapName}"
                )

                game_result, time = play_map(
                    with_agent=NAgent(ws, game_map, max_steps),
                    with_weighted_model=weighted_predictor,
                )
                rst.append(game_result)
                list_of_map2result.append(Map2Result(game_map, game_result))

                logging.info(
                    f"<{weighted_predictor.name()}> finished map {game_map.MapName} "
                    f"in {game_result.steps_count} steps, {time} seconds, "
                    f"actual coverage: {game_result.actual_coverage_percent:.2f}"
                )
                pbar.update(1)
    return list_of_map2result
