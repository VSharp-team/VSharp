import logging
from statistics import StatisticsError
from time import perf_counter
from typing import TypeAlias
import random

import tqdm
from func_timeout import FunctionTimedOut, func_set_timeout

from common.classes import GameResult, Map2Result
from common.constants import TQDM_FORMAT_DICT
from common.utils import get_states
from config import FeatureConfig
from connection.broker_conn.socket_manager import game_server_socket_manager
from connection.game_server_conn.connector import Connector
from connection.game_server_conn.utils import MapsType, get_maps
from learning.timer.resources_manager import manage_map_inference_times_array
from learning.timer.stats import compute_statistics
from learning.timer.utils import get_map_inference_times
from ml.fileop import save_model
from ml.model_wrappers.protocols import Predictor

TimeDuration: TypeAlias = float


def play_map(
    with_connector: Connector, with_predictor: Predictor
) -> tuple[GameResult, TimeDuration]:
    steps_count = 0
    game_state = None
    actual_coverage = None
    steps = with_connector.steps

    start_time = perf_counter()

    try:
        for _ in range(steps):
            game_state = with_connector.recv_state_or_throw_gameover()
            predicted_state_id = with_predictor.predict(
                game_state, with_connector.map.MapName
            )
            logging.debug(
                f"<{with_predictor.name()}> step: {steps_count}, available states: {get_states(game_state)}, predicted: {predicted_state_id}"
            )

            with_connector.send_step(
                next_state_id=predicted_state_id,
                predicted_usefullness=42.0,  # left it a constant for now
            )

            _ = with_connector.recv_reward_or_throw_gameover()
            steps_count += 1

        _ = with_connector.recv_state_or_throw_gameover()  # wait for gameover
        steps_count += 1
    except Connector.GameOver as gameover:
        if game_state is None:
            logging.warning(
                f"<{with_predictor.name()}>: immediate GameOver on {with_connector.map.MapName}"
            )
            return (
                GameResult(steps, 0, 0, 0),
                perf_counter() - start_time,
            )
        if gameover.actual_coverage is not None:
            actual_coverage = gameover.actual_coverage

        tests_count = gameover.tests_count
        errors_count = gameover.errors_count

    end_time = perf_counter()

    if actual_coverage != 100 and steps_count != steps:
        logging.warning(
            f"<{with_predictor.name()}>: not all steps exshausted on {with_connector.map.MapName} with non-100% coverage"
            f"steps taken: {steps_count}, actual coverage: {actual_coverage:.2f}"
        )
        steps_count = steps

    model_result = GameResult(
        steps_count=steps_count,
        tests_count=tests_count,
        errors_count=errors_count,
        actual_coverage_percent=actual_coverage,
    )

    with_predictor.update(with_connector.map.MapName, model_result)
    return model_result, end_time - start_time


def play_map_with_stats(
    with_connector: Connector, with_predictor: Predictor
) -> tuple[GameResult, TimeDuration]:
    model_result, time_duration = play_map(with_connector, with_predictor)

    with manage_map_inference_times_array():
        try:
            map_inference_times = get_map_inference_times()
            mean, std = compute_statistics(map_inference_times)
            logging.info(
                f"Inference stats for <{with_predictor.name()}> on {with_connector.map.MapName}: {mean=}ms, {std=}ms"
            )
        except StatisticsError:
            logging.info(
                f"<{with_predictor.name()}> on {with_connector.map.MapName}: too few samples for stats count"
            )

    return model_result, time_duration


@func_set_timeout(FeatureConfig.DUMP_BY_TIMEOUT.timeout_sec)
def play_map_with_timeout(
    with_connector: Connector, with_predictor: Predictor
) -> tuple[GameResult, TimeDuration]:
    return play_map_with_stats(with_connector, with_predictor)


def play_game(with_predictor: Predictor, max_steps: int, maps_type: MapsType):
    with game_server_socket_manager() as ws:
        maps = get_maps(websocket=ws, type=maps_type)
    random.shuffle(maps)
    with tqdm.tqdm(
        total=len(maps),
        desc=f"{with_predictor.name():20}: {maps_type.value}",
        **TQDM_FORMAT_DICT,
    ) as pbar:
        list_of_map2result: list[Map2Result] = []
        for game_map in maps:
            logging.info(f"<{with_predictor.name()}> is playing {game_map.MapName}")

            try:
                play_func = (
                    play_map_with_timeout
                    if FeatureConfig.DUMP_BY_TIMEOUT.enabled
                    else play_map_with_stats
                )
                with game_server_socket_manager() as ws:
                    game_result, time = play_func(
                        with_connector=Connector(ws, game_map, max_steps),
                        with_predictor=with_predictor,
                    )
                logging.info(
                    f"<{with_predictor.name()}> finished map {game_map.MapName} "
                    f"in {game_result.steps_count} steps, {time} seconds, "
                    f"actual coverage: {game_result.actual_coverage_percent:.2f}"
                )
            except FunctionTimedOut as fto:
                game_result, time = (
                    GameResult(0, 0, 0, 0),
                    FeatureConfig.DUMP_BY_TIMEOUT.timeout_sec,
                )
                logging.warning(
                    f"<{with_predictor.name()}> timeouted on map {game_map.MapName} with {fto.timedOutAfter}s"
                )
                save_model(
                    with_predictor.model(),
                    to=FeatureConfig.DUMP_BY_TIMEOUT.save_path
                    / f"{with_predictor.name()}.pth",
                )
            list_of_map2result.append(Map2Result(game_map, game_result))
            pbar.update(1)
    return list_of_map2result
