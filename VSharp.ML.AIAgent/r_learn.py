import logging
import queue
from contextlib import closing
from typing import Callable, TypeAlias

import numpy as np
import torch
import tqdm
import websocket

from agent.n_agent import NAgent
from agent.utils import MapsType, get_maps
from common.constants import MAX_STEPS, SOCKET_URLS, Constant
from common.game import GameState, MoveReward
from common.utils import covered, get_states
from ml.data_loader_compact import ServerDataloaderHeteroVector
from ml.model_wrappers.protocols import Mutable, Predictor
from ml.predict_state_vector_hetero import PredictStateVectorHetGNN
from ml.utils import load_model_with_last_layer
from selection.classes import GameResult, ModelResultsOnGameMaps
from selection.scorer import minkowski_superscorer

NewGenProviderFunction: TypeAlias = Callable[[ModelResultsOnGameMaps], list[Mutable]]


def play_map(with_agent: NAgent, with_model: Predictor) -> GameResult:
    cumulative_reward = MoveReward(0, 0)
    steps_count = 0
    last_step_covered = None
    game_state = None
    actual_coverage = None
    steps = with_agent.steps

    try:
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
            last_step_covered = reward.ForMove.ForVisitedInstructions

        _ = with_agent.recv_state_or_throw_gameover()  # wait for gameover
        steps_count += 1
    except NAgent.GameOver as gameover:
        if game_state is None:
            logging.error(
                f"<{with_model.name()}>: immediate GameOver on {with_agent.map.MapName}"
            )
            return GameResult(
                move_reward=MoveReward(ForCoverage=0, ForVisitedInstructions=0),
                steps_count=steps,
                coverage_percent=0,
            )
        if gameover.actual_coverage is not None:
            actual_coverage = gameover.actual_coverage

    computed_coverage, vertexes_in_zone = covered(game_state)
    computed_coverage += last_step_covered

    coverage_percent = computed_coverage / vertexes_in_zone * 100

    actual_report = (
        f", actual coverage: {actual_coverage:.2f}"
        if actual_coverage is not None
        else ""
    )
    if (
        actual_if_exists(actual_coverage, coverage_percent) != 100
        and steps_count != steps
    ):
        logging.error(
            f"<{with_model.name()}>: not all steps exshausted on {with_agent.map.MapName} with non-100% coverage"
            f"steps taken: {steps_count}, coverage: {coverage_percent:.2f}"
            f"{actual_report}"
        )
        steps_count = steps

    model_result = GameResult(
        cumulative_reward,
        steps_count,
        coverage_percent,
        actual_coverage,
    )

    return model_result


def create_socket_queue():
    # manager = mp.Manager()
    ws_urls = queue.Queue()
    for ws_url in SOCKET_URLS:
        ws_urls.put(ws_url)
    return ws_urls


socket_queue = create_socket_queue()


class NNWrapper(Predictor):
    def __init__(self, model: torch.nn.modules) -> None:
        self.model = model

    def name(self) -> str:
        return ""

    def predict(self, input: GameState):
        hetero_input, state_map = ServerDataloaderHeteroVector.convert_input_to_tensor(
            input
        )
        assert self.model is not None

        next_step_id = PredictStateVectorHetGNN.predict_state_single_out(
            self.model, hetero_input, state_map
        )
        del hetero_input
        return next_step_id


def fitness_function(ga_instance, solution: list[float], solution_idx) -> float:
    global socket_queue
    maps_type = MapsType.TRAIN

    ws_url = socket_queue.get()
    model = load_model_with_last_layer(
        Constant.IMPORTED_DICT_MODEL_PATH, np.array(solution)
    )
    predictor = NNWrapper(model)
    maps = get_maps(ws_string=ws_url, type=maps_type)[:8]

    model_id = sum(solution)

    rst = []
    with closing(websocket.create_connection(ws_url)) as ws, tqdm.tqdm(
        total=len(maps),
        desc=f"{model_id: 20}: {maps_type.value}",
        **Constant.TQDM_FORMAT_DICT,
    ) as pbar:
        for game_map in maps:
            logging.info(f"<{model_id}> is playing {game_map.MapName}")

            agent = NAgent(ws, game_map, MAX_STEPS)
            game_result = play_map(with_agent=agent, with_model=predictor)
            rst.append(game_result)

            actual_report = (
                f"actual coverage: {game_result.actual_coverage_percent:.2f}, "
                if game_result.actual_coverage_percent is not None
                else ""
            )
            logging.info(
                f"<{model_id}> finished map {game_map.MapName} "
                f"in {game_result.steps_count} steps, "
                f"coverage: {game_result.coverage_percent:.2f}%, "
                f"{actual_report}"
                f"reward.ForVisitedInstructions: {game_result.move_reward.ForVisitedInstructions}"
            )

            pbar.update(1)
    socket_queue.put(ws_url)

    return minkowski_superscorer(rst, k=2)


def actual_if_exists(actual_coverage, manual_coverage):
    if actual_coverage is not None:
        return actual_coverage
    return manual_coverage
