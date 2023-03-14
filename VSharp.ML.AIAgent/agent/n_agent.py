import logging
import logging.config
from typing import Type

from common.game import GameMap, GameState
from common.constants import Constant

from .connection_manager import ConnectionManager
from .messages import (
    ClientMessage,
    GameOverServerMessage,
    GameStateServerMessage,
    GetTrainMapsMessageBody,
    GetValidationMapsMessageBody,
    MapsServerMessage,
    Reward,
    RewardServerMessage,
    ServerMessage,
    ServerMessageType,
    StartMessageBody,
    StepMessageBody,
)

logger = logging.getLogger(Constant.Loggers.AGENT_LOGGER)


def get_validation_maps(cm: ConnectionManager) -> list[GameMap]:
    return get_maps(cm, with_message_body_type=GetValidationMapsMessageBody)


def get_train_maps(cm: ConnectionManager) -> list[GameMap]:
    return get_maps(cm, with_message_body_type=GetTrainMapsMessageBody)


def get_maps(
    cm: ConnectionManager,
    with_message_body_type: Type[GetTrainMapsMessageBody]
    | Type[GetValidationMapsMessageBody],
) -> list[GameMap]:
    ws = cm.issue()
    request_all_maps_message = ClientMessage(with_message_body_type())
    ws.send(request_all_maps_message.to_json())
    maps_message = ws.recv()

    cm.release(ws)
    return MapsServerMessage.from_json_handle(
        maps_message, expected=MapsServerMessage
    ).MessageBody.Maps


class NAgent:
    class WrongAgentStateError(Exception):
        def __init__(
            self, source: str, received: str, expected: str, at_step: int
        ) -> None:
            super().__init__(
                f"Wrong operations order at step #{at_step}: at function \
                <{source}> received {received}, expected {expected}",
            )

    class IncorrectSentStateError(Exception):
        pass

    class GameOver(Exception):
        pass

    def __init__(
        self,
        cm: ConnectionManager,
        map_id_to_play: int,
        steps: int,
    ) -> None:
        self.cm = cm
        self._ws = cm.issue()

        start_message = ClientMessage(
            StartMessageBody(MapId=map_id_to_play, StepsToPlay=steps)
        )
        logger.debug(f"--> {start_message}")
        self._ws.send(start_message.to_json())
        self._current_step = 0
        self.game_is_over = False

    def _raise_if_gameover(self, msg) -> GameOverServerMessage | str:
        if self.game_is_over:
            raise NAgent.GameOver
        match ServerMessage.from_json_handle(msg, expected=ServerMessage).MessageType:
            case ServerMessageType.GAMEOVER:
                self.game_is_over = True
                raise NAgent.GameOver
            case _:
                return msg

    def recv_state_or_throw_gameover(self) -> GameState:
        received = self._ws.recv()
        data = GameStateServerMessage.from_json_handle(
            self._raise_if_gameover(received), expected=GameStateServerMessage
        )
        return data.MessageBody

    def send_step(self, next_state_id: int, predicted_usefullness: int):
        do_step_message = ClientMessage(
            StepMessageBody(
                StateId=next_state_id, PredictedStateUsefulness=predicted_usefullness
            )
        )
        logger.debug(f"--> {do_step_message}")
        self._ws.send(do_step_message.to_json())
        self._sent_state_id = next_state_id

    def recv_reward_or_throw_gameover(self) -> Reward:
        data = RewardServerMessage.from_json_handle(
            self._raise_if_gameover(self._ws.recv()), expected=RewardServerMessage
        )
        logger.debug(f"<-- {data.MessageType}")

        return self._process_reward_server_message(data)

    def _process_reward_server_message(self, msg):
        match msg.MessageType:
            case ServerMessageType.INCORRECT_PREDICTED_STATEID:
                raise NAgent.IncorrectSentStateError(
                    f"Sending state_id={self._sent_state_id} \
                    at step #{self._current_step} resulted in {msg.MessageType}"
                )

            case ServerMessageType.MOVE_REVARD:
                self._current_step += 1
                return msg.MessageBody

            case _:
                raise RuntimeError(
                    f"Unexpected message type received: {msg.MessageType}"
                )

    def close(self):
        self.cm.release(self._ws)
