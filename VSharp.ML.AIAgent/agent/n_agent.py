import logging
import logging.config
import queue
from contextlib import closing
from typing import Type

import websocket

from common.game import GameMap, GameState

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


def get_validation_maps(ws_strings_queue: queue.Queue) -> list[GameMap]:
    return get_maps(
        ws_strings_queue, with_message_body_type=GetValidationMapsMessageBody
    )


def get_train_maps(ws_strings_queue: queue.Queue) -> list[GameMap]:
    return get_maps(ws_strings_queue, with_message_body_type=GetTrainMapsMessageBody)


def get_maps(
    ws_strings_queue: queue.Queue,
    with_message_body_type: Type[GetTrainMapsMessageBody]
    | Type[GetValidationMapsMessageBody],
) -> list[GameMap]:
    request_all_maps_message = ClientMessage(with_message_body_type())

    map_socket_url = ws_strings_queue.get()
    with closing(websocket.create_connection(map_socket_url)) as ws:
        ws.send(request_all_maps_message.to_json())
        maps_message = ws.recv()
    ws_strings_queue.put(map_socket_url)

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
        ws: websocket.WebSocket,
        map_id_to_play: int,
        steps: int,
    ) -> None:
        self.ws = ws

        start_message = ClientMessage(
            StartMessageBody(MapId=map_id_to_play, StepsToPlay=steps)
        )
        logging.debug(f"--> StartMessage  : {start_message}")
        self.ws.send(start_message.to_json())
        self._current_step = 0
        self.game_is_over = False
        self.map_id = map_id_to_play

    def _raise_if_gameover(self, msg) -> GameOverServerMessage | str:
        if self.game_is_over:
            raise NAgent.GameOver

        matching_message_type = ServerMessage.from_json_handle(
            msg, expected=ServerMessage
        ).MessageType
        match matching_message_type:
            case ServerMessageType.GAMEOVER:
                self.game_is_over = True
                logging.debug(f"--> {matching_message_type}")
                raise NAgent.GameOver
            case _:
                return msg

    def recv_state_or_throw_gameover(self) -> GameState:
        received = self.ws.recv()
        data = GameStateServerMessage.from_json_handle(
            self._raise_if_gameover(received),
            expected=GameStateServerMessage,
        )
        logging.debug(f"<-- {data.MessageType}")
        return data.MessageBody

    def send_step(self, next_state_id: int, predicted_usefullness: int):
        do_step_message = ClientMessage(
            StepMessageBody(
                StateId=next_state_id, PredictedStateUsefulness=predicted_usefullness
            )
        )
        logging.debug(f"--> ClientMessage : {do_step_message}")
        self.ws.send(do_step_message.to_json())
        self._sent_state_id = next_state_id

    def recv_reward_or_throw_gameover(self) -> Reward:
        data = RewardServerMessage.from_json_handle(
            self._raise_if_gameover(self.ws.recv()),
            expected=RewardServerMessage,
        )
        logging.debug(f"<-- MoveReward    : {data.MessageBody}")

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
