import websocket
from enum import Enum, auto
from contextlib import closing

from common.game import GameMap
from common.game import GameState
from common.messages import Reward
from common.messages import ClientMessage
from common.messages import StartMessageBody
from common.messages import StepMessageBody
from common.messages import ServerMessage
from common.messages import ServerMessageType
from common.messages import GetAllMapsMessageBody


def get_server_maps(url) -> list[GameMap]:
    with closing(websocket.create_connection(url)) as ws:
        request_all_maps_message = ClientMessage(GetAllMapsMessageBody())
        ws.send(request_all_maps_message.to_json())

        return ServerMessage.from_json(ws.recv()).MessageBody.Maps


class NAgent:
    """
    агент для взаимодействия с сервером
    - отслеживает состояние общения
    - ловит и кидает ошибки
    - делает шаги

    можно выдавать им вебсокеты, чтобы переиспользовать
    """

    class WrongAgentStateError(Exception):
        def __init__(self, received, expected) -> None:
            super().__init__(
                f"Wrong operations order: received {received}, expected {expected}"
            )

    class GameOverError(Exception):
        pass

    class IncorrectSentStateError(Exception):
        pass

    class State(Enum):
        SENDING_NEXT_STATE = auto()
        RECEIVING_GAMESTATE = auto()
        RECEIVING_REWARD = auto()
        ERROR = auto()

    def __init__(
        self, url: str, map_id_to_play: int, steps: int, log: bool = False
    ) -> None:
        self._ws = websocket.create_connection(url)
        self.log = log

        start_message = ClientMessage(
            StartMessageBody(MapId=map_id_to_play, StepsToPlay=steps)
        )
        if log:
            print("-->", start_message, "\n")
        self._ws.send(start_message.to_json())
        self._state = NAgent.State.RECEIVING_GAMESTATE

    def _check_expected_state(self, expected):
        if self._state != expected:
            raise NAgent.WrongAgentStateError(self._state, expected)

    def recv_state_from_server(self) -> GameState:
        self._check_expected_state(NAgent.State.RECEIVING_GAMESTATE)

        received = ServerMessage.from_json(self._ws.recv())
        self._state = NAgent.State.SENDING_NEXT_STATE
        return received

    def send_step(self, next_state_id: int, predicted_usefullness: int):
        self._check_expected_state(NAgent.State.SENDING_NEXT_STATE)

        if self.log:
            print(f"{next_state_id=}")
        do_step_message = ClientMessage(
            StepMessageBody(
                StateId=next_state_id, PredictedStateUsefulness=predicted_usefullness
            )
        )
        if self.log:
            print("-->", do_step_message)
        self._ws.send(do_step_message.to_json())
        self._state = NAgent.State.RECEIVING_REWARD

    def recv_reward(self) -> Reward:
        self._check_expected_state(NAgent.State.RECEIVING_REWARD)

        received = ServerMessage.from_json(self._ws.recv())
        if self.log:
            print("<--", received, end="\n\n")

        match received.MessageType:
            case ServerMessageType.INCORRECT_PREDICTED_STATEID:
                self._state = NAgent.State.ERROR
                raise NAgent.IncorrectSentStateError(f"Server: {received.MessageType}")

            case ServerMessageType.GAMEOVER:
                self._state = NAgent.State.ERROR
                raise NAgent.GameOverError(f"Server: {received.MessageType}")

            case ServerMessageType.MOVE_REVARD:
                self._state = NAgent.State.RECEIVING_GAMESTATE
                return received

            case _:
                self.close_connection()
                self._state = NAgent.State.ERROR
                raise RuntimeError(
                    f"Unexpected message type received: {received.MessageType}"
                )

    def close_connection(self):
        # TODO: create context manager
        self._ws.close()
