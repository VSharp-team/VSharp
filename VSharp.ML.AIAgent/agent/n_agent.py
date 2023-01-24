import websocket
from enum import Enum, auto
from contextlib import closing

from common.game import GameState
from common.messages import Reward
from common.messages import ClientMessage
from common.messages import StartMessageBody
from common.messages import StepMessageBody
from common.messages import ServerMessage
from common.messages import ServerMessageType
from common.messages import GetAllMapsMessageBody


def decode(msg) -> ServerMessage | GameState:
    for ServerMessageClass in [ServerMessage, GameState]:
        try:
            return ServerMessageClass.from_json(msg)
        except KeyError:
            pass

    raise RuntimeError("Couldn't decode")


def get_server_maps(url) -> ServerMessage:
    with closing(websocket.create_connection(url)) as ws:
        request_all_maps_message = ClientMessage(GetAllMapsMessageBody())
        print("-->", request_all_maps_message)
        ws.send(request_all_maps_message.to_json())

        return ServerMessage.from_json(ws.recv()).MessageBody


class NAgent:
    """
    агент для взаимодействия с сервером
    - отслеживает состояние общения
    - ловит и кидает ошибки
    - делает шаги

    можно выдавать им вебсокеты, чтобы переиспользовать
    """

    class State(Enum):
        SENDING_NEXT_STATE = auto()
        RECEIVING_GAMESTATE = auto()
        RECEIVING_REWARD = auto()
        ERROR = auto()

    def __init__(self, url: str, map_id_to_play: int, steps: int) -> None:
        self.ws = websocket.create_connection(url)

        start_message = ClientMessage(
            StartMessageBody(MapId=map_id_to_play, StepsToPlay=steps)
        )
        print("-->", start_message, "\n")
        self.ws.send(start_message.to_json())
        self.state = NAgent.State.RECEIVING_GAMESTATE

    def _check_expected_state(self, expected):
        if self.state != expected:
            raise RuntimeError(
                f"Wrong operations order: got {self.state}, received {expected}"
            )

    def recv_state_from_server(self) -> GameState:
        self._check_expected_state(NAgent.State.RECEIVING_GAMESTATE)

        received = decode(self.ws.recv())
        self.state = NAgent.State.SENDING_NEXT_STATE
        return received

    def send_step(self, next_state_id: int, predicted_usefullness: int):
        self._check_expected_state(NAgent.State.SENDING_NEXT_STATE)

        print(f"{next_state_id=}")
        do_step_message = ClientMessage(
            StepMessageBody(
                StateId=next_state_id, PredictedStateUsefulness=predicted_usefullness
            )
        )
        print("-->", do_step_message)
        self.ws.send(do_step_message.to_json())
        self.state = NAgent.State.RECEIVING_REWARD

    def recv_reward(self) -> Reward:
        self._check_expected_state(NAgent.State.RECEIVING_REWARD)

        received = decode(self.ws.recv())
        print("<--", received, end="\n\n")

        match received.MessageType:
            case ServerMessageType.INCORRECT_PREDICTED_STATEID | ServerMessageType.GAMEOVER:
                self.state = NAgent.State.ERROR
                raise RuntimeError(f"Server: {received.MessageType}")
            case ServerMessageType.MOVE_REVARD:
                self.state = NAgent.State.RECEIVING_GAMESTATE
                return received
            case _:
                self.state = NAgent.State.ERROR
                raise RuntimeError(
                    f"Unexpected message type received: {received.MessageType}"
                )
