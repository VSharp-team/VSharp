import json
from dataclasses import dataclass, field
from enum import Enum

from common.game import GameMap, GameState, Reward
from dataclasses_json import config, dataclass_json


class ClientMessageType(str, Enum):
    START = "start"
    STEP = "step"
    GETTRAINMAPS = "gettrainmaps"
    GETVALIDATIONMAPS = "getvalidationmaps"


@dataclass_json
@dataclass
class ClientMessageBody:
    def type(self) -> ClientMessageType:
        pass


@dataclass_json
@dataclass
class GetTrainMapsMessageBody(ClientMessageBody):
    def type(self) -> ClientMessageType:
        return ClientMessageType.GETTRAINMAPS


@dataclass_json
@dataclass
class GetValidationMapsMessageBody(ClientMessageBody):
    def type(self) -> ClientMessageType:
        return ClientMessageType.GETVALIDATIONMAPS


@dataclass_json
@dataclass
class StartMessageBody(ClientMessageBody):
    MapId: int
    StepsToPlay: int

    def type(self) -> ClientMessageType:
        return ClientMessageType.START


@dataclass_json
@dataclass
class StepMessageBody(ClientMessageBody):
    StateId: int
    PredictedStateUsefulness: float

    def type(self) -> ClientMessageType:
        return ClientMessageType.STEP


@dataclass_json
@dataclass
class ClientMessage:
    MessageType: str = field(init=False)
    MessageBody: ClientMessageBody = field(
        metadata=config(
            encoder=lambda x: x.to_json()
            if issubclass(type(x), ClientMessageBody)
            else json.dumps(x)
        )
    )

    def __post_init__(self):
        self.MessageType = self.MessageBody.type()


@dataclass_json
@dataclass
class MapsMessageBody:
    Maps: list[GameMap]


class ServerMessageType(str, Enum):
    MAPS = "Maps"
    READY_FOR_NEXT_STEP = "ReadyForNextStep"
    MOVE_REVARD = "MoveReward"
    GAMEOVER = "GameOver"
    INCORRECT_PREDICTED_STATEID = "IncorrectPredictedStateId"


@dataclass_json
@dataclass
class ServerMessage:
    MessageType: ServerMessageType

    class DeserializationException(Exception):
        pass

    def from_json_handle(*args, expected, **kwargs):
        try:
            return expected.from_json(args[0], **kwargs)
        except Exception as e:
            raise ServerMessage.DeserializationException(
                f"{type(e)} - {e}: "
                f"tried to decode {expected}, "
                f"got raw data: {json.dumps(json.loads(args[0]), indent=2)}"
            )


@dataclass
class GameStateServerMessage(ServerMessage):
    MessageBody: GameState


@dataclass
class RewardServerMessage(ServerMessage):
    MessageBody: Reward


@dataclass
class MapsServerMessage(ServerMessage):
    MessageBody: MapsMessageBody


@dataclass
class GameOverServerMessage(ServerMessage):
    MessageBody: None
