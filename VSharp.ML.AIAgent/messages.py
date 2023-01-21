from dataclasses_json import dataclass_json, config
from dataclasses import dataclass, field
import json
from enum import Enum

from game import GameMap


class ClientMessageType(str, Enum):
    START = "start"
    STEP = "step"
    GETALLMAPS = "getallmaps"


@dataclass_json
@dataclass
class ClientMessageBody:
    def type(self) -> ClientMessageType:
        pass


@dataclass_json
@dataclass
class GetAllMapsMessageBody(ClientMessageBody):
    def type(self) -> ClientMessageType:
        return ClientMessageType.GETALLMAPS


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
class Reward:
    StepReward: int
    MaxPossibleReward: int


@dataclass_json
@dataclass
class GameOverMessageBody:
    pass


class ServerMessageType(str, Enum):
    MAPS = "Maps"
    MOVE_REVARD = "MoveReward"
    GAMEOVER = "GameOver"
    INCORRECT_PREDICTED_STATEID = "IncorrectPredictedStateId"


@dataclass_json
@dataclass
class ServerMessage:
    MessageType: ServerMessageType
    MessageBody: list[GameMap] | Reward | GameOverMessageBody | int = field(
        metadata=config(decoder=lambda x: json.loads(x) if x != "" else "")
    )
