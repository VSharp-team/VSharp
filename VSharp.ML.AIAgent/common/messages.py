from dataclasses_json import dataclass_json, config
from dataclasses import dataclass, field
import json
from enum import Enum

from .game import GameMap, Reward


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
    def decode(x):
        if x == "":
            return x
        d = json.loads(x)

        try:
            return Reward.from_dict(d)
        except (ValueError, AttributeError):
            pass

        if isinstance(d, list):
            try:
                return [GameMap.from_dict(i) for i in d]
            except ValueError:
                pass

        raise RuntimeError(f"Can't decode msg: {x}")

    MessageType: ServerMessageType
    MessageBody: list[GameMap] | Reward | None = field(metadata=config(decoder=decode))
