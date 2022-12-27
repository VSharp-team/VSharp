from dataclasses_json import dataclass_json, config
from dataclasses import dataclass, field
import json


@dataclass_json
@dataclass
class ClientMessageBody:
    def type() -> str:
        pass


@dataclass_json
@dataclass
class GetAllMapsMessageBody(ClientMessageBody):
    def type(self):
        return "getallmaps"


@dataclass_json
@dataclass
class StartMessageBody(ClientMessageBody):
    MapId: int
    StepsToPlay: int

    def type(self):
        return "start"


@dataclass_json
@dataclass
class StepMessageBody(ClientMessageBody):
    StateId: int
    PredictedStateUsefulness: float

    def type(self):
        return "step"


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
