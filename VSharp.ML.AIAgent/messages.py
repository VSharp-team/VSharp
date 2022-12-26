from dataclasses import asdict, dataclass, field
import json


class MessageBody:
    def type() -> str:
        pass


@dataclass
class GetAllMapsMessageBody(MessageBody):
    def type(self):
        return "getallmaps"


@dataclass
class StartMessageBody(MessageBody):
    MapId: int
    StepsToPlay: int

    def type(self):
        return "start"


@dataclass
class StepMessageBody(MessageBody):
    StateId: int
    PredictedStateUsefulness: float

    def type(self):
        return "step"


def factory(data):
    # for now only one level of nested structures supported
    dict = {}

    for (key, value) in data:
        if type(value) in (str, int, float):
            dict[key] = value
        else:
            dict[key] = json.dumps(value)
    return dict


@dataclass
class Message:
    MessageType: str = field(init=False)
    MessageBody: MessageBody

    def __post_init__(self):
        self.MessageType = self.MessageBody.type()

    def dumps(self) -> str:
        return json.dumps(asdict(self, dict_factory=factory))


def main():
    msg = Message(StartMessageBody(MapId=0, StepsToPlay=10))
    print(msg.dumps())


if __name__ == "__main__":
    main()
