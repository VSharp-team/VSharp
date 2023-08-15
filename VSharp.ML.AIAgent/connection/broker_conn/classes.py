from dataclasses import dataclass, field
from typing import Callable, TypeAlias

from dataclasses_json import config, dataclass_json

from common.classes import Map2Result
from config import FeatureConfig
from connection.game_server_conn.unsafe_json import asdict
from ml.model_wrappers.nnwrapper import NNWrapper, decode, encode

WSUrl: TypeAlias = str
Undefined: TypeAlias = None


@dataclass_json
@dataclass(slots=True, frozen=True)
class ServerInstanceInfo:
    port: int
    ws_url: WSUrl
    pid: int | Undefined


def custom_encoder_if_disable_message_checks() -> Callable | None:
    return asdict if FeatureConfig.DISABLE_MESSAGE_CHECKS else None


@dataclass_json
@dataclass
class Agent2ResultsOnMaps:
    agent: NNWrapper = field(metadata=config(encoder=encode, decoder=decode))
    results: list[Map2Result] = field(
        metadata=config(encoder=custom_encoder_if_disable_message_checks())
    )
