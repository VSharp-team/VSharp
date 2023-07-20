from dataclasses import dataclass, field
from typing import Callable

from dataclasses_json import config, dataclass_json

from agent.unsafe_json import asdict
from config import FeatureConfig
from ml.model_wrappers.nnwrapper import NNWrapper, decode, encode
from selection.classes import Map2Result


def custom_encoder_if_disable_message_checks() -> Callable | None:
    return asdict if FeatureConfig.DISABLE_MESSAGE_CHECKS else None


@dataclass_json
@dataclass
class Agent2ResultsOnMaps:
    agent: NNWrapper = field(metadata=config(encoder=encode, decoder=decode))
    results: list[Map2Result] = field(
        metadata=config(encoder=custom_encoder_if_disable_message_checks())
    )
