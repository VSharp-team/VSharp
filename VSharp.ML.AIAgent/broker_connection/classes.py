from dataclasses import dataclass, field

from dataclasses_json import config, dataclass_json

from ml.model_wrappers.nnwrapper import NNWrapper, encode, decode
from selection.classes import Map2Result


@dataclass_json
@dataclass
class Agent2ResultsOnMaps:
    agent: NNWrapper = field(metadata=config(encoder=encode, decoder=decode))
    results: list[Map2Result]
