import json

import torch.nn

from common.game import GameState
from ml.data_loader_compact import ServerDataloaderHeteroVector
from ml.model_wrappers.protocols import Predictor
from ml.predict_state_vector_hetero import PredictStateVectorHetGNN


class NNWrapper(Predictor):
    def __init__(self, model: torch.nn.Module, weights_flat: list[float]) -> None:
        self.model = model
        self.weights = weights_flat
        self._name = str(sum(weights_flat))
        self._hash = tuple(weights_flat).__hash__()

    def name(self) -> str:
        return self._name

    def predict(self, input: GameState):
        hetero_input, state_map = ServerDataloaderHeteroVector.convert_input_to_tensor(
            input
        )
        assert self.model is not None

        next_step_id = PredictStateVectorHetGNN.predict_state_single_out(
            self.model, hetero_input, state_map
        )
        del hetero_input
        return next_step_id

    def __eq__(self, __value: object) -> bool:
        if type(__value) != NNWrapper:
            raise AttributeError(f"Can't compare {type(__value)} with NNWrapper")
        return self.__hash__() == __value.__hash__()

    def __hash__(self) -> int:
        return self._name.__hash__() + self._hash


def encode(obj):
    if isinstance(obj, NNWrapper):
        return {"_name": obj._name, "_hash": obj._hash}
    return json.dumps(obj)


def decode(obj):
    if "_name" not in obj or "_hash" not in obj:
        return obj

    fake_nnwrapper = NNWrapper(None, [])
    fake_nnwrapper._name = obj["_name"]
    fake_nnwrapper._hash = obj["_hash"]
    return fake_nnwrapper
