import json

import torch.nn
from predict import predict_state_with_dict

from common.game import GameState
from ml.data_loader_compact import ServerDataloaderHeteroVector
from ml.model_wrappers.protocols import Predictor


class NNWrapper(Predictor):
    def __init__(self, model: torch.nn.Module, weights_flat: list[float]) -> None:
        self._name = str(sum(weights_flat))
        self._model = model
        self.weights_hash = tuple(weights_flat).__hash__()

    def name(self) -> str:
        return self._name

    def model(self) -> list:
        return self._model

    def predict(self, input: GameState, map_name):
        hetero_input, state_map = ServerDataloaderHeteroVector.convert_input_to_tensor(
            input
        )
        assert self._model is not None

        next_step_id = predict_state_with_dict(self._model, hetero_input, state_map)
        del hetero_input
        return next_step_id

    def __eq__(self, __value: object) -> bool:
        if type(__value) != NNWrapper:
            raise AttributeError(f"Can't compare {type(__value)} with NNWrapper")
        return self.__hash__() == __value.__hash__()

    def __hash__(self) -> int:
        return self.name().__hash__() + self.weights_hash


def encode(obj):
    if isinstance(obj, NNWrapper):
        return {"name": obj._name, "weights_hash": obj.weights_hash}
    return json.dumps(obj)


def decode(obj):
    if "name" not in obj or "weights_hash" not in obj:
        return obj

    fake_nnwrapper = NNWrapper(None, [])
    fake_nnwrapper._name = obj["name"]
    fake_nnwrapper.weights_hash = obj["weights_hash"]
    return fake_nnwrapper
