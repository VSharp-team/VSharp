import copy
import logging

import torch
from common.game import GameState
from ml.common_model.utils import back_prop
from ml.data_loader_compact import ServerDataloaderHeteroVector
from ml.model_wrappers.protocols import Predictor

from ml.predict import predict_state_with_dict


class CommonModelWrapper(Predictor):
    def __init__(
        self,
        model: torch.nn.Module,
    ) -> None:
        self._model = model
        self.model_copy = model
        self._name = "1"

    def name(self):
        return "Common model"

    def make_copy(self, model_name: str):
        self.model_copy = copy.deepcopy(self._model)
        self._name = model_name

    def model(self):
        return self._model

    def predict(self, input: GameState, map_name):
        hetero_input, state_map = ServerDataloaderHeteroVector.convert_input_to_tensor(
            input
        )
        assert self._model is not None

        next_step_id, nn_output = predict_state_with_dict(
            self._model, hetero_input, state_map
        )

        del hetero_input
        return next_step_id, nn_output


class BestModelsWrapper(Predictor):
    def __init__(
        self,
        best_models: dict,
    ) -> None:
        self.best_models = best_models

    def name(self):
        return "Best model"

    def model(self):
        return None

    def predict(self, input: GameState, map_name):
        hetero_input, state_map = ServerDataloaderHeteroVector.convert_input_to_tensor(
            input
        )
        next_step_id, nn_output = predict_state_with_dict(
            self.best_models[map_name][0], hetero_input, state_map
        )

        del hetero_input
        return next_step_id, nn_output
