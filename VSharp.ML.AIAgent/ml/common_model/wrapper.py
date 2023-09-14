import copy
import logging

import torch
from predict import predict_state_single_out, predict_state_with_dict

from common.game import GameState
from ml.common_model.utils import back_prop
from ml.data_loader_compact import ServerDataloaderHeteroVector
from ml.model_wrappers.protocols import Predictor

from predict import predict_state_with_dict


class CommonModelWrapper(Predictor):
    def __init__(
        self,
        model: torch.nn.Module,
    ) -> None:
        self._model = model
        self.model_copy = model
        self._name = "1"
        # self.name = sum(torch.cat([p.view(-1) for p in self.model.parameters()], dim=0))

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
        model: torch.nn.Module,
        best_models: dict,
    ) -> None:
        self.best_models = best_models
        self._model = model

    def name(self):
        return "Common model"

    def update(self, map_name, map_result, dataset):
        map_result = (
            map_result.actual_coverage_percent,
            -map_result.tests_count,
            map_result.errors_count,
            -map_result.steps_count,
        )
        dataset.save_map_data(map_name)

    def model(self):
        return self._model

    def predict(self, input: GameState, map_name):
        hetero_input, state_map = ServerDataloaderHeteroVector.convert_input_to_tensor(
            input
        )
        assert self._model is not None

        next_step_id, nn_output = predict_state_with_dict(
            self.best_models[map_name][0], hetero_input, state_map
        )

        del hetero_input
        return next_step_id, nn_output
