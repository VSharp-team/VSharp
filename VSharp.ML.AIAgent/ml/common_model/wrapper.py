import torch
import copy
import logging

from common.game import GameState
from ml.data_loader_compact import ServerDataloaderHeteroVector
from ml.model_wrappers.protocols import Predictor
from ml.predict_state_vector_hetero import PredictStateVectorHetGNN
from ml.common_model.utils import back_prop


class CommonModelWrapper(Predictor):
    def __init__(
        self, model: torch.nn.Module, best_models: dict, optimizer, criterion
    ) -> None:
        self.best_models = best_models
        self._model = model
        # self.name = sum(torch.cat([p.view(-1) for p in self.model.parameters()], dim=0))
        self.optimizer = optimizer
        self.criterion = criterion

    def name(self):
        return "Common model"

    def update(self, map_name, map_result):
        map_result = map_result.actual_coverage_percent
        if self.best_models[map_name][1] <= map_result:
            logging.info(
                f"The model with result = {self.best_models[map_name][1]} was replaced with the model with "
                f"result = {map_result} on the map {map_name}"
            )
            self.best_models[map_name] = (copy.deepcopy(self._model), map_result)

    def model(self):
        return self._model

    def predict(self, input: GameState, map_name):
        hetero_input, state_map = ServerDataloaderHeteroVector.convert_input_to_tensor(
            input
        )
        assert self._model is not None

        next_step_id = PredictStateVectorHetGNN.predict_state_with_dict(
            self._model, hetero_input, state_map
        )

        back_prop(
            self.best_models[map_name][0],
            self._model,
            hetero_input,
            self.optimizer,
            self.criterion,
        )

        del hetero_input
        return next_step_id
