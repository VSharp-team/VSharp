import random
from math import floor

import numpy as np
import numpy.typing as npt
import torch
from torch_geometric.loader import DataLoader

from common.constants import Constant
from common.game import GameState
from config import Config
from ml.data_loader_compact import ServerDataloaderHeteroVector
from ml.model_wrappers.utils import gen_name
from ml.predict_state_vector_hetero import PredictStateVectorHetGNN
from ml.utils import load_model

from .protocols import Predictor

MAX_W, MIN_W = 1, -1


class GeneticLearner(Predictor):
    MODEL = None

    def name(self) -> str:
        return self._name

    def rename(self, new_name: str):
        self._name = new_name

    @staticmethod
    def set_static_model():
        GeneticLearner.MODEL = load_model(Constant.IMPORTED_DICT_MODEL_PATH)

    def __init__(
        self, weights: list[float] = None, successor_name_closure=lambda x: x
    ) -> None:
        if weights is None:
            # -1 to 1
            self.weights = np.array(
                [random.random() * 2 - 1 for _ in range(Constant.NUM_FEATURES)]
            )
        else:
            self.weights = np.array(weights)

        self._name = gen_name()
        if Config.SHOW_SUCCESSORS:
            self._name = successor_name_closure(self._name)

    def predict(self, input: GameState):
        hetero_input, state_map = ServerDataloaderHeteroVector.convert_input_to_tensor(
            input
        )
        assert GeneticLearner.MODEL is not None

        next_step_id = PredictStateVectorHetGNN.predict_state_weighted(
            GeneticLearner.MODEL, self.weights, hetero_input, state_map
        )
        del hetero_input
        return next_step_id

    def train_single_val(self):
        return super().train_single_val()

    def copy(self, new_name: str) -> "GeneticLearner":
        assert new_name != self.name()
        copy = GeneticLearner(self.weights.copy())
        copy.rename(new_name)
        return copy

    def __str__(self) -> str:
        return f"{self.name()}: {self.weights.tolist()}"

    def __hash__(self) -> int:
        return self.__str__().__hash__()

    def __eq__(self, __value: object) -> bool:
        if type(__value) != GeneticLearner:
            raise AttributeError(f"Can't compare {type(__value)} with GeneticLearner")
        return self.__hash__() == __value.__hash__()
