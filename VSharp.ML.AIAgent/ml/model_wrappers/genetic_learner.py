import random
import string
from copy import deepcopy
from math import floor

import numpy as np
import numpy.typing as npt
import torch
from common.game import GameState
from ml.data_loader_compact import ServerDataloaderHeteroVector
from ml.predict_state_vector_hetero import PredictStateVectorHetGNN

from .protocols import ModelWrapper, Mutable

MAX_W, MIN_W = 1, -1


class GeneticLearner(ModelWrapper):
    _model: torch.nn.Module

    @staticmethod
    def get_model():
        if GeneticLearner._model is None:
            raise AttributeError("GenericLearner model is not set!")
        return GeneticLearner._model

    @staticmethod
    def set_model(torch_model: torch.nn.Module, num_features: int):
        GeneticLearner._model = torch_model
        GeneticLearner.NUM_FEATURES = num_features

    def name(self) -> str:
        return self._name

    def __init__(self, weights: npt.NDArray = None) -> None:
        if weights is None:
            # -1 to 1
            self.weights = np.random.rand((GeneticLearner.NUM_FEATURES)) * 2 - 1
        else:
            self.weights = weights

        self._name = "".join(
            random.choices(string.ascii_uppercase + string.digits, k=5)
        )

    def __str__(self) -> str:
        return f"{self.name()}: {[round(component, 2) for component in self.weights]}"

    def predict(self, input: GameState):
        hetero_input, state_map = ServerDataloaderHeteroVector.convert_input_to_tensor(
            input
        )
        next_step_id, _ = PredictStateVectorHetGNN.predict_state_weighted(
            GeneticLearner._model, self.weights, hetero_input, state_map
        )
        return next_step_id

    @staticmethod
    def average(ms: list[Mutable]) -> Mutable:
        mutables_weights = [model.weights for model in ms]
        return GeneticLearner(weights=np.mean(mutables_weights, axis=0))

    @staticmethod
    def mutate(
        mutable: Mutable, mutation_volume: float, mutation_freq: float
    ) -> Mutable:
        """
        mutation_volume - 0..1, percentage of components of the weights vector to mutate
        mutation_freq - 0..1, variation of weights, within (MAX_W, MIN_W)
        """
        assert mutation_freq < MAX_W and mutation_freq > MIN_W
        new_mutable = deepcopy(mutable)
        to_mutate = floor(GeneticLearner.NUM_FEATURES * mutation_volume)

        for _ in range(to_mutate):
            index_to_mutate = random.randint(0, GeneticLearner.NUM_FEATURES - 1)
            new_mutable.weights[index_to_mutate] = variate(
                val=mutable.weights[index_to_mutate],
                range_percent=mutation_freq,
                borders=(MIN_W, MAX_W),
            )

        return new_mutable

    def train_single_val(self):
        return super().train_single_val()


def variate(val: float, range_percent: float, borders: tuple[float, float]):
    sign = 1 if random.random() - 0.5 > 0 else -1
    border_range = borders[1] - borders[0]
    variated = val + sign * range_percent * border_range
    if variated > borders[1]:
        return borders[1]
    if variated < borders[0]:
        return borders[0]
    return variated
