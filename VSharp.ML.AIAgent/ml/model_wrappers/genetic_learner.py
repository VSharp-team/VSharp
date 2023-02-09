import random
import torch
import numpy as np
import numpy.typing as npt
from math import floor
from copy import deepcopy

from ml.converter import convert_input_to_tensor
from .protocols import ModelWrapper, Mutable
from common.game import GameState

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

    def __init__(self, weights: npt.NDArray = None) -> None:
        if weights == None:
            self.weights = np.zeros((GeneticLearner.NUM_FEATURES))
        else:
            self.weights = weights

    def predict(self, input: GameState):
        data = convert_input_to_tensor(input)
        out: list[tuple[int, npt.NDArray]] = GeneticLearner._model(
            data.x_dict, data.edge_index_dict
        )
        weighted = [(index, array * self.weights) for (index, array) in out]
        return max(weighted, key=lambda x: np.sum(x[1]))[0]

    @staticmethod
    def average_n_mutables(ms: list[Mutable]) -> Mutable:
        return np.mean(ms, axis=0)

    @staticmethod
    def mutate(
        mutable: Mutable, mutation_volume: float, mutation_freq: float
    ) -> Mutable:
        """
        mutation_volume - процент компонентов вектора весов, которые будут мутированы\n
        mutation_freq - разброс изменения весов, в пределах (MAX_W, MIN_W)
        """
        new_mutable = deepcopy(mutable)
        to_mutate = floor(GeneticLearner.NUM_FEATURES / (mutation_volume / 100))

        for _ in range(to_mutate):
            index_to_mutate = random.randint(0, GeneticLearner.NUM_FEATURES - 1)
            new_mutable.weights[index_to_mutate] = variate(
                val=new_mutable.weights[index_to_mutate],
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
