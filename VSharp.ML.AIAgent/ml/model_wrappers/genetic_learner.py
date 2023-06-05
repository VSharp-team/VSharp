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

from .protocols import ModelWrapper, Mutable

MAX_W, MIN_W = 1, -1


class GeneticLearner(ModelWrapper):
    MODEL = None

    def name(self) -> str:
        return self._name

    @staticmethod
    def set_static_model():
        GeneticLearner.MODEL = load_model(Constant.IMPORTED_DICT_MODEL_PATH)

    def __init__(
        self, weights: npt.NDArray = None, successor_name_closure=lambda x: x
    ) -> None:
        if weights is None:
            # -1 to 1
            self.weights = np.array(
                [random.random() * 2 - 1 for _ in range(Constant.NUM_FEATURES)]
            )
        else:
            self.weights = weights

        self._name = gen_name()
        if Config.SHOW_SUCCESSORS:
            self._name = successor_name_closure(self._name)

    def __str__(self) -> str:
        return f"{self.name()}: {self.weights.tolist()}"

    def info(self) -> str:
        return f"{self.weights.tolist()}"

    def __hash__(self) -> int:
        return self.__str__().__hash__()

    def __eq__(self, __value: object) -> bool:
        if type(__value) != GeneticLearner:
            raise AttributeError(f"Can't compare {type(__value)} with GeneticLearner")
        return self.__hash__() == __value.__hash__()

    def predict(self, input: GameState):
        hetero_input, state_map = ServerDataloaderHeteroVector.convert_input_to_tensor(
            input
        )
        assert GeneticLearner.MODEL is not None

        # problem
        next_step_id = PredictStateVectorHetGNN.predict_state_weighted(
            GeneticLearner.MODEL, self.weights, hetero_input, state_map
        )
        del hetero_input
        # /problem
        return next_step_id

    @staticmethod
    def average(ms: list[Mutable]) -> Mutable:
        mutables_weights = [model.weights for model in ms]
        return GeneticLearner(
            weights=np.mean(mutables_weights, axis=0),
            successor_name_closure=lambda x: f"{x}(av)",
        )

    @staticmethod
    def mutate(
        mutable: Mutable, mutation_volume: float, mutation_freq: float
    ) -> Mutable:
        """
        mutation_volume - 0..1, percentage of components of the weights vector to mutate
        mutation_freq - 0..1, variation of weights, within (MAX_W, MIN_W)
        """
        assert mutation_freq < MAX_W and mutation_freq > MIN_W
        assert type(mutable) is GeneticLearner
        new_weights = mutable.weights.copy()
        to_mutate = floor(Constant.NUM_FEATURES * mutation_volume)

        for _ in range(to_mutate):
            index_to_mutate = random.randint(0, Constant.NUM_FEATURES - 1)
            new_weights[index_to_mutate] = variate(
                val=mutable.weights[index_to_mutate],
                range_percent=mutation_freq,
                borders=(MIN_W, MAX_W),
            )

        return GeneticLearner(
            weights=new_weights,
            successor_name_closure=lambda x: f"{x}(<-{mutable.name()})",
        )

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
