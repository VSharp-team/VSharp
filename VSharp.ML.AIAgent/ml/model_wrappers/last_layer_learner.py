import random

from common.constants import BASE_NN_OUT_FEATURES_NUM, IMPORTED_DICT_MODEL_PATH
from common.game import GameState
from config import FeatureConfig
from ml.data_loader_compact import ServerDataloaderHeteroVector
from ml.model_wrappers.utils import gen_name
from ml.predict_state_vector_hetero import PredictStateVectorHetGNN
from ml.utils import load_model_with_last_layer

from .protocols import ModelWrapper

MAX_W, MIN_W = 1, -1


class LastLayerLearner(ModelWrapper):
    def name(self) -> str:
        return self._name

    def rename(self, new_name: str):
        self._name = new_name

    def info(self) -> str:
        return f"{self.weights}"

    def __init__(
        self, weights: list[float] = None, successor_name_closure=lambda x: x
    ) -> None:
        if weights is None:
            # -1 to 1
            self.weights = [
                random.uniform(-1, 1) for _ in range(BASE_NN_OUT_FEATURES_NUM)
            ]
        else:
            self.weights = weights

        self.nn = load_model_with_last_layer(IMPORTED_DICT_MODEL_PATH, self.weights)

        self._name = gen_name()
        if FeatureConfig.SHOW_SUCCESSORS:
            self._name = successor_name_closure(self._name)

    def predict(self, input: GameState):
        hetero_input, state_map = ServerDataloaderHeteroVector.convert_input_to_tensor(
            input
        )

        next_step_id = PredictStateVectorHetGNN.predict_state_weighted(
            self.nn, self.weights, hetero_input, state_map
        )
        del hetero_input
        return next_step_id

    def train_single_val(self):
        return super().train_single_val()

    def copy(self, new_name: str) -> "LastLayerLearner":
        assert new_name != self.name()
        copy = LastLayerLearner(self.weights.copy())
        copy.rename(new_name)
        return copy

    def __str__(self) -> str:
        return f"{self.name()}: {self.weights}"

    def __hash__(self) -> int:
        return self.__str__().__hash__()

    def __eq__(self, __value: object) -> bool:
        if type(__value) != LastLayerLearner:
            raise AttributeError(f"Can't compare {type(__value)} with LastLayerLearner")
        return self.__hash__() == __value.__hash__()
