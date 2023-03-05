from abc import abstractmethod
from typing import Protocol

from common.game import GameState


class Named(Protocol):
    @abstractmethod
    def name(self) -> str:
        raise NotImplementedError


class Mutable(Named, Protocol):
    @staticmethod
    @abstractmethod
    def average_n_mutables(ms: list["Mutable"]):
        raise NotImplementedError

    @staticmethod
    @abstractmethod
    def mutate(
        mutable: "Mutable", mutation_volume: float, mutation_freq: float
    ) -> "Mutable":
        raise NotImplementedError


class Predictor(Named, Protocol):
    @abstractmethod
    def predict(self, input: GameState):
        raise NotImplementedError


class RLearner(Named, Protocol):
    @abstractmethod
    def train_single_val(self):
        raise NotImplementedError


class ModelWrapper(Mutable, Predictor, RLearner):
    pass
