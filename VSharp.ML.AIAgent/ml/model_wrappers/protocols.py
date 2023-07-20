from abc import ABC, abstractmethod

from common.game import GameState


class Named(ABC):
    @abstractmethod
    def name(self) -> str:
        raise NotImplementedError


class Predictor(Named, ABC):
    @abstractmethod
    def predict(self, input: GameState):
        raise NotImplementedError


class WeightedPredictor(Predictor, ABC):
    @abstractmethod
    def weights(self) -> list:
        raise NotImplementedError
