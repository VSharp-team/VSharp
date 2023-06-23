from abc import abstractmethod
from typing import Protocol

from common.game import GameState


class Named(Protocol):
    @abstractmethod
    def name(self) -> str:
        raise NotImplementedError


class Predictor(Named, Protocol):
    @abstractmethod
    def predict(self, input: GameState):
        raise NotImplementedError
