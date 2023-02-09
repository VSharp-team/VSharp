from abc import abstractmethod
from typing import Protocol

from common.game import GameState


class Predictor(Protocol):
    @abstractmethod
    def predict(self, input: GameState):
        raise NotImplementedError
