from abc import ABC, abstractmethod

import torch

from common.game import GameState


class Named(ABC):
    @abstractmethod
    def name(self) -> str:
        raise NotImplementedError


class Predictor(Named, ABC):
    @abstractmethod
    def predict(self, input: GameState, map_name):
        raise NotImplementedError

    @abstractmethod
    def model(self) -> torch.nn.Module:
        raise NotImplementedError
