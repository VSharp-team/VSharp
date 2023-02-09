from abc import abstractmethod
from typing import Protocol


class Mutable(Protocol):
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
