from collections import defaultdict
from dataclasses import dataclass
from typing import TypeAlias

from common.game import GameMap, MoveReward
from ml.model_wrappers.protocols import Mutable


@dataclass
class MutableResult:
    move_reward: MoveReward
    steps_count: int
    coverage_percent: float

    def printable(self, verbose) -> str:
        coverage_percent_format = (
            f"coverage %: {self.coverage_percent:.2f},"
            if verbose
            else f"%c={self.coverage_percent:.2f}"
        )
        steps_format = (
            f"steps: {self.steps_count}," if verbose else f"#s={self.steps_count}"
        )
        return f"{coverage_percent_format} {steps_format} {self.move_reward.printable(verbose)}"


@dataclass
class MutableResultMapping:
    mutable: Mutable
    mutable_result: MutableResult


GameMapsModelResults: TypeAlias = defaultdict[GameMap, list[MutableResultMapping]]


@dataclass
class MutationProportions:
    n_tops: int
    average_of_n_tops: int
    average_of_all: int
    mutate_average_of_n_tops: int
    mutate_average_of_all: int


@dataclass
class MutatorConfig:
    proportions: MutationProportions
    mutation_volume: float  # 0-1
    mutation_freq: float  # 0-1

    def __post_init__(self):
        in_percents = lambda x: x >= 0 and x <= 1
        if not in_percents(self.mutation_volume):
            raise ValueError(
                f"mutation volume is not in percents: {self.mutation_volume=}"
            )
        if not in_percents(self.mutation_freq):
            raise ValueError(f"mutation freq is not in percents: {self.mutation_freq=}")
