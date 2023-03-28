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


@dataclass
class MapResultMapping:
    map: GameMap
    mutable_result: MutableResult


GameMapsModelResults: TypeAlias = defaultdict[GameMap, list[MutableResultMapping]]
ModelResultsOnGameMaps: TypeAlias = defaultdict[Mutable, list[MapResultMapping]]
