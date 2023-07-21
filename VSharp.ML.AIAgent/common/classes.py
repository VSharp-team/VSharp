from collections import defaultdict
from dataclasses import dataclass
from typing import Optional, TypeAlias

from dataclasses_json import dataclass_json

from common.game import GameMap
from ml.model_wrappers.protocols import Named


@dataclass_json
@dataclass
class GameResult:
    steps_count: int
    tests_count: int
    errors_count: int
    actual_coverage_percent: Optional[float] = None

    def printable(self, verbose) -> str:
        steps_format = (
            f"steps: {self.steps_count}," if verbose else f"#s={self.steps_count}"
        )
        tests_count_format = (
            f"test count: {self.tests_count}" if verbose else f"#t={self.tests_count}"
        )
        errors_count_format = (
            f"error count: {self.errors_count}"
            if verbose
            else f"#e={self.errors_count}"
        )
        actual_coverage_percent_format = (
            f"actual %: {self.actual_coverage_percent:.2f},"
            if verbose
            else f"%ac={self.actual_coverage_percent:.2f}"
        )
        return f"{actual_coverage_percent_format} {steps_format} {tests_count_format} {errors_count_format}"


@dataclass
class Agent2Result:
    agent: Named
    game_result: GameResult


@dataclass_json
@dataclass
class Map2Result:
    map: GameMap
    game_result: GameResult


GameMapsModelResults: TypeAlias = defaultdict[GameMap, list[Agent2Result]]
AgentResultsOnGameMaps: TypeAlias = defaultdict[Named, list[Map2Result]]
