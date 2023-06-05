from typing import Callable, Sequence

from displayer.common import CoverageStats, Interval, Name2Stats
from selection.classes import Map2Result, ModelResultsOnGameMaps


def euc_av(data: list[float]) -> float:
    return sum([abs(100 - cov) ** 2 for cov in data]) ** (1 / 2)


def av(data: list[float]) -> float:
    return sum(data) / len(data)


def get_av_coverage(
    results: list[Map2Result], av_fun: Callable[[list[float]], float] = euc_av
) -> CoverageStats:
    def get_map_coverages(map2result_mappings: list[Map2Result]) -> list[float]:
        return [
            mutable2result.game_result.actual_coverage_percent
            if mutable2result.game_result.actual_coverage_percent is not None
            else mutable2result.game_result.coverage_percent
            for mutable2result in map2result_mappings
        ]

    mutable_map_coverages = get_map_coverages(results)
    average = av_fun(mutable_map_coverages)
    interval = Interval(min(mutable_map_coverages), max(mutable_map_coverages))

    return CoverageStats(average, interval)
