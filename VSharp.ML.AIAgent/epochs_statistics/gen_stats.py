from statistics import mean, median

from epochs_statistics.common import CoverageStats, Interval
from common.classes import Map2Result


def euc_dist2full_coverage(data: list[float]) -> float:
    return sum([abs(100 - cov) ** 2 for cov in data]) ** (1 / 2)


def get_map_coverages(map2result_mappings: list[Map2Result]) -> list[float]:
    return [
        mutable2result.game_result.actual_coverage_percent
        if mutable2result.game_result.actual_coverage_percent is not None
        else mutable2result.game_result.coverage_percent
        for mutable2result in map2result_mappings
    ]


def compute_average_coverage(data: list[Map2Result]) -> float:
    return mean(get_map_coverages(data))


def compute_median_coverage(data: list[Map2Result]):
    return median(get_map_coverages(data))


def compute_euc_dist_to_full_coverage(results: list[Map2Result]) -> CoverageStats:
    mutable_map_coverages = get_map_coverages(results)
    euc_dist = euc_dist2full_coverage(mutable_map_coverages)
    av = compute_average_coverage(results)
    med = compute_median_coverage(results)
    interval = Interval(min(mutable_map_coverages), max(mutable_map_coverages))

    return CoverageStats(euc_dist, av, med, interval)
