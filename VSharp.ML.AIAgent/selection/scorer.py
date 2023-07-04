"""The Scorer module: contains scorers to use with selectors"""


from abc import ABCMeta, abstractmethod
from typing import Any, Callable, Iterable, TypeAlias, TypeVar

from bit_coder.bit_coder import convert_to_big_int

from .classes import GameResult, Map2Result


class Comparable(metaclass=ABCMeta):
    @abstractmethod
    def __lt__(self, other: Any) -> bool:
        ...


ComparableType = TypeVar("ComparableType", bound=Comparable)

ScorerFunction: TypeAlias = Callable[[list[Map2Result]], ComparableType]


def minkowski_distance(model_results: list[Map2Result], k) -> float:
    """Counts model dist to full coverage vector

    Uses `<minkowski_dist[k](vec(100, size=N), coverages)>`
    for model scoring
    """

    def actual_percent_if_present(res):
        if res.game_result.actual_coverage_percent is not None:
            return res.game_result.actual_coverage_percent
        return res.game_result.coverage_percent

    dist = sum(
        [abs(100 - actual_percent_if_present(res)) ** k for res in model_results]
    )

    # aim for:
    # decreasing dist
    return -dist


def decart_distance(
    model_results: list[Map2Result],
) -> float:
    return minkowski_distance(model_results, 1)


def euclidean_distance(
    model_results: list[Map2Result],
) -> float:
    return minkowski_distance(model_results, 2)


def minkowski_superscorer(model_results: list[GameResult], k) -> float:
    """Scores model

    Counts difference between full coverage vector and model results to maximize score
    """

    full_coverage_vector = sum([100**k for _ in model_results])

    # aim for maximal diff between max_value and dist
    score = full_coverage_vector - sum(
        [abs(100 - actual_percent_if_present(res)) ** k for res in model_results]
    )

    return score


def euc_dist(data: Iterable, bound, item_checker=lambda item: item):
    return sum([abs(bound - item_checker(item)) ** 2 for item in data])


def straight_scorer(model_results: list[GameResult]) -> tuple:
    # less is better
    coverage_score = -1 * euc_dist(
        data=[actual_percent_if_present(res) for res in model_results], bound=100
    )

    # less is better
    tests_score = -1 * euc_dist(
        data=[res.tests_count for res in model_results], bound=0
    )

    # more is better
    errors_score = euc_dist(
        data=[res.errors_count for res in model_results],
        bound=0,
    )

    # less is better
    steps_score = -1 * euc_dist(
        data=[res.steps_count for res in model_results], bound=0
    )

    return convert_to_big_int(coverage_score, tests_score, errors_score, steps_score)


def actual_percent_if_present(game_result):
    if game_result.actual_coverage_percent is not None:
        return game_result.actual_coverage_percent
    return game_result.coverage_percent
