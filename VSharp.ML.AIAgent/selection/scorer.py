"""The Scorer module: contains scorers to use with selectors"""


from abc import ABCMeta, abstractmethod
from typing import Any, Callable, TypeAlias, TypeVar

from .classes import Map2Result


class Comparable(metaclass=ABCMeta):
    @abstractmethod
    def __lt__(self, other: Any) -> bool:
        ...


ComparableType = TypeVar("ComparableType", bound=Comparable)

ScorerFunction: TypeAlias = Callable[[list[Map2Result]], ComparableType]


def minkowski_scorer(model_results: list[Map2Result], k) -> float:
    """Scores Mutable

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


def decart_scorer(
    model_results: list[Map2Result],
) -> float:
    return minkowski_scorer(model_results, 1)


def euclidean_scorer(
    model_results: list[Map2Result],
) -> float:
    return minkowski_scorer(model_results, 2)
