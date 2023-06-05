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


def minkowski_scorer(model_results: list[Map2Result], k) -> tuple[float, float, float]:
    """Scores Mutable

    Uses `<minkowski_dist(k), sum(visited instructions rewards), steps>` tuple
    for model scoring
    """

    def actual_percent_if_present(res):
        if res.mutable_result.actual_coverage_percent is not None:
            return res.mutable_result.actual_coverage_percent
        return res.mutable_result.coverage_percent

    dist = sum(
        [abs(100 - actual_percent_if_present(res)) ** k for res in model_results]
    )
    visited_instructions_sum = sum(
        [res.game_result.move_reward.ForVisitedInstructions for res in model_results]
    )
    steps_sum = sum([res.game_result.steps_count for res in model_results])

    # aim for:
    # decreasing dist
    # increasing visited_instructions_sum
    # decreasing steps_sum
    return (-dist, visited_instructions_sum, -steps_sum)


def decart_scorer(
    model_results: list[Map2Result],
) -> tuple[float, float, float]:
    return minkowski_scorer(model_results, 1)


def euclidean_scorer(
    model_results: list[Map2Result],
) -> tuple[float, float, float]:
    return minkowski_scorer(model_results, 2)
