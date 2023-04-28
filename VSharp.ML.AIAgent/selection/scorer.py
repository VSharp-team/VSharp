"""The Scorer module: contains scorers to use with selectors"""


from abc import ABCMeta, abstractmethod
from typing import Any, Callable, TypeAlias, TypeVar

from .classes import MapResultMapping


class Comparable(metaclass=ABCMeta):
    @abstractmethod
    def __lt__(self, other: Any) -> bool:
        ...


ComparableType = TypeVar("ComparableType", bound=Comparable)

ScorerFunction: TypeAlias = Callable[[list[MapResultMapping]], ComparableType]


def minkowski_scorer(
    model_results: list[MapResultMapping], k
) -> tuple[float, float, float]:
    """Scores Mutable

    Uses `<minkowski_dist(k), sum(visited instructions rewards), steps>` tuple
    for model scoring
    """
    dist = sum(
        [abs(100 - res.mutable_result.coverage_percent) ** k for res in model_results]
    )
    visited_instructions_sum = sum(
        [res.mutable_result.move_reward.ForVisitedInstructions for res in model_results]
    )
    steps_sum = sum([res.mutable_result.steps_count for res in model_results])

    # aim for:
    # decreasing dist
    # increasing visited_instructions_sum
    # decreasing steps_sum
    return (-dist, visited_instructions_sum, -steps_sum)


def decart_scorer(
    model_results: list[MapResultMapping],
) -> tuple[float, float, float]:
    return minkowski_scorer(model_results, 1)


def euclidean_scorer(
    model_results: list[MapResultMapping],
) -> tuple[float, float, float]:
    return minkowski_scorer(model_results, 2)
