"""The Scorer module: contains scorers to use with selectors"""


from typing import Iterable

from bit_coder.bit_coder import convert_to_big_int

from common.classes import GameResult


def minkowski_superscorer(model_results: list[GameResult], k) -> float:
    """Scores model

    Counts difference between full coverage vector and model results to maximize score
    """

    full_coverage_vector = sum([100**k for _ in model_results])

    # aim for maximal diff between max_value and dist
    score = full_coverage_vector - sum(
        [abs(100 - game_res.actual_coverage_percent) ** k for game_res in model_results]
    )

    return score


def euc_dist(data: Iterable, bound):
    return sum([abs(bound - item) ** 2 for item in data])


def straight_scorer(model_results: Iterable[GameResult]) -> tuple:
    # less is better
    coverage_score = -1 * euc_dist(
        data=[game_res.actual_coverage_percent for game_res in model_results], bound=100
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
