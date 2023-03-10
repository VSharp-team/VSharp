from collections import defaultdict
from dataclasses import dataclass
from typing import Type, TypeAlias

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


class Mutator:
    def __init__(self, config: MutatorConfig, mutable_type: Type[Mutable]) -> None:
        self.config = config
        self.mutable_type = mutable_type

    def n_tops(
        self, model_results_on_map: GameMapsModelResults, n: int
    ) -> list[Mutable]:
        def sort_by_reward_asc_steps_desc(
            mutable_mapping: MutableResultMapping,
        ):
            # sort by <MoveRewardReward, -StepsCount (less is better)>
            result = mutable_mapping.mutable_result
            return (
                result.move_reward.ForCoverage,
                result.move_reward.ForVisitedInstructions,
                -result.steps_count,
            )

        all_model_results: list[list[Mutable]] = []

        for model_result_mapping_list in model_results_on_map.values():
            all_model_results.append(
                sorted(model_result_mapping_list, key=sort_by_reward_asc_steps_desc)
            )

        result_array = []

        for i in range(n):
            result_array.append(all_model_results[i % len(all_model_results)].pop())

        return [mapping.mutable for mapping in result_array]

    def average_of_n_tops(
        self, model_results_on_map: GameMapsModelResults, n: int
    ) -> Mutable:
        return self.mutable_type.average(self.n_tops(model_results_on_map, n))

    def average_of_all(self, model_results_on_map: GameMapsModelResults) -> Mutable:
        all: list[Mutable] = []
        for model_results_mapping_list_for_map in model_results_on_map.values():
            all.extend(map(lambda t: t.mutable, model_results_mapping_list_for_map))

        return self.mutable_type.average(all)

    def mutate_average_of_n_tops(
        self, model_results_on_map: GameMapsModelResults, n: int
    ) -> Mutable:
        return self.mutable_type.mutate(
            mutable=self.average_of_n_tops(model_results_on_map, n),
            mutation_volume=self.config.mutation_volume,
            mutation_freq=self.config.mutation_freq,
        )

    def mutate_average_of_all(
        self, model_results_on_map: GameMapsModelResults
    ) -> Mutable:
        return self.mutable_type.mutate(
            mutable=self.average_of_all(model_results_on_map),
            mutation_volume=self.config.mutation_volume,
            mutation_freq=self.config.mutation_freq,
        )

    def new_generation(
        self,
        model_results_on_map: GameMapsModelResults,
    ) -> list[Mutable]:
        new_gen = (
            self.n_tops(model_results_on_map, self.config.proportions.n_tops)
            + [
                self.average_of_n_tops(
                    model_results_on_map, self.config.proportions.n_tops
                )
                for _ in range(self.config.proportions.average_of_n_tops)
            ]
            + [
                self.average_of_all(model_results_on_map)
                for _ in range(self.config.proportions.average_of_all)
            ]
            + [
                self.mutate_average_of_n_tops(
                    model_results_on_map, self.config.proportions.n_tops
                )
                for _ in range(self.config.proportions.mutate_average_of_n_tops)
            ]
            + [
                self.mutate_average_of_all(model_results_on_map)
                for _ in range(self.config.proportions.mutate_average_of_all)
            ]
        )

        return new_gen
