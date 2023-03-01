from collections import defaultdict
from dataclasses import dataclass
from typing import TypeAlias, Type
from common.game import GameMap, MoveReward
from ml.model_wrappers.protocols import Mutable

StepsCount: TypeAlias = int
ModelResult: TypeAlias = tuple[Mutable, tuple[MoveReward, StepsCount]]
IterationResults: TypeAlias = defaultdict[GameMap, list[ModelResult]]


@dataclass
class MutationProportions:
    n_tops: int
    averaged_n_tops: int
    n_averaged_all: int
    random_n_tops_averaged_mutations: int
    random_all_averaged_mutations: int


@dataclass
class MutatorConfig:
    proportions: MutationProportions
    mutation_volume: float  # 0-100 %
    mutation_freq: float  # 0-100 %

    def __post_init__(self):
        in_percents = lambda x: x >= 0 and x <= 100
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

    def n_tops(self, iteration_results: IterationResults, n: int) -> list[Mutable]:
        # топ по каждой карте
        n_tops: list[Mutable] = []
        for game_map in iteration_results.keys():
            n_tops.extend(
                map(
                    lambda t: t[0],  # take TfModelWrapper
                    sorted(
                        iteration_results[game_map],
                        key=lambda t: (
                            t[1][0].ForCoverage,
                            t[1][0].ForVisitedInstructions,
                            t[1][1],
                        ),  # sort by <MoveRewardReward, StepsCount>
                    ),
                )
            )

        return n_tops[:n]

    def averaged_n_tops(self, iteration_results: IterationResults, n: int) -> Mutable:
        # среднее по топам
        return self.mutable_type.average_n_mutables(self.n_tops(iteration_results, n))

    def averaged_all(self, iteration_results) -> Mutable:
        # среднее по всем отобранным нейронкам

        all: list[Mutable] = []
        for game_map in iteration_results.keys():
            all.extend(
                map(lambda t: t[0], iteration_results[game_map])
            )  # take TfModelWrapper

        return self.mutable_type.average_n_mutables(all)

    def random_n_tops_averaged_mutations(
        self, iteration_results: IterationResults, n: int
    ) -> list[Mutable]:
        # случайные мутации среднего по топам
        return self.mutable_type.mutate(
            mutable=self.averaged_n_tops(iteration_results, n),
            mutation_volume=self.config.mutation_volume,
            mutation_freq=self.config.mutation_freq,
        )

    def random_all_averaged_mutations(
        self, iteration_results: IterationResults
    ) -> Mutable:
        # случайные мутации среднего по всем отобранным нейронкам
        return self.mutable_type.mutate(
            mutable=self.averaged_all(iteration_results),
            mutation_volume=self.config.mutation_volume,
            mutation_freq=self.config.mutation_freq,
        )

    def new_generation(
        self,
        iteration_results: IterationResults,
    ) -> list[Mutable]:
        new_gen = (
            self.n_tops(iteration_results, self.config.proportions.n_tops)
            + [
                self.averaged_n_tops(iteration_results, self.config.proportions.n_tops)
                for _ in range(self.config.proportions.averaged_n_tops)
            ]
            + [
                self.averaged_all(iteration_results)
                for _ in range(self.config.proportions.n_averaged_all)
            ]
            + [
                self.random_n_tops_averaged_mutations(
                    iteration_results, self.config.proportions.n_tops
                )
                for _ in range(self.config.proportions.random_n_tops_averaged_mutations)
            ]
            + [
                self.random_all_averaged_mutations(iteration_results)
                for _ in range(self.config.proportions.random_all_averaged_mutations)
            ]
        )
        assert len(new_gen) == 10

        return new_gen
