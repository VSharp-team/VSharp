from typing import Type

from ml.model_wrappers.protocols import Mutable

from .classes import GameMapsModelResults, MutatorConfig
from .strategy import (
    AverageOfAllStrategy,
    AverageOfNTopsStrategy,
    MutateAverageOfAllStrategy,
    MutateAverageOfNTopsStrategy,
    NTopsStrategy,
)


class Mutator:
    def __init__(self, config: MutatorConfig, mutable_type: Type[Mutable]) -> None:
        self.config = config
        self.mutable_type = mutable_type

    def n_tops(
        self, model_results_on_map: GameMapsModelResults, n: int
    ) -> list[Mutable]:
        strat = NTopsStrategy(model_results_on_map, n)
        return strat.create_mutation()

    def average_of_n_tops(
        self, model_results_on_map: GameMapsModelResults, n: int
    ) -> Mutable:
        strat = AverageOfNTopsStrategy(model_results_on_map, n, self.mutable_type)
        return strat.create_mutation()

    def average_of_all(self, model_results_on_map: GameMapsModelResults) -> Mutable:
        strat = AverageOfAllStrategy(model_results_on_map, self.mutable_type)
        return strat.create_mutation()

    def mutate_average_of_n_tops(
        self, model_results_on_map: GameMapsModelResults, n: int
    ) -> Mutable:
        strat = MutateAverageOfNTopsStrategy(
            model_results_on_map, n, self.mutable_type, n, self.config
        )
        return strat.create_mutation()

    def mutate_average_of_all(
        self, model_results_on_map: GameMapsModelResults
    ) -> Mutable:
        strat = MutateAverageOfAllStrategy(
            model_results_on_map, self.mutable_type, self.config
        )
        return strat.create_mutation()

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
