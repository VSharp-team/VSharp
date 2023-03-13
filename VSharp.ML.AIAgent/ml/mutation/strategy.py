from dataclasses import dataclass
from typing import Protocol, Type

from ml.model_wrappers.protocols import Mutable

from .classes import GameMapsModelResults, MutableResultMapping, MutatorConfig


class OneMutationStrategy(Protocol):
    def create_mutation(self) -> Mutable:
        raise NotImplementedError


class NMutationStrategy(Protocol):
    def create_mutation(self) -> list[Mutable]:
        raise NotImplementedError


@dataclass
class NTopsStrategy(NMutationStrategy):
    model_results_on_map: GameMapsModelResults
    n: int

    def create_mutation(self) -> list[Mutable]:
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

        for model_result_mapping_list in self.model_results_on_map.values():
            all_model_results.append(
                sorted(model_result_mapping_list, key=sort_by_reward_asc_steps_desc)
            )

        result_array = []

        for i in range(self.n):
            result_array.append(all_model_results[i % len(all_model_results)].pop())

        return [mapping.mutable for mapping in result_array]


@dataclass
class AverageOfNTopsStrategy(OneMutationStrategy):
    model_results_on_map: GameMapsModelResults
    n: int
    mutable_type: Type[Mutable]

    def create_mutation(self) -> Mutable:
        n_tops_strategy = NTopsStrategy(self.model_results_on_map, self.n)
        n_tops = n_tops_strategy.create_mutation()
        return self.mutable_type.average(n_tops)


@dataclass
class AverageOfAllStrategy(OneMutationStrategy):
    model_results_on_map: GameMapsModelResults
    mutable_type: Type[Mutable]

    def create_mutation(self) -> Mutable:
        all: list[Mutable] = []
        for model_results_mapping_list_for_map in self.model_results_on_map.values():
            all.extend(map(lambda t: t.mutable, model_results_mapping_list_for_map))

        return self.mutable_type.average(all)


@dataclass
class MutateAverageOfNTopsStrategy(OneMutationStrategy):
    model_results_on_map: GameMapsModelResults
    n: int
    mutable_type: Type[Mutable]
    config: MutatorConfig

    def create_mutation(self) -> Mutable:
        av_of_n_tops_strategy = AverageOfNTopsStrategy(
            self.model_results_on_map, self.n, self.mutable_type
        )

        av_of_n_tops = av_of_n_tops_strategy.create_mutation()

        return self.mutable_type.mutate(
            mutable=av_of_n_tops,
            mutation_volume=self.config.mutation_volume,
            mutation_freq=self.config.mutation_freq,
        )


@dataclass
class MutateAverageOfAllStrategy(OneMutationStrategy):
    model_results_on_map: GameMapsModelResults
    mutable_type: Type[Mutable]
    config: MutatorConfig

    def create_mutation(self) -> Mutable:
        strat = AverageOfAllStrategy(self.model_results_on_map, self.mutable_type)
        return self.mutable_type.mutate(
            mutable=strat.create_mutation(),
            mutation_volume=self.config.mutation_volume,
            mutation_freq=self.config.mutation_freq,
        )


# ???
class MutationStrategyFactory:
    def __init__(
        self,
        model_results_on_map: GameMapsModelResults,
        n: int,
        mutable_type: Type[Mutable],
        config: MutatorConfig,
    ) -> None:
        self.model_results_on_map = model_results_on_map
        self.n = n
        self.mutable_type = mutable_type
        self.config = config

    def create_n_tops_strategy(self) -> NTopsStrategy:
        return NTopsStrategy(self.model_results_on_map, self.n)

    def create_average_of_n_tops_strategy(self) -> AverageOfNTopsStrategy:
        return AverageOfNTopsStrategy(
            self.model_results_on_map, self.n, self.mutable_type
        )

    def create_average_of_all_strategy(self) -> AverageOfAllStrategy:
        return AverageOfAllStrategy(self.model_results_on_map, self.mutable_type)

    def create_mutate_av_of_n_tops_strategy(self) -> MutateAverageOfNTopsStrategy:
        return MutateAverageOfNTopsStrategy(
            self.model_results_on_map, self.n, self.mutable_type, self.config
        )
