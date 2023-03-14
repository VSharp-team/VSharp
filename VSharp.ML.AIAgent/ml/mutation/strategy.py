from collections import defaultdict
from dataclasses import dataclass
from typing import Protocol, Type

from ml.model_wrappers.protocols import Mutable

from .classes import (
    GameMapsModelResults,
    MapResultMapping,
    ModelResultsOnGameMaps,
    MutableResultMapping,
    MutatorConfig,
)


class MutationStrategy(Protocol):
    def create_mutation(
        self, model_results_on_map: GameMapsModelResults
    ) -> Mutable | list[Mutable]:
        pass


@dataclass
class NTopsStrategy(MutationStrategy):
    n: int

    def create_mutation(
        self, model_results_on_map: GameMapsModelResults
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

        for i in range(self.n):
            result_array.append(all_model_results[i % len(all_model_results)].pop())

        return [mapping.mutable for mapping in result_array]


@dataclass
class AverageOfNTopsStrategy(MutationStrategy):
    n: int
    mutable_type: Type[Mutable]

    def create_mutation(self, model_results_on_map: GameMapsModelResults) -> Mutable:
        n_tops_strategy = NTopsStrategy(self.n)
        n_tops = n_tops_strategy.create_mutation(model_results_on_map)
        return self.mutable_type.average(n_tops)


@dataclass
class AverageOfAllStrategy(MutationStrategy):
    mutable_type: Type[Mutable]

    def create_mutation(self, model_results_on_map: GameMapsModelResults) -> Mutable:
        all: list[Mutable] = []
        for model_results_mapping_list_for_map in model_results_on_map.values():
            all.extend(map(lambda t: t.mutable, model_results_mapping_list_for_map))

        return self.mutable_type.average(all)


@dataclass
class MutateAverageOfNTopsStrategy(MutationStrategy):
    n: int
    mutable_type: Type[Mutable]
    config: MutatorConfig

    def create_mutation(self, model_results_on_map: GameMapsModelResults) -> Mutable:
        av_of_n_tops_strategy = AverageOfNTopsStrategy(self.n, self.mutable_type)

        av_of_n_tops = av_of_n_tops_strategy.create_mutation(model_results_on_map)

        return self.mutable_type.mutate(
            mutable=av_of_n_tops,
            mutation_volume=self.config.mutation_volume,
            mutation_freq=self.config.mutation_freq,
        )


@dataclass
class MutateAverageOfAllStrategy(MutationStrategy):
    mutable_type: Type[Mutable]
    config: MutatorConfig

    def create_mutation(self, model_results_on_map: GameMapsModelResults) -> Mutable:
        strat = AverageOfAllStrategy(self.mutable_type)
        return self.mutable_type.mutate(
            mutable=strat.create_mutation(model_results_on_map),
            mutation_volume=self.config.mutation_volume,
            mutation_freq=self.config.mutation_freq,
        )


def invert_mapping(
    model_results_on_map: GameMapsModelResults,
) -> ModelResultsOnGameMaps:
    inverse_mapping: ModelResultsOnGameMaps = defaultdict(list)

    for map, list_of_mutable_result_mappings in model_results_on_map.items():
        for mutable_result_mapping in list_of_mutable_result_mappings:
            mutable, result = (
                mutable_result_mapping.mutable,
                mutable_result_mapping.mutable_result,
            )
            inverse_mapping[mutable].append(MapResultMapping(map, result))

    return inverse_mapping


@dataclass
class KEuclideanBestStrategy(MutationStrategy):
    k: int

    def create_mutation(
        self, model_results_on_map: GameMapsModelResults
    ) -> list[Mutable]:
        model_results_on_game_maps = invert_mapping(model_results_on_map)

        def sorter_for_mutable(model: Mutable):
            results: list[MapResultMapping] = model_results_on_game_maps[model]
            euc_dist = sum(
                [(100 - res.mutable_result.coverage_percent) ** 2 for res in results]
            )
            visited_instructions_sum = sum(
                [
                    res.mutable_result.move_reward.ForVisitedInstructions
                    for res in results
                ]
            )
            steps_sum = sum([res.mutable_result.steps_count for res in results])
            return (euc_dist, visited_instructions_sum, -steps_sum)

        models = model_results_on_game_maps.keys()

        return sorted(models, key=sorter_for_mutable, reverse=True)[: self.k]


class MutationStrategyBuilder(MutationStrategy):
    def __init__(self, mutation_strategies: list[MutationStrategy]) -> None:
        self.mutation_strategies = mutation_strategies

    def create_mutation(
        self, model_results_on_map: GameMapsModelResults
    ) -> list[Mutable]:
        result: list[Mutable] = []
        for strat in self.mutation_strategies:
            mutation = strat.create_mutation(model_results_on_map)
            if isinstance(mutation, list):
                result.extend(mutation)
            else:
                result.append(mutation)
        return result
