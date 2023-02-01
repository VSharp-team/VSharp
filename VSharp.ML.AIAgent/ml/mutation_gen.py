from collections import defaultdict
from dataclasses import dataclass

from common.game import GameMap, MoveReward
from ml.torch_model_wrapper import TorchModelWrapper, average_n_models, mutate_random

IterationResults = defaultdict[GameMap : list[tuple[TorchModelWrapper, MoveReward]]]


@dataclass
class MutationProportions:
    n_tops: int
    averaged_n_tops: int
    n_averaged_all: int
    random_n_tops_averaged_mutations: int
    random_all_averaged_mutations: int


def n_tops(iteration_data, n) -> list[TorchModelWrapper]:
    # топ по каждой карте (должны быть уникальны?)
    n_tops = []
    for game_map in iteration_data.keys():
        n_tops.extend(
            map(
                lambda t: t[0],  # take TfModelWrapper
                sorted(
                    iteration_data[game_map],
                    key=lambda t: (t[1].ForCoverage, t[1].ForVisitedInstructions),
                )[:n],
            )
            # default tuple comparison works fine
        )

    return n_tops


def averaged_n_tops(iteration_data, n) -> TorchModelWrapper:
    # среднее по топам
    return average_n_models(n_tops(iteration_data, n))


def averaged_all(iteration_data) -> TorchModelWrapper:
    # среднее по всем отобранным нейронкам

    # должны ли все нейронки быть уникальны?
    all = []
    for game_map in iteration_data.keys():
        all.extend(map(lambda t: t[0], iteration_data[game_map]))  # take TfModelWrapper

    return average_n_models(all)


def random_n_tops_averaged_mutations(iteration_data, n) -> list[TorchModelWrapper]:
    # случайные мутации среднего по топам
    return mutate_random(averaged_n_tops(iteration_data, n))


def random_all_averaged_mutations(iteration_data) -> TorchModelWrapper:
    # случайные мутации среднего по всем отобранным нейронкам
    return mutate_random(averaged_all(iteration_data))


def new_generation(
    iteration_data: IterationResults,
    proportions: MutationProportions,
) -> list[TorchModelWrapper]:
    new_gen = (
        n_tops(iteration_data, proportions.n_tops)
        + [
            averaged_n_tops(iteration_data, proportions.n_tops)
            for _ in range(proportions.averaged_n_tops)
        ]
        + [averaged_all(iteration_data) for _ in range(proportions.n_averaged_all)]
        + [
            random_n_tops_averaged_mutations(iteration_data, proportions.n_tops)
            for _ in range(proportions.random_n_tops_averaged_mutations)
        ]
        + [
            random_all_averaged_mutations(iteration_data)
            for _ in range(proportions.random_all_averaged_mutations)
        ]
    )

    return new_gen
