import pygad
import pygad.torchga

import learning.genetic_alorithm as ga
from common.constants import BASE_NN_OUT_FEATURES_NUM, IMPORTED_DICT_MODEL_PATH
from config import GeneralConfig
from ml.utils import (
    load_model_with_last_layer,
    model_weights_with_random_last_layer,
    random_model_weights,
)
from selection.crossover_type import CrossoverType
from selection.mutation_type import MutationType
from selection.parent_selection_type import ParentSelectionType


def weights_vector(weights: list[float], model_path: str = IMPORTED_DICT_MODEL_PATH):
    model = load_model_with_last_layer(
        model_path,
        weights,
    )
    assert len(weights) == BASE_NN_OUT_FEATURES_NUM
    return pygad.torchga.model_weights_as_vector(model)


def n_random_model_weights(
    n: int, low: float, hi: float, model_path: str = IMPORTED_DICT_MODEL_PATH
):
    rv = [
        random_model_weights(low=low, hi=hi, model_load_path=model_path)
        for _ in range(n)
    ]
    return rv


def n_random_last_layer_model_weights(
    n: int, low: float, hi: float, model_path: str = IMPORTED_DICT_MODEL_PATH
):
    rv = [
        model_weights_with_random_last_layer(low=low, hi=hi, model_load_path=model_path)
        for _ in range(n)
    ]
    return rv


def main():
    num_models_with_random_last_layer = 28
    num_random_models = 30
    num_generations = 20
    num_parents_mating = 10
    keep_parents = 2
    random_init_weights_max_val = 5.0
    random_init_weights_min_val = -5.0
    initial_weights = [
        [
            -0.7853140655460631,
            0.7524892603731441,
            0.2844810949678288,
            -0.6819831165289404,
            -0.0830326280153653,
            0.1779108098019602,
            0.95478059636744,
            0.27937866719070503,
        ],
        [
            -0.7853139452883172,
            0.752490045931864,
            0.2844807733073216,
            -0.6819766889604519,
            -0.08303258833890134,
            0.17791068654815034,
            0.9555442824877577,
            0.2793786892860371,
        ],
    ]
    initial_population = []
    for last_layer in initial_weights:
        initial_population.append(weights_vector(last_layer))

    with_random_last_layer_weights = n_random_last_layer_model_weights(
        n=num_models_with_random_last_layer,
        low=random_init_weights_min_val,
        hi=random_init_weights_max_val,
    )

    initial_population += with_random_last_layer_weights

    with_random_weights = n_random_model_weights(
        n=num_random_models,
        low=random_init_weights_min_val,
        hi=random_init_weights_max_val,
    )

    initial_population += with_random_weights

    ga.run(
        server_count=GeneralConfig.SERVER_COUNT,
        num_generations=num_generations,
        num_parents_mating=num_parents_mating,
        keep_parents=keep_parents,
        parent_selection_type=ParentSelectionType.STOCHASTIC_UNIVERSAL_SELECTION,
        crossover_type=CrossoverType.SINGLE_POINT,
        mutation_type=MutationType.RANDOM,
        mutation_percent_genes=30,
        initial_population=initial_population,
    )


if __name__ == "__main__":
    main()
