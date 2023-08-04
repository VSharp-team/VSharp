import learning.entry_point as ga
import ml.onnx.onnx_import
from config import GeneralConfig
from learning.selection.crossover_type import CrossoverType
from learning.selection.mutation_type import MutationType
from learning.selection.parent_selection_type import ParentSelectionType
from ml.utils import (
    create_population,
    model_weights_with_last_layer,
    model_weights_with_random_last_layer,
)


def main():
    model = GeneralConfig.MODEL_INIT()

    model.forward(*ml.onnx.onnx_import.create_torch_dummy_input())

    random_population = create_population(lo=-5, hi=5, model=model, population_size=4)
    with_random_last_layer = [
        model_weights_with_random_last_layer(lo=-1, hi=1, model=model) for _ in range(2)
    ]
    with_last_layer1 = model_weights_with_last_layer(
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
        model,
    )
    with_last_layer2 = model_weights_with_last_layer(
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
        model,
    )

    initial_population = [
        *random_population,
        *with_random_last_layer,
        with_last_layer1,
        with_last_layer2,
    ]

    ga.run(
        server_count=GeneralConfig.SERVER_COUNT,
        num_generations=GeneralConfig.NUM_GENERATIONS,
        num_parents_mating=GeneralConfig.NUM_PARENTS_MATING,
        keep_elitism=2,
        parent_selection_type=ParentSelectionType.STOCHASTIC_UNIVERSAL_SELECTION,
        crossover_type=CrossoverType.SINGLE_POINT,
        mutation_type=MutationType.RANDOM,
        mutation_percent_genes=GeneralConfig.MUTATION_PERCENT_GENES,
        initial_population=initial_population,
    )


if __name__ == "__main__":
    main()
