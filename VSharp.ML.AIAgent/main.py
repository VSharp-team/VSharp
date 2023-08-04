import learning.entry_point as ga
import ml.onnx.onnx_import
from common.constants import IMPORTED_DICT_MODEL_PATH
from config import GeneralConfig
from learning.selection.crossover_type import CrossoverType
from learning.selection.mutation_type import MutationType
from learning.selection.parent_selection_type import ParentSelectionType
from ml.utils import (
    create_population,
    load_model,
    model_weights_with_last_layer,
    model_weights_with_random_last_layer,
)


def main():
    imported_model = load_model(
        IMPORTED_DICT_MODEL_PATH, model=GeneralConfig.IMPORT_MODEL_INIT()
    )
    imported_model.forward(*ml.onnx.onnx_import.create_torch_dummy_input())

    exported_model = GeneralConfig.EXPORT_MODEL_INIT()
    exported_model.forward(*ml.onnx.onnx_import.create_torch_dummy_input())

    random_population = create_population(
        lo=-5,
        hi=5,
        model=exported_model,
        population_size=GeneralConfig.NUM_RANDOM_SOLUTIONS,
    )
    with_random_last_layer = [
        model_weights_with_random_last_layer(
            lo=-1,
            hi=1,
            old_sd=imported_model.state_dict(),
            new_sd=exported_model.state_dict(),
        )
        for _ in range(GeneralConfig.NUM_RANDOM_LAST_LAYER)
    ]
    with_last_layer1 = model_weights_with_last_layer(
        old_sd=imported_model.state_dict(),
        new_sd=exported_model.state_dict(),
        last_layer_weights=[
            -0.7853140655460631,
            0.7524892603731441,
            0.2844810949678288,
            -0.6819831165289404,
            -0.0830326280153653,
            0.1779108098019602,
            0.95478059636744,
            0.27937866719070503,
        ],
    )
    with_last_layer2 = model_weights_with_last_layer(
        old_sd=imported_model.state_dict(),
        new_sd=exported_model.state_dict(),
        last_layer_weights=[
            -0.7853139452883172,
            0.752490045931864,
            0.2844807733073216,
            -0.6819766889604519,
            -0.08303258833890134,
            0.17791068654815034,
            0.9555442824877577,
            0.2793786892860371,
        ],
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
        keep_elitism=GeneralConfig.KEEP_ELITISM,
        parent_selection_type=ParentSelectionType.STOCHASTIC_UNIVERSAL_SELECTION,
        crossover_type=CrossoverType.UNIFORM,
        mutation_type=MutationType.RANDOM,
        mutation_percent_genes=GeneralConfig.MUTATION_PERCENT_GENES,
        initial_population=initial_population,
    )


if __name__ == "__main__":
    main()
