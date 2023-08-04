import pygad
import pygad.torchga

import learning.entry_point as ga
import ml.onnx.onnx_import
from config import GeneralConfig
from learning.selection.crossover_type import CrossoverType
from learning.selection.mutation_type import MutationType
from learning.selection.parent_selection_type import ParentSelectionType


def main():
    model = GeneralConfig.MODEL_INIT()

    model.forward(*ml.onnx.onnx_import.create_torch_dummy_input())
    torch_ga = pygad.torchga.TorchGA(
        model=model, num_solutions=GeneralConfig.NUM_SOLUTIONS
    )
    initial_population = torch_ga.population_weights

    ga.run(
        server_count=GeneralConfig.SERVER_COUNT,
        num_generations=GeneralConfig.NUM_GENERATIONS,
        num_parents_mating=GeneralConfig.NUM_PARENTS_MATING,
        keep_elitism=GeneralConfig.KEEP_ELITISM,
        parent_selection_type=ParentSelectionType.STOCHASTIC_UNIVERSAL_SELECTION,
        crossover_type=CrossoverType.SINGLE_POINT,
        mutation_type=MutationType.RANDOM,
        mutation_percent_genes=GeneralConfig.MUTATION_PERCENT_GENES,
        initial_population=initial_population,
    )


if __name__ == "__main__":
    main()
