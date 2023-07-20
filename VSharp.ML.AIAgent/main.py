import pygad
import pygad.torchga

import learning.entry_point as ga
import ml.onnx.onnx_import
from config import GeneralConfig
from selection.crossover_type import CrossoverType
from selection.mutation_type import MutationType
from selection.parent_selection_type import ParentSelectionType


def main():
    num_generations = 20
    num_parents_mating = 10
    keep_elitism = 2

    model = GeneralConfig.MODEL_INIT()

    model.forward(*ml.onnx.onnx_import.create_torch_dummy_input())
    torch_ga = pygad.torchga.TorchGA(model=model, num_solutions=60)
    initial_population = torch_ga.population_weights

    ga.run(
        server_count=GeneralConfig.SERVER_COUNT,
        num_generations=num_generations,
        num_parents_mating=num_parents_mating,
        keep_elitism=keep_elitism,
        parent_selection_type=ParentSelectionType.STOCHASTIC_UNIVERSAL_SELECTION,
        crossover_type=CrossoverType.SINGLE_POINT,
        mutation_type=MutationType.RANDOM,
        mutation_percent_genes=30,
        initial_population=initial_population,
    )


if __name__ == "__main__":
    main()
