import logging

from agent.connection_manager import ConnectionManager
from agent.n_agent import get_train_maps, get_validation_maps
from common.constants import Constant
from displayer.utils import clean_tables_file
from ml.model_wrappers.genetic_learner import GeneticLearner
from ml.model_wrappers.protocols import Mutable
from selection import scorer, selectors
from selection.classes import ModelResultsOnGameMaps
from ml.utils import load_full_model
from r_learn import r_learn

logging.basicConfig(
    level=logging.DEBUG,
    filename="app.log",
    filemode="w",
    format="%(asctime)s - %(name)s - [%(levelname)s]: %(message)s",
)


def new_gen_function(mr: ModelResultsOnGameMaps) -> list[Mutable]:
    """
    helper methods
    """

    def mutate(model: Mutable):
        return GeneticLearner.mutate(
            model,
            mutation_volume=0.25,
            mutation_freq=0.1,
        )

    def average(models: list[Mutable]):
        return GeneticLearner.average(models)

    """
    selection & mutation
    """

    best_mutables = [
        *selectors.select_k_best(scorer.decart_scorer, mr, k=3),
        *selectors.select_n_maps_tops(mr, n=4),
    ]

    average_of_best = average(best_mutables)
    mutated_average_of_best = mutate(average_of_best)
    mutated_average_of_all = average(
        selectors.select_all_models(model_result_with_maps=mr)
    )

    tournament_average = average(
        selectors.tournament_selection(
            model_result_with_maps=mr,
            desired_population=3,
            n_comparisons=4,
            scorer=scorer.decart_scorer,
        )
    )
    tournament_average_mutated = mutate(tournament_average)

    """
    assemble generation
    """

    return [
        *selectors.select_k_best(scorer.decart_scorer, mr, k=5),
        average_of_best,
        mutated_average_of_best,
        mutated_average_of_all,
        tournament_average,
        tournament_average_mutated,
    ]


def main():
    socket_urls = [Constant.DEFAULT_GAMESERVER_URL]
    cm = ConnectionManager(socket_urls)

    loaded_model = load_full_model(Constant.IMPORTED_FULL_MODEL_PATH)

    epochs = 10
    max_steps = 300
    n_models = 10

    GeneticLearner.set_model(loaded_model, 8)
    models = [GeneticLearner() for _ in range(n_models)]

    def train_maps_provider():
        return get_train_maps(cm)

    def validation_maps_provider():
        return get_validation_maps(cm)

    clean_tables_file()
    r_learn(
        epochs=epochs,
        train_max_steps=max_steps,
        models=models,
        train_maps_provider=train_maps_provider,
        validation_maps_provider=validation_maps_provider,
        new_gen_provider_function=new_gen_function,
        connection_manager=cm,
    )

    cm.close()


if __name__ == "__main__":
    main()
