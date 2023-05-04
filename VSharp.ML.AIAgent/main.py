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

    best_decart = selectors.select_k_best(scorer.decart_scorer, mr, k=1)
    k_best_decart = selectors.select_k_best(scorer.decart_scorer, mr, k=4)

    best_euclid = selectors.select_k_best(scorer.euclidean_scorer, mr, k=1)
    k_best_euclid = selectors.select_k_best(scorer.euclidean_scorer, mr, k=4)

    avg_best_tops = average(selectors.select_n_maps_tops(mr, n=1))

    avg_best_decart = average(k_best_decart)
    avg_best_euclid = average(k_best_euclid)

    return [
        *best_decart,
        *best_euclid,
        avg_best_decart,
        avg_best_euclid,
        avg_best_tops,

        *[GeneticLearner.mutate(avg_best_decart, mutation_volume=0.25, mutation_freq=0.2,) for _ in range(4)],
        *[GeneticLearner.mutate(avg_best_euclid, mutation_volume=0.25, mutation_freq=0.2,) for _ in range(4)],
        *[GeneticLearner.mutate(avg_best_tops, mutation_volume=0.25, mutation_freq=0.2,) for _ in range(4)],
        *[GeneticLearner.mutate(best_decart[0], mutation_volume=0.25, mutation_freq=0.2,) for _ in range(4)],
        *[GeneticLearner.mutate(best_euclid[0], mutation_volume=0.25, mutation_freq=0.2,) for _ in range(4)]
    ]


def main():
    socket_urls = [Constant.DEFAULT_GAMESERVER_URL]
    cm = ConnectionManager(socket_urls)

    loaded_model = load_full_model(Constant.IMPORTED_FULL_MODEL_PATH)

    epochs = 20
    max_steps = 600
    n_models = 20
    # verification every k epochs, start from 1
    # every 4th epoch
    epochs_to_verify = [i for i in range(4, epochs + 1) if i % 4 == 0]
    # epochs_to_verify = [4, 8, 10]

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
        epochs_to_verify=epochs_to_verify,
    )

    cm.close()


if __name__ == "__main__":
    main()
