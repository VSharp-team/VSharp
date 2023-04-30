import logging
from multiprocessing import Manager

import multiprocessing_logging

from common.constants import Constant
from displayer.utils import clean_log_file, clean_tables_file
from ml.model_wrappers.genetic_learner import GeneticLearner
from ml.model_wrappers.protocols import Mutable
from r_learn import r_learn
from selection import scorer, selectors
from selection.classes import ModelResultsOnGameMaps

logging.basicConfig(
    level=logging.DEBUG,
    filename="app.log",
    filemode="a",
    format="%(asctime)s - p%(process)d: %(name)s - [%(levelname)s]: %(message)s",
)


multiprocessing_logging.install_mp_handler()


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
    epochs = 10
    max_steps = 300
    n_models = 10
    # verification every k epochs, start from 1
    # every 4th epoch
    epochs_to_verify = [i for i in range(1, epochs + 1) if i % 4 == 0]
    # epochs_to_verify = [4, 8, 10]

    models = [GeneticLearner() for _ in range(n_models)]

    manager = Manager()
    ws_urls = manager.Queue()
    for ws_url in [url for url in Constant.SOKET_URLS]:
        ws_urls.put(ws_url)
    clean_tables_file()
    clean_log_file()
    r_learn(
        epochs=epochs,
        train_max_steps=max_steps,
        models=models,
        new_gen_provider_function=new_gen_function,
        epochs_to_verify=epochs_to_verify,
        ws_urls=ws_urls,
    )


if __name__ == "__main__":
    main()
