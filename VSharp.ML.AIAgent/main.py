import logging
import torch.multiprocessing as mp
import torch

# import multiprocessing_logging

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


# multiprocessing_logging.install_mp_handler()


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


# len(SOCKET_URLS) == proc_num
SOCKET_URLS = [
    "ws://0.0.0.0:8080/gameServer",
    "ws://0.0.0.0:8090/gameServer",
    "ws://0.0.0.0:8100/gameServer",
    "ws://0.0.0.0:8110/gameServer",
]


def main():
    epochs = 10
    max_steps = 300
    n_models = 10
    proc_num = len(SOCKET_URLS)
    # verification every k epochs, start from 1
    # every 4th epoch
    epochs_to_verify = [i for i in range(1, epochs + 1) if i % 4 == 0]
    # epochs_to_verify = [4, 8, 10]

    GeneticLearner.set_static_model()
    models = [GeneticLearner() for _ in range(n_models)]

    manager = mp.Manager()
    ws_urls = manager.Queue()
    for ws_url in SOCKET_URLS:
        ws_urls.put(ws_url)
    clean_tables_file()
    clean_log_file()
    r_learn(
        epochs_num=epochs,
        train_max_steps=max_steps,
        models=models,
        new_gen_provider_function=new_gen_function,
        epochs_to_verify=epochs_to_verify,
        ws_urls=ws_urls,
        proc_num=proc_num,
    )


if __name__ == "__main__":
    mp.set_start_method("spawn")
    main()
