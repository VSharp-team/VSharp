import logging
from typing import Callable

import torch.multiprocessing as mp

from displayer.utils import clean_log_file, clean_tables_file
from ml.model_wrappers.genetic_learner import GeneticLearner
from ml.model_wrappers.protocols import Mutable
from r_learn import r_learn
from selection import scorer, selectors
from selection.classes import ModelResultsOnGameMaps

logging.basicConfig(
    level=logging.INFO,
    filename="app.log",
    filemode="a",
    format="%(asctime)s - p%(process)d: %(name)s - [%(levelname)s]: %(message)s",
)


def keeps_duplicates(new_gen_func) -> Callable[[ModelResultsOnGameMaps], list[Mutable]]:
    """
    allows to create multiple instances of the same model
    """

    def wrapper(mr: ModelResultsOnGameMaps) -> list[Mutable]:
        selected_models = new_gen_func(mr)
        to_copy: tuple[Mutable, int] = []

        for unique_model in set(selected_models):
            if selected_models.count(unique_model) > 1:
                to_copy.append((unique_model, selected_models.count(unique_model) - 1))

        res: list[Mutable] = []

        for model, copy_count in to_copy:
            last_copied = model
            for _ in range(copy_count):
                new_model = last_copied.copy(last_copied.name() + "*")
                res.append(new_model)
                last_copied = new_model

        res += list(set(selected_models))

        assert len(res) == len(selected_models)
        return res

    return wrapper


@keeps_duplicates
def new_gen_function(mr: ModelResultsOnGameMaps) -> list[Mutable]:
    """
    helper methods
    """

    def mutate(model: Mutable):
        return GeneticLearner.mutate(
            model,
            mutation_volume=0.15,
            mutation_freq=0.2,
        )

    def average(models: list[Mutable]):
        return GeneticLearner.average(models)

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
        *[mutate(avg_best_decart) for _ in range(4)],
        *[mutate(avg_best_euclid) for _ in range(4)],
        *[mutate(avg_best_tops) for _ in range(4)],
        *[mutate(best_decart[0]) for _ in range(4)],
        *[mutate(best_euclid[0]) for _ in range(4)],
    ]


START_PORT = 8100
SERVER_COUNT = 7

# len(SOCKET_URLS) == proc_num
SOCKET_URLS = [f"ws://0.0.0.0:{START_PORT + i}/gameServer" for i in range(SERVER_COUNT)]


def main():
    epochs = 26
    max_steps = 500
    n_models = 25
    proc_num = len(SOCKET_URLS)
    # verification every k epochs, start from 1
    epochs_to_verify = [i for i in range(1, epochs + 1) if (i - 1) % 5 == 0]

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
