import logging

import torch.multiprocessing as mp

from displayer.utils import clean_log_file, clean_tables_file
from ml.model_wrappers.last_layer_learner import LastLayerLearner
from r_learn import r_learn
from selection.new_gen_functions import manual_new_gen, pyGAD_new_gen

logging.basicConfig(
    level=logging.INFO,
    filename="app.log",
    filemode="a",
    format="%(asctime)s - p%(process)d: %(name)s - [%(levelname)s]: %(message)s",
)


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

    models = [LastLayerLearner() for _ in range(n_models)]

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
        new_gen_provider_function=pyGAD_new_gen,
        epochs_to_verify=epochs_to_verify,
        ws_urls=ws_urls,
        proc_num=proc_num,
    )


if __name__ == "__main__":
    mp.set_start_method("spawn")
    main()
