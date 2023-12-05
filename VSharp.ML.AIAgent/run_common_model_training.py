import logging
import multiprocessing as mp
import os
import random
import typing as t
from dataclasses import asdict, dataclass
from datetime import datetime
from pathlib import Path

import numpy as np
import pandas as pd
import torch
import torch.nn as nn
import tqdm
from torch_geometric.loader import DataLoader

from config import GeneralConfig
from connection.broker_conn.socket_manager import game_server_socket_manager
from connection.game_server_conn.utils import MapsType, get_maps
from epochs_statistics.tables import create_pivot_table, table_to_string
from learning.play_game import play_game
from ml.common_model.dataset import FullDataset
from ml.common_model.paths import (
    BEST_MODELS_DICT_PATH,
    COMMON_MODELS_PATH,
    DATASET_MAP_RESULTS_FILENAME,
    DATASET_ROOT_PATH,
    PRETRAINED_MODEL_PATH,
    TRAINING_DATA_PATH,
)
from ml.common_model.utils import csv2best_models, get_model
from ml.common_model.wrapper import BestModelsWrapper, CommonModelWrapper
from ml.models.RGCNEdgeTypeTAG2VerticesDouble.model_modified import (
    StateModelEncoderLastLayer,
)
from ml.models.StateGNNEncoderConvEdgeAttr.model_modified import (
    StateModelEncoderLastLayer as RefStateModelEncoderLastLayer,
)
import optuna
from functools import partial
import joblib

LOG_PATH = Path("./ml_app.log")
TABLES_PATH = Path("./ml_tables.log")
COMMON_MODELS_PATH = Path(COMMON_MODELS_PATH)
BEST_MODELS_DICT = Path(BEST_MODELS_DICT_PATH)
TRAINING_DATA_PATH = Path(TRAINING_DATA_PATH)


logging.basicConfig(
    level=GeneralConfig.LOGGER_LEVEL,
    filename=LOG_PATH,
    filemode="a",
    format="%(asctime)s - p%(process)d: %(name)s - [%(levelname)s]: %(message)s",
)

if not COMMON_MODELS_PATH.exists():
    os.makedirs(COMMON_MODELS_PATH)

if not BEST_MODELS_DICT.exists():
    os.makedirs(BEST_MODELS_DICT_PATH)

if not TRAINING_DATA_PATH.exists():
    os.makedirs(TRAINING_DATA_PATH)


def create_file(file: Path):
    open(file, "w").close()


def append_to_file(file: Path, s: str):
    with open(file, "a") as file:
        file.write(s)


def play_game_task(task):
    maps, dataset, cmwrapper = task[0], task[1], task[2]
    result = play_game(
        with_predictor=cmwrapper,
        max_steps=GeneralConfig.MAX_STEPS,
        maps=maps,
        maps_type=MapsType.TRAIN,
        with_dataset=dataset,
    )
    return result


@dataclass
class TrainConfig:
    lr: float
    epochs: int
    batch_size: int
    optimizer: torch.optim.Optimizer
    loss: any
    random_seed: int


def train(trial: optuna.trial.Trial, dataset: FullDataset):
    config = TrainConfig(
        lr=trial.suggest_float("lr", 1e-7, 1e-3),
        batch_size=trial.suggest_int("batch_size", 32, 1024),
        epochs=10,
        optimizer=trial.suggest_categorical("optimizer", [torch.optim.Adam]),
        loss=trial.suggest_categorical("loss", [nn.KLDivLoss]),
        random_seed=937,
    )
    np.random.seed(config.random_seed)
    # for name, param in model.named_parameters():
    #     if "lin_last" not in name:
    #         param.requires_grad = False

    path_to_weights = os.path.join(
        PRETRAINED_MODEL_PATH,
        "RGCNEdgeTypeTAG2VerticesDouble",
        "64ch",
        "100e",
        "GNN_state_pred_het_dict",
    )
    model = get_model(
        Path(path_to_weights),
        lambda: StateModelEncoderLastLayer(hidden_channels=64, out_channels=8),
    )

    model.to(GeneralConfig.DEVICE)
    optimizer = config.optimizer(model.parameters(), lr=config.lr)
    criterion = config.loss()

    timestamp = datetime.now().timestamp()
    run_name = (
        f"{datetime.fromtimestamp(timestamp)}_{config.batch_size}_Adam_{config.lr}_KLDL"
    )

    print(run_name)
    path_to_saved_models = os.path.join(COMMON_MODELS_PATH, run_name)
    os.makedirs(path_to_saved_models)
    TABLES_PATH = Path(os.path.join(TRAINING_DATA_PATH, run_name + ".log"))
    create_file(TABLES_PATH)
    create_file(LOG_PATH)

    cmwrapper = CommonModelWrapper(model)

    with game_server_socket_manager() as ws:
        all_maps = get_maps(websocket=ws, type=MapsType.TRAIN)
        maps = np.array_split(all_maps, GeneralConfig.SERVER_COUNT)
        random.shuffle(maps)
        tasks = [
            (maps[i], FullDataset("", ""), cmwrapper)
            for i in range(GeneralConfig.SERVER_COUNT)
        ]

    mp.set_start_method("spawn", force=True)
    # p = Pool(GeneralConfig.SERVER_COUNT)

    all_average_results = []
    for epoch in range(config.epochs):
        data_list = dataset.get_plain_data()
        data_loader = DataLoader(data_list, batch_size=config.batch_size, shuffle=True)
        print("DataLoader size", len(data_loader))

        model.train()
        for batch in tqdm.tqdm(data_loader, desc="training"):
            batch.to(GeneralConfig.DEVICE)
            optimizer.zero_grad()

            out = model(
                game_x=batch["game_vertex"].x,
                state_x=batch["state_vertex"].x,
                edge_index_v_v=batch["game_vertex_to_game_vertex"].edge_index,
                edge_type_v_v=batch["game_vertex_to_game_vertex"].edge_type,
                edge_index_history_v_s=batch[
                    "game_vertex_history_state_vertex"
                ].edge_index,
                edge_attr_history_v_s=batch[
                    "game_vertex_history_state_vertex"
                ].edge_attr,
                edge_index_in_v_s=batch["game_vertex_in_state_vertex"].edge_index,
                edge_index_s_s=batch["state_vertex_parent_of_state_vertex"].edge_index,
            )
            y_true = batch.y_true
            loss = criterion(out, y_true)
            if loss != 0:
                loss.backward()
                optimizer.step()
            del out
            del batch
            torch.cuda.empty_cache()

        # validation
        model.eval()
        cmwrapper.make_copy(str(epoch + 1))

        with mp.Pool(GeneralConfig.SERVER_COUNT) as p:
            result = list(p.map(play_game_task, tasks))

            all_results = []
            for maps_result, maps_data in result:
                for map_name in maps_data.keys():
                    dataset.update(
                        map_name, maps_data[map_name][0], maps_data[map_name][1], True
                    )
                all_results += maps_result

            dataset.save()

        print(
            "Average dataset_state result",
            np.average(list(map(lambda x: x[0][0], dataset.maps_data.values()))),
        )
        average_result = np.average(
            list(map(lambda x: x.game_result.actual_coverage_percent, all_results))
        )
        all_average_results.append(average_result)
        table, _, _ = create_pivot_table(
            {cmwrapper: sorted(all_results, key=lambda x: x.map.MapName)}
        )
        table = table_to_string(table)
        append_to_file(
            TABLES_PATH,
            f"Epoch#{epoch}" + " Average coverage: " + str(average_result) + "\n",
        )
        append_to_file(TABLES_PATH, table + "\n")

        path_to_model = os.path.join(COMMON_MODELS_PATH, run_name, str(epoch + 1))
        torch.save(model.state_dict(), Path(path_to_model))
        del data_list
        del data_loader
    # p.close()

    return max(all_average_results)


def get_dataset(
    generate_dataset: bool, ref_model_init: t.Callable[[], torch.nn.Module]
):
    dataset = FullDataset(DATASET_ROOT_PATH, DATASET_MAP_RESULTS_FILENAME)

    if generate_dataset:
        with game_server_socket_manager() as ws:
            all_maps = get_maps(websocket=ws, type=MapsType.TRAIN)
        # creating new dataset
        best_models_dict = csv2best_models(ref_model_init=ref_model_init)
        play_game(
            with_predictor=BestModelsWrapper(best_models_dict),
            max_steps=GeneralConfig.MAX_STEPS,
            maps=all_maps,
            maps_type=MapsType.TRAIN,
            with_dataset=dataset,
        )
        dataset.save()
    else:
        # loading existing dataset
        dataset.load()
    return dataset


def main():
    print(GeneralConfig.DEVICE)
    ref_model_initializer = lambda: RefStateModelEncoderLastLayer(
        hidden_channels=32, out_channels=8
    )

    generate_dataset = False
    dataset = get_dataset(generate_dataset, ref_model_init=ref_model_initializer)

    sampler = optuna.samplers.TPESampler(n_startup_trials=10)
    study = optuna.create_study(sampler=sampler, direction="maximize")
    objective = partial(train, dataset=dataset)
    study.optimize(objective, n_trials=100)
    joblib.dump(study, f"{datetime.fromtimestamp(datetime.now().timestamp())}.pkl")


if __name__ == "__main__":
    main()
