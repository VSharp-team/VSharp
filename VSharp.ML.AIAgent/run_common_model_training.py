import copy
from dataclasses import dataclass, asdict
import logging
from multiprocessing import Pool
import multiprocessing

import os
from pathlib import Path
from datetime import datetime
import random

import torch
import torch.nn as nn
import numpy as np

from config import GeneralConfig
from connection.broker_conn.socket_manager import game_server_socket_manager
from connection.game_server_conn.utils import MapsType, get_maps
from epochs_statistics.tables import create_pivot_table, table_to_string
from learning.play_game import play_game
from ml.common_model.models import CommonModel
from ml.common_model.utils import (
    csv2best_models,
    get_model,
    euclidean_dist,
    save_best_models2csv,
    load_best_models_dict,
    load_dataset_state_dict,
    back_prop,
)
from ml.common_model.wrapper import CommonModelWrapper, BestModelsWrapper
from ml.fileop import save_model
from ml.common_model.paths import (
    common_models_path,
    best_models_dict_path,
    dataset_root_path,
    dataset_map_results_file_name,
    training_data_path,
    pretrained_models_path,
)
from ml.model_wrappers.protocols import Predictor
from ml.utils import load_model, convert_to_export
import numpy as np
from ml.common_model.dataset import FullDataset
from torch_geometric.loader import DataLoader
import tqdm
import pandas as pd
from ml.models.TAGSageSimple.model import StateModelEncoder
from ml.models.TAGSageSimple.model_modified import StateModelEncoderLastLayer


LOG_PATH = Path("./ml_app.log")
TABLES_PATH = Path("./ml_tables.log")
COMMON_MODELS_PATH = Path(common_models_path)
BEST_MODELS_DICT = Path(best_models_dict_path)
TRAINING_DATA_PATH = Path(training_data_path)


logging.basicConfig(
    level=GeneralConfig.LOGGER_LEVEL,
    filename=LOG_PATH,
    filemode="a",
    format="%(asctime)s - p%(process)d: %(name)s - [%(levelname)s]: %(message)s",
)

if not COMMON_MODELS_PATH.exists():
    os.makedirs(common_models_path)

if not BEST_MODELS_DICT.exists():
    os.makedirs(best_models_dict_path)

if not TRAINING_DATA_PATH.exists():
    os.makedirs(training_data_path)


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


def train(train_config: TrainConfig, model: torch.nn.Module, dataset: FullDataset):
    for name, param in model.named_parameters():
        if "lin_last" not in name:
            param.requires_grad = False

    model.to(GeneralConfig.DEVICE)
    optimizer = torch.optim.Adam(model.parameters(), lr=train_config.lr)
    criterion = nn.KLDivLoss()

    timestamp = datetime.now().timestamp()
    run_name = f"{datetime.fromtimestamp(timestamp)}_{train_config.batch_size}_Adam_{train_config.lr}_KLDL"

    print(run_name)
    path_to_saved_models = os.path.join(common_models_path, run_name)
    os.makedirs(path_to_saved_models)
    TABLES_PATH = Path(os.path.join(training_data_path, run_name + ".log"))
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

    multiprocessing.set_start_method("spawn", force=True)

    all_average_results = []
    for epoch in range(train_config.epochs):
        data_list = dataset.get_plain_data()
        data_loader = DataLoader(
            data_list, batch_size=train_config.batch_size, shuffle=True
        )
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
        result = list(map(play_game_task, tasks))

        all_results = []
        for maps_result, maps_data in result:
            for map_name in maps_data.keys():
                dataset.update(map_name, maps_data[map_name][0], maps_data[map_name][1])
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
        all_results = sorted(all_results, key=lambda x: x.map.MapName)
        table, _, _ = create_pivot_table({cmwrapper: all_results})
        table = table_to_string(table)
        append_to_file(
            TABLES_PATH,
            f"Epoch#{epoch}" + " Average coverage: " + str(average_result) + "\n",
        )
        append_to_file(TABLES_PATH, table + "\n")

        path_to_model = os.path.join(common_models_path, run_name, str(epoch + 1))
        torch.save(model.state_dict(), Path(path_to_model))
        del data_list
        del data_loader

    return all_average_results


def get_dataset(generate_dataset: bool):
    dataset = FullDataset(dataset_root_path, dataset_map_results_file_name)

    with game_server_socket_manager() as ws:
        all_maps = get_maps(websocket=ws, type=MapsType.TRAIN)
    if generate_dataset:
        # creating new dataset
        best_models_dict = csv2best_models()
        play_game(
            with_predictor=BestModelsWrapper(best_models_dict),
            max_steps=GeneralConfig.MAX_STEPS,
            maps=all_maps,
            maps_type=MapsType.TRAIN,
            with_dataset=dataset,
        )
        dataset.save()
        generate_dataset = False
    else:
        # loading existing dataset
        dataset.load()
    return dataset


def main():
    print(GeneralConfig.DEVICE)
    path_to_weights = os.path.join(
        pretrained_models_path,
        "TAGSageSimple",
        "32ch",
        "20e",
        "GNN_state_pred_het_dict",
    )

    best_result = {"average_coverage": 0, "config": dict(), "epoch": 0}
    generate_dataset = True
    dataset = get_dataset(generate_dataset)

    while True:
        config = TrainConfig(
            lr=random.choice([10 ** (-i) for i in range(3, 8)]),
            batch_size=random.choice([2**i for i in range(5, 10)]),
            epochs=10,
        )
        print("Current hyperparameters")
        data_frame = pd.DataFrame(
            data=[asdict(config).values()],
            columns=asdict(config).keys(),
            index=["value"],
        )
        print(data_frame)

        model = get_model(
            Path(path_to_weights),
            StateModelEncoderLastLayer(hidden_channels=32, out_channels=8),
        )

        results = train(train_config=config, model=model, dataset=dataset)
        generate_dataset = True
        max_value = max(results)
        max_ind = results.index(max_value)
        if best_result["average_coverage"] < max_value:
            best_result["average_coverage"] = max_value
            best_result["config"] = asdict(config)
            best_result["epoch"] = max_ind + 1
        print(
            f"The best result for now:\nAverage coverage: {best_result['average_coverage']}"
        )
        data_frame = pd.DataFrame(
            data=[best_result["config"].values()],
            columns=best_result["config"].keys(),
            index=["value"],
        )
        print(data_frame)


if __name__ == "__main__":
    main()
