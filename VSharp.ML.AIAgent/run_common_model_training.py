import copy
import logging
from multiprocessing import Pool
import multiprocessing
import os
from pathlib import Path
from datetime import datetime
import random

import torch
import torch.nn as nn

from config import GeneralConfig
from connection.broker_conn.socket_manager import game_server_socket_manager
from connection.game_server_conn.utils import MapsType, get_maps
from epochs_statistics.tables import create_pivot_table, table_to_string
from learning.play_game import play_game
from ml.common_model.models import CommonModel
from ml.common_model.utils import (
    csv2best_models,
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
)
from ml.model_wrappers.protocols import Predictor
from ml.utils import load_model, convert_to_export
import numpy as np
from ml.common_model.dataset import FullDataset
from torch_geometric.loader import DataLoader
import tqdm
from ml.model_modified import StateModelEncoderExport, StateModelEncoderExportONNX
import pandas as pd


LOG_PATH = Path("./ml_app.log")
TABLES_PATH = Path("./ml_tables.log")
COMMON_MODELS_PATH = Path(common_models_path)
BEST_MODELS_DICT = Path(best_models_dict_path)
TRAINING_DATA_PATH = Path(training_data_path)

GENERATE_DATASET = False

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


def f(task):
    maps, dataset, cmwrapper = task[0], task[1], task[2]
    result = play_game(
        with_predictor=cmwrapper,
        max_steps=GeneralConfig.MAX_STEPS,
        maps=maps,
        maps_type=MapsType.TRAIN,
        with_dataset=dataset,
    )
    return result


def train(config):
    # model = CommonModel(
    #     hidden_channels=config['hidden_channels'],
    #     num_gv_layers=config['num_gv_layers'],
    #     num_sv_layers=config['num_sv_layers'],
    #     num_gv_hops=config["num_gv_hops"],
    #     num_sv_hops=config["num_sv_hops"],
    # )

    path_to_model = os.path.join(
        "ml",
        "pretrained_models",
        "GNN_state_pred_het_dict_onxx_with_last_layer",
    )
    model = StateModelEncoderExportONNX(hidden_channels=32, out_channels=8)

    model.load_state_dict(torch.load(path_to_model))

    for name, param in model.named_parameters():
        if "lin_last" not in name:
            param.requires_grad = False

    model.to(GeneralConfig.DEVICE)
    optimizer = torch.optim.Adam(model.parameters(), lr=config["lr"])
    criterion = nn.KLDivLoss()
    best_models_dict = csv2best_models()

    timestamp = datetime.now().timestamp()
    # run_name = f"{datetime.fromtimestamp(timestamp, tz=None)}_{config['batch_size']}_Adam_{config['lr']}_KLDL_"\
    #     f"softmax_classification_not_pretrained_{config['hidden_channels']}_{config['num_gv_layers']}_{config['num_sv_layers']}_"\
    #         f"tag_{config['num_gv_hops']}_{config['num_sv_hops']}"
    run_name = (
        f"{datetime.fromtimestamp(timestamp, tz=None)}_{config['batch_size']}_Adam_{config['lr']}_KLDL_"
        f"last_layer"
    )

    print(run_name)
    path_to_saved_models = os.path.join(common_models_path, run_name)
    os.makedirs(path_to_saved_models)
    TABLES_PATH = Path(os.path.join(training_data_path, run_name + ".log"))
    create_file(TABLES_PATH)
    create_file(LOG_PATH)

    bmwrapper = BestModelsWrapper(model, best_models_dict)
    cmwrapper = CommonModelWrapper(model)
    dataset = FullDataset(dataset_root_path, dataset_map_results_file_name)

    with game_server_socket_manager() as ws:
        all_maps = get_maps(websocket=ws, type=MapsType.TRAIN)
        # maps = np.array_split(all_maps, GeneralConfig.SERVER_COUNT)
        # random.shuffle(maps)
        # global tasks
        # tasks = [
        #     (maps[i], FullDataset("", ""), cmwrapper)
        #     for i in range(GeneralConfig.SERVER_COUNT)
        # ]

    if GENERATE_DATASET:
        # creating new dataset
        play_game(
            with_predictor=bmwrapper,
            max_steps=GeneralConfig.MAX_STEPS,
            maps=all_maps,
            maps_type=MapsType.TRAIN,
            with_dataset=dataset,
        )
        dataset.save()
    else:
        # loading existing dataset
        dataset.load()

    # multiprocessing.set_start_method("spawn", force=True)
    # p = Pool(GeneralConfig.SERVER_COUNT)

    all_average_results = []
    # with Pool(GeneralConfig.SERVER_COUNT) as p:
    for epoch in range(config["epochs"]):
        data_list = dataset.get_plain_data()
        data_loader = DataLoader(
            data_list, batch_size=config["batch_size"], shuffle=True
        )
        print("DataLoader size", len(data_loader))

        model.train()
        for batch in tqdm.tqdm(data_loader, desc="training"):
            batch.to(GeneralConfig.DEVICE)
            optimizer.zero_grad()

            out = model(
                batch["game_vertex"].x,
                batch["state_vertex"].x,
                batch[("game_vertex", "to", "game_vertex")].edge_index,
                batch[("game_vertex", "history", "state_vertex")].edge_index,
                batch[("game_vertex", "history", "state_vertex")].edge_attr,
                batch[("game_vertex", "in", "state_vertex")].edge_index,
                batch[("state_vertex", "parent_of", "state_vertex")].edge_index,
            )["state_vertex"]
            # out = model(batch["game_vertex"].x, batch.edge_index_dict, batch.edge_attr_dict)[
            #     "state_vertex"
            # ]
            y_true = batch.y_true["state_vertex"]
            loss = criterion(out, y_true)
            if loss != 0:
                loss.backward()
                optimizer.step()

        # validation
        model.eval()
        cmwrapper.make_copy(str(epoch + 1))

        # result = p.map(f, tasks)
        # all_results = []
        # for maps_result, maps_data in result:
        #     for map_name in maps_data.keys():
        #         dataset.update(map_name, maps_data[map_name][0], maps_data[map_name][1])
        #     all_results += maps_result

        # dataset.save()
        all_results = play_game(
            with_predictor=cmwrapper,
            max_steps=GeneralConfig.MAX_STEPS,
            maps=all_maps,
            maps_type=MapsType.TRAIN,
            with_dataset=dataset,
        )[0]
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
        path_to_best_models_dict = os.path.join(
            best_models_dict_path, str(epoch + 1) + ".csv"
        )
        save_best_models2csv(best_models_dict, path_to_best_models_dict)
        del data_list
        del data_loader
    # p.close()

    return all_average_results


def main():
    lr = 0.000001
    epochs = 10
    batch_size = 64
    hidden_channels = 32
    num_gv_layers = 3
    num_sv_layers = 3
    print(GeneralConfig.DEVICE)
    best_result = {"average_coverage": 0, "config": dict(), "epoch": 0}

    while True:
        config = {
            "hidden_channels": random.choice([i for i in range(16, 256, 20)]),
            "num_gv_layers": random.choice([i for i in range(1, 9)]),
            "num_sv_layers": random.choice([i for i in range(1, 9)]),
            "num_gv_hops": random.choice([i for i in range(1, 9)]),
            "num_sv_hops": random.choice([i for i in range(1, 9)]),
            "lr": random.choice([10 ** (-i) for i in range(2, 8)]),
            "batch_size": random.choice([2**i for i in range(5, 9)]),
            "epochs": 10,
        }
        print("Current hyperparameters")
        data_frame = pd.DataFrame(
            data=config.values(), columns=["value"], index=config.keys()
        )
        print(data_frame)
        results = train(config)
        max_value, ind = torch.max(results)
        if best_result["average_coverage"] < max_value:
            best_result["average_coverage"] = max_value
            best_result["config"] = config
            best_result["epoch"] = ind + 1
        print(
            f"The best result for now: \n Average coverage: {best_result['average_coverage']}"
        )
        data_frame = pd.DataFrame(
            data=best_result["config"].values(),
            columns=["value"],
            index=best_result["config"].keys(),
        )

    # path_to_model = os.path.join(
    #     "ml",
    #     "pretrained_models",
    #     "-262.75775990410693.pth",
    # )

    path_to_model = os.path.join(
        "ml",
        "pretrained_models",
        "GNN_state_pred_het_dict__StateGNNEncoderConvEdgeAttr_state_after_with_last_layer.pth",
    )

    # best_result = result.get_best_result("average_result", "max", "last")
    # print(f"Best result config: {best_result.config}")
    # print(f"Best average_result: {best_result.last_result['average_result']}")

    # export_model_sd = GeneralConfig.EXPORT_MODEL_INIT().state_dict()

    # model = StateModelEncoderExportCompact(hidden_channels=32, out_channels=8)

    # model.load_state_dict(torch.load(path_to_model))

    # for name, param in model.named_parameters():
    #     if "lin_last" not in name:
    #         param.requires_grad = False
    #     print(param.requires_grad)

    # path_to_best_models_dict = os.path.join(
    #     best_models_dict_path,
    #     "2",
    # )
    # best_models_dict = load_best_models_dict(path_to_best_models_dict)

    # print(tasks)


if __name__ == "__main__":
    main()
