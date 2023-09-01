import logging
import os
from pathlib import Path
from datetime import datetime

import torch
import torch.nn as nn

from config import GeneralConfig
from connection.game_server_conn.utils import MapsType
from epochs_statistics.tables import create_pivot_table, table_to_string
from learning.play_game import play_game
from ml.common_model.models import CommonModel
from ml.common_model.utils import (
    csv2best_models,
    euclidean_dist,
    save_best_models2csv,
    load_best_models_dict,
    load_dataset_state_dict,
    save_dataset_state_dict,
    back_prop,
)
from ml.common_model.wrapper import CommonModelWrapper, BestModelsWrapper
from ml.fileop import save_model
from ml.common_model.paths import (
    common_models_path,
    best_models_dict_path,
    dataset_root_path,
    dataset_state_path,
    training_data_path,
)
from ml.model_wrappers.protocols import Predictor
from ml.utils import load_model, convert_to_export
import numpy as np
from ml.common_model.dataset import FullDataset
from torch_geometric.loader import DataLoader
import tqdm
from ml.model_modified import StateModelEncoderExportCompact

LOG_PATH = Path("./ml_app.log")
TABLES_PATH = Path("./ml_tables.log")
COMMON_MODELS_PATH = Path(common_models_path)
BEST_MODELS_DICT = Path(best_models_dict_path)
DATASET_STATE_PATH = Path(dataset_state_path)
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

if not DATASET_STATE_PATH.exists():
    best_models_dict = csv2best_models()
    dataset_state_dict = {}
    for map_name in best_models_dict.keys():
        dataset_state_dict[map_name] = best_models_dict[map_name][1]
    save_dataset_state_dict(dataset_state_dict, DATASET_STATE_PATH)

if not TRAINING_DATA_PATH.exists():
    os.makedirs(training_data_path)


def create_file(file: Path):
    open(file, "w").close()


def append_to_file(file: Path, s: str):
    with open(file, "a") as file:
        file.write(s)


def main():
    lr = 0.000001
    epochs = 50
    batch_size = 64
    print(GeneralConfig.DEVICE)

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

    # export_model_sd = GeneralConfig.EXPORT_MODEL_INIT().state_dict()

    model = StateModelEncoderExportCompact(hidden_channels=32, out_channels=8)
    model.load_state_dict(torch.load(path_to_model))
    model.to(GeneralConfig.DEVICE)

    # for name, param in model.named_parameters():
    #     if "lin_last" not in name:
    #         param.requires_grad = False
    #     print(param.requires_grad)

    timestamp = datetime.now().timestamp()
    run_name = f"{datetime.fromtimestamp(timestamp, tz=None)}_{batch_size}_Adam_{lr}_KLDL_softmax_other_y_true"
    print(run_name)
    path_to_saved_models = os.path.join(common_models_path, run_name)
    os.makedirs(path_to_saved_models)
    TABLES_PATH = Path(os.path.join(training_data_path, run_name + ".log"))
    create_file(TABLES_PATH)
    create_file(LOG_PATH)

    optimizer = torch.optim.Adam(model.parameters(), lr=lr)
    criterion = nn.KLDivLoss()
    best_models_dict = csv2best_models()
    dataset_state = load_dataset_state_dict(dataset_state_path)

    # path_to_best_models_dict = os.path.join(
    #     best_models_dict_path,
    #     "2",
    # )
    # best_models_dict = load_best_models_dict(path_to_best_models_dict)

    bmwrapper = BestModelsWrapper(
        model, best_models_dict, optimizer, criterion, dataset_state
    )
    cmwrapper = CommonModelWrapper(model, best_models_dict, dataset_state)

    data_dict = {}

    dataset = FullDataset(
        dataset_root_path, best_models=best_models_dict, data_dict=data_dict
    )
    # creating dataset
    play_game(
        with_predictor=bmwrapper,
        max_steps=GeneralConfig.MAX_STEPS,
        maps_type=MapsType.TRAIN,
        with_dataset=dataset,
    )

    dataset = FullDataset(
        dataset_root_path, best_models=best_models_dict, data_dict=data_dict
    )
    for single_method_data, map_name in tqdm.tqdm(dataset, desc="data loading"):
        single_method_data_list = []
        for i in single_method_data:
            if i["y_true"]["state_vertex"].size()[0] != 1:
                if not i["y_true"]["state_vertex"].isnan().any():
                    max_ind = torch.argmax(i["y_true"]["state_vertex"])
                    i["y_true"]["state_vertex"] = torch.zeros_like(
                        i["y_true"]["state_vertex"]
                    )
                    i["y_true"]["state_vertex"][max_ind] = 1.0
                    single_method_data_list.append(i)
        data_dict[map_name] = single_method_data_list

    for epoch in range(epochs):
        data_list = []
        for map_name in data_dict.keys():
            data_list += data_dict[map_name]

        data_loader = DataLoader(data_list, batch_size=batch_size, shuffle=True)
        print("DataLoader size", len(data_loader))

        # training
        # play_game(
        #     with_predictor=bmwrapper,
        #     max_steps=GeneralConfig.MAX_STEPS,
        #     maps_type=MapsType.TRAIN,
        #     with_dataset=dataset
        # )
        model.train()
        for batch in tqdm.tqdm(data_loader, desc="training"):
            batch.to(GeneralConfig.DEVICE)
            optimizer.zero_grad()
            out = model(batch.x_dict, batch.edge_index_dict, batch.edge_attr_dict)[
                "state_vertex"
            ]
            y_true = batch.y_true["state_vertex"]
            loss = criterion(out, y_true)
            if loss != 0:
                loss.backward()
                optimizer.step()

        # validation
        model.eval()
        cmwrapper.make_copy(str(epoch + 1))
        result = play_game(
            with_predictor=cmwrapper,
            max_steps=GeneralConfig.MAX_STEPS,
            maps_type=MapsType.TRAIN,
            with_dataset=dataset,
        )
        print(
            "Average dataset_state result",
            np.average(list(map(lambda x: x[0], dataset_state.values()))),
        )
        average_result = np.average(
            list(map(lambda x: x.game_result.actual_coverage_percent, result))
        )
        result = sorted(result, key=lambda x: x.map.MapName)
        table, _, _ = create_pivot_table({cmwrapper: result})
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


if __name__ == "__main__":
    main()
