import logging
import os
from pathlib import Path

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
)
from ml.common_model.wrapper import CommonModelWrapper, BestModelsWrapper
from ml.fileop import save_model
from ml.common_model.paths import common_models_path, best_models_dict_path
from ml.model_wrappers.protocols import Predictor
from ml.utils import load_model
import numpy as np


LOG_PATH = Path("./ml_app.log")
TABLES_PATH = Path("./ml_tables.log")
COMMON_MODELS_PATH = Path(common_models_path)
BEST_MODELS_DICT = Path(best_models_dict_path)

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


def create_file(file: Path):
    open(file, "w").close()


def append_to_file(file: Path, s: str):
    with open(file, "a") as file:
        file.write(s)


def main():
    lr = 0.000001
    epochs = 20
    hidden_channels = 32
    num_gv_layers = 2
    num_sv_layers = 2
    print(GeneralConfig.DEVICE)
    # model = CommonModel(hidden_channels, num_gv_layers, num_sv_layers)
    # model.forward(*ml.onnx.onnx_import.create_torch_dummy_input())
    # path = os.path.join(
    #     common_models_path,
    #     "1",
    # )
    # path_to_model = os.path.join(
    #     "ml",
    #     "pretrained_models",
    #     "-262.75775990410693.pth",
    # )
    path_to_model = os.path.join(
        common_models_path,
        "1",
    )

    model = load_model(Path(path_to_model), model=GeneralConfig.EXPORT_MODEL_INIT())
    model.to(GeneralConfig.DEVICE)

    # for name, param in model.named_parameters():
    #     if "lin_last" not in name:
    #         param.requires_grad = False

    create_file(TABLES_PATH)
    create_file(LOG_PATH)
    optimizer = torch.optim.Adam(model.parameters(), lr=lr)
    criterion = euclidean_dist
    # best_models_dict = csv2best_models()
    path_to_best_models_dict = os.path.join(
        best_models_dict_path,
        "1",
    )
    best_models_dict = load_best_models_dict(path_to_best_models_dict)
    bmwrapper = BestModelsWrapper(model, best_models_dict, optimizer, criterion)
    cmwrapper = CommonModelWrapper(model, best_models_dict)

    for epoch in range(epochs):
        # training
        play_game(
            with_predictor=bmwrapper,
            max_steps=GeneralConfig.MAX_STEPS,
            maps_type=MapsType.TRAIN,
        )
        # validation
        cmwrapper.make_copy(str(epoch + 1))
        result = play_game(
            with_predictor=cmwrapper,
            max_steps=GeneralConfig.MAX_STEPS,
            maps_type=MapsType.TRAIN,
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

        path_to_model = os.path.join(common_models_path, str(epoch + 1))
        save_model(model=cmwrapper.model(), to=Path(path_to_model))
        path_to_best_models_dict = os.path.join(
            best_models_dict_path, str(epoch + 1) + ".csv"
        )
        save_best_models2csv(best_models_dict, path_to_best_models_dict)


if __name__ == "__main__":
    main()
