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
from ml.common_model.utils import csv2best_models, euclidean_dist
from ml.common_model.wrapper import CommonModelWrapper, BestModelsWrapper
from ml.fileop import save_model
from ml.common_model.paths import common_models_path
from ml.model_wrappers.protocols import Predictor
from ml.utils import load_model


LOG_PATH = Path("./ml_app.log")
TABLES_PATH = Path("./ml_tables.log")
COMMON_MODELS_PATH = Path(common_models_path)

logging.basicConfig(
    level=GeneralConfig.LOGGER_LEVEL,
    filename=LOG_PATH,
    filemode="a",
    format="%(asctime)s - p%(process)d: %(name)s - [%(levelname)s]: %(message)s",
)

if not COMMON_MODELS_PATH.exists():
    os.makedirs(common_models_path)


def create_file(file: Path):
    open(file, "w").close()


def append_to_file(file: Path, s: str):
    with open(file, "a") as file:
        file.write(s)


def main():
    lr = 0.0000001
    epochs = 1
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
    path_to_model = os.path.join(
         common_models_path,
        "-262.75775990410693.pth",
    )

    model = load_model(Path(path_to_model), model=GeneralConfig.EXPORT_MODEL_INIT())
    model.to(GeneralConfig.DEVICE)

    create_file(TABLES_PATH)
    create_file(LOG_PATH)
    optimizer = torch.optim.Adam(model.parameters(), lr=lr)
    criterion = euclidean_dist
    best_models_dict = csv2best_models()
    cmwrapper = CommonModelWrapper(model, best_models_dict)
    bmwrapper = BestModelsWrapper(model, best_models_dict, optimizer, criterion)

    for epoch in range(epochs):
        # training
        play_game(
            with_predictor=bmwrapper,
            max_steps=GeneralConfig.MAX_STEPS,
            maps_type=MapsType.TRAIN,
        )
        # validation
        result = play_game(
            with_predictor=cmwrapper,
            max_steps=GeneralConfig.MAX_STEPS,
            maps_type=MapsType.TRAIN,
        )
        table, _, _ = create_pivot_table({cmwrapper: result})
        table = table_to_string(table)
        append_to_file(TABLES_PATH, f"Epoch#{epoch}" + "\n")
        append_to_file(TABLES_PATH, table + "\n")

        path_to_model = os.path.join(common_models_path, str(epoch + 1))
        save_model(model=cmwrapper.model(), to=Path(path_to_model))


if __name__ == "__main__":
    main()
