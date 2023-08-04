from pathlib import Path

import torch

from config import BrokerConfig


def _build_bar_format() -> str:
    custom_left = "{desc}: {n_fmt}/{total_fmt}"
    custom_bar = "{percentage:3.0f}% [{bar}]"
    custom_info = "{elapsed}<{remaining}, {rate_fmt}{postfix}"

    return f"{custom_left} {custom_bar} - {custom_info}"


IMPORTED_FULL_MODEL_PATH = Path(
    "ml/imported/GNN_state_pred_het_full_TAGConv_20e_2xAll_10h"
)
IMPORTED_DICT_MODEL_PATH = Path(
    "ml/imported/GNN_state_pred_het_dict_TAGConv_20e_2xAll_10h"
)

BASE_REPORT_DIR = Path("./report")
TABLES_LOG_FILE = BASE_REPORT_DIR / "tables.log"
LEADERS_TABLES_LOG_FILE = BASE_REPORT_DIR / "leaders.log"
EPOCH_BEST_DIR = BASE_REPORT_DIR / "epochs_best"
APP_LOG_FILE = Path("app.log")

TQDM_FORMAT_DICT = {
    "unit": "game",
    "bar_format": _build_bar_format(),
    "dynamic_ncols": True,
}


class WebsocketSourceLinks:
    GET_WS = f"http://0.0.0.0:{BrokerConfig.BROKER_PORT}/get_ws"
    POST_WS = f"http://0.0.0.0:{BrokerConfig.BROKER_PORT}/post_ws"


class ResultsHandlerLinks:
    POST_RES = f"http://0.0.0.0:{BrokerConfig.BROKER_PORT}/send_res"
    GET_RES = f"http://0.0.0.0:{BrokerConfig.BROKER_PORT}/recv_res"


DUMMY_INPUT_PATH = Path("ml/onnx/dummy_input.json")
BEST_MODEL_ONNX_SAVE_PATH = Path("ml/onnx/StateModelEncoder.onnx")
TEMP_EPOCH_INFERENCE_TIMES_DIR = Path(".epoch_inference_times/")

# assuming we start from /VSharp/VSharp.ML.AIAgent
SERVER_WORKING_DIR = "../VSharp.ML.GameServer.Runner/bin/Release/net6.0/"
