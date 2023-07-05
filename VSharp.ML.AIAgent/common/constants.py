from pathlib import Path

import torch


def _build_bar_format() -> str:
    custom_left = "{desc}: {n_fmt}/{total_fmt}"
    custom_bar = "{percentage:3.0f}% [{bar}]"
    custom_info = "{elapsed}<{remaining}, {rate_fmt}{postfix}"

    return f"{custom_left} {custom_bar} - {custom_info}"


class Constant:
    IMPORTED_FULL_MODEL_PATH = Path(
        "ml/imported/GNN_state_pred_het_full_TAGConv_20e_2xAll_10h"
    )
    IMPORTED_DICT_MODEL_PATH = Path(
        "ml/imported/GNN_state_pred_het_dict_TAGConv_20e_2xAll_10h"
    )
    NUM_FEATURES = 8
    TABLES_LOG_FILE = Path("./tables.log")
    APP_LOG_FILE = Path("./app.log")
    TQDM_FORMAT_DICT = {
        "unit": "game",
        "bar_format": _build_bar_format(),
        "dynamic_ncols": True,
    }


DEVICE = torch.device("cuda:0" if torch.cuda.is_available() else "cpu")

BROKER_SERVER_PORT = 8080
VSHARP_INSTANCES_START_PORT = 8100
MAX_STEPS = 500
BASE_NN_OUT_FEATURES_NUM = 8


class WebsocketSourceLinks:
    GET_WS = f"http://0.0.0.0:{BROKER_SERVER_PORT}/get_ws"
    POST_WS = f"http://0.0.0.0:{BROKER_SERVER_PORT}/post_ws"


class ResultsHandlerLinks:
    POST_RES = f"http://0.0.0.0:{BROKER_SERVER_PORT}/send_res"
    GET_RES = f"http://0.0.0.0:{BROKER_SERVER_PORT}/recv_res"


DUMMY_INPUT_PATH = "ml/onnx/dummy_input.json"
BEST_MODEL_ONNX_SAVE_PATH = "ml/onnx/StateModelEncoder.onnx"
