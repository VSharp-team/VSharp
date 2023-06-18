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

START_PORT = 8100
SERVER_COUNT = 4

# len(SOCKET_URLS) == proc_num
SOCKET_URLS = [f"ws://0.0.0.0:{START_PORT + i}/gameServer" for i in range(SERVER_COUNT)]

MAX_STEPS = 300
