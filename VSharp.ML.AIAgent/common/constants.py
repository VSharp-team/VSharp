from pathlib import Path


def _build_bar_format() -> str:
    custom_left = "{desc}: {n_fmt}/{total_fmt}"
    custom_bar = "{percentage:3.0f}% [{bar}]"
    custom_info = "{elapsed}<{remaining}, {rate_fmt}{postfix}"

    return f"{custom_left} {custom_bar} - {custom_info}"


class Constant:
    IMPORTED_FULL_MODEL_PATH = Path("ml/imported/GNN_state_pred_het_full")
    IMPORTED_DICT_MODEL_PATH = Path("ml/imported/GNN_state_pred_het_dict")
    NUM_FEATURES = 8
    TABLES_LOG_FILE = Path("./tables.log")
    APP_LOG_FILE = Path("./app.log")
    TQDM_FORMAT_DICT = {
        "unit": "game",
        "bar_format": _build_bar_format(),
        "dynamic_ncols": True,
    }
