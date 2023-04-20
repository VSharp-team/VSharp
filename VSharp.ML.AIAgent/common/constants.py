from pathlib import Path


def _build_bar_format() -> str:
    custom_left = "{desc}: {n_fmt}/{total_fmt}"
    custom_bar = "{percentage:3.0f}% [{bar}]"
    custom_info = "{elapsed}<{remaining}, {rate_fmt}{postfix}"

    return f"{custom_left} {custom_bar} - {custom_info}"


class Constant:
    DEFAULT_GAMESERVER_URL = "ws://0.0.0.0:8080/gameServer"
    IMPORTED_FULL_MODEL_PATH = Path("ml/imported/GNN_state_pred_het_full")
    TABLES_LOG_FILE = Path("./tables.log")
    TQDM_FORMAT_DICT = {
        "unit": "game",
        "bar_format": _build_bar_format(),
        "dynamic_ncols": True,
    }
