from pathlib import Path


class Constant:
    DEFAULT_GAMESERVER_URL = "ws://0.0.0.0:8080/gameServer"
    IMPORTED_FULL_MODEL_PATH = Path("ml/imported/GNN_state_pred_het_full")
    TABLES_LOG_FILE = Path("./tables.log")
