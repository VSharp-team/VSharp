from pathlib import Path


class Constant:
    class Loggers:
        AGENT_LOGGER = "agent_logger"
        ML_LOGGER = "ml_logger"
        TABLE_LOGGER = "table_logger"

    DEFAULT_GAMESERVER_URL = "ws://0.0.0.0:8080/gameServer"
    IMPORTED_FULL_MODEL_PATH = Path("ml/imported/GNN_state_pred_het_full")
    LOG_DIR = Path("logger")
