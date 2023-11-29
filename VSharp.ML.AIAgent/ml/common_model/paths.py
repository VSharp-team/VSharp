import os
import pathlib

CSV_PATH = os.path.join("report", "epochs_tables")
MODELS_PATH = os.path.join("report", "epochs_best")
COMMON_MODELS_PATH = os.path.join("report", "common_models")
BEST_MODELS_DICT_PATH = os.path.join("report", "updated_best_models_dicts")
DATASET_ROOT_PATH = os.path.join("report", "dataset")
DATASET_MAP_RESULTS_FILENAME = os.path.join("report", "dataset_state.csv")
TRAINING_DATA_PATH = os.path.join("report", "run_tables")
PRETRAINED_MODEL_PATH = pathlib.Path("ml/models")

PATH_TO_MODELS_FOR_PARALLEL_ARCHITECTURE = os.path.join(
    "ml", "pretrained_models", "models_for_parallel_architecture"
)
