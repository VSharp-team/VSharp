import os

csv_path = os.path.join("report", "epochs_tables")
models_path = os.path.join("report", "epochs_best")
common_models_path = os.path.join("report", "common_models")
best_models_dict_path = os.path.join("report", "updated_best_models_dicts")
dataset_root_path = os.path.join("report", "dataset")
dataset_map_results_file_name = os.path.join("report", "dataset_state.csv")
training_data_path = os.path.join("report", "run_tables")

path_to_models_for_parallel_architecture = os.path.join(
    "ml", "pretrained_models", "models_for_parallel_architecture"
)
