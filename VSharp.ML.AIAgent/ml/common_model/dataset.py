from collections.abc import Sequence
import torch
from torch_geometric.data import Dataset
import os
from ml.data_loader_compact import ServerDataloaderHeteroVector
from config import GeneralConfig
import logging


class FullDataset(Dataset):
    def __init__(
        self,
        root,
        best_models: dict,
        data_dict: dict,
        transform=None,
        pre_transform=None,
        pre_filter=None,
    ):
        super().__init__(root, transform, pre_transform, pre_filter)
        self.data_list = []
        self.best_models = best_models
        self.single_map_data = []
        self.data_dict = data_dict

    @property
    def raw_file_names(self):
        return []

    @property
    def processed_file_names(self):
        return os.listdir(self.processed_dir)

    def indices(self) -> Sequence:
        return self.processed_file_names

    def download(self):
        pass

    def save_map_data(self, map_name):
        torch.save(
            self.single_map_data, os.path.join(self.processed_dir, map_name + ".pt")
        )
        self.single_map_data = []
        self.data_dict = self.single_map_data

    def process_single_input(self, input, nn_output):
        hetero_input, state_map = ServerDataloaderHeteroVector.convert_input_to_tensor(
            input
        )
        hetero_input.to(GeneralConfig.DEVICE)
        if not nn_output["state_vertex"].isnan().any():
            hetero_input["y_true"] = nn_output
            self.single_map_data.append(hetero_input)
        else:
            state = nn_output["state_vertex"]
            logging.warning(f"Model's output contains NaN. {state}")

    def len(self):
        return len(self.processed_file_names)

    def get(self, map_name):
        data = torch.load(
            os.path.join(self.processed_dir, map_name), map_location="cpu"
        )
        return data, map_name
