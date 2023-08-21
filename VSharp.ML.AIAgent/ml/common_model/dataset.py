from collections.abc import Sequence
import torch
from torch_geometric.data import Dataset
import os
from ml.data_loader_compact import ServerDataloaderHeteroVector
from config import GeneralConfig


class FullDataset(Dataset):
    def __init__(
        self,
        root,
        best_models: dict,
        transform=None,
        pre_transform=None,
        pre_filter=None,
    ):
        super().__init__(root, transform, pre_transform, pre_filter)
        self.data_list = []
        self.best_models = best_models
        self.single_map_data = []

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

    def process_single_input(self, map_name, input):
        hetero_input, state_map = ServerDataloaderHeteroVector.convert_input_to_tensor(
            input
        )
        hetero_input.to(GeneralConfig.DEVICE)
        y_true = self.best_models[map_name][0](
            hetero_input.x_dict,
            hetero_input.edge_index_dict,
            hetero_input.edge_attr_dict,
        )
        hetero_input["y_true"] = y_true

        self.single_map_data.append(hetero_input)

    def len(self):
        return len(self.processed_file_names)

    def get(self, map_name):
        data = torch.load(
            os.path.join(self.processed_dir, map_name), map_location="cpu"
        )
        return data
