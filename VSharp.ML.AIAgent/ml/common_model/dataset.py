from collections.abc import Sequence
import torch
from torch_geometric.data import Dataset
from pathlib import Path

import os

import tqdm
from ml.data_loader_compact import ServerDataloaderHeteroVector
from config import GeneralConfig
import logging
import copy
from ml.common_model.utils import csv2best_models, load_dataset_state_dict
import csv

# from ray.experimental.tqdm_ray import tqdm


class FullDataset:
    def __init__(self, dataset_root_path, dataset_map_results_file_name):
        self.dataset_map_results_file_name = dataset_map_results_file_name
        self.dataset_root_path = dataset_root_path
        self.maps_data = dict()

    def load(self):
        maps_results = load_dataset_state_dict(self.dataset_map_results_file_name)
        for file_with_map_steps in tqdm.tqdm(
            os.listdir(self.dataset_root_path), desc="data loading"
        ):
            map_data = torch.load(
                os.path.join(self.dataset_root_path, file_with_map_steps),
                map_location="cpu",  # maybe store on cpu?
            )
            map_name = file_with_map_steps[:-3]
            single_map_steps = []
            for i in map_data:
                if i["y_true"]["state_vertex"].size()[0] != 1:
                    if not i["y_true"]["state_vertex"].isnan().any():
                        max_ind = torch.argmax(i["y_true"]["state_vertex"])
                        i["y_true"]["state_vertex"] = torch.zeros_like(
                            i["y_true"]["state_vertex"]
                        )
                        i["y_true"]["state_vertex"][max_ind] = 1.0
                        single_map_steps.append(i)
            self.maps_data[map_name] = (maps_results[map_name], single_map_steps)

    def get_plain_data(self):
        result = []
        for _, map_steps in self.maps_data.values():
            result += map_steps
        return result

    def save(self):
        values_for_csv = []
        for map_name in self.maps_data.keys():
            values_for_csv.append(
                {
                    "map_name": map_name,
                    "result": self.maps_data[map_name][0],
                }
            )
            torch.save(
                self.maps_data[map_name][1],
                os.path.join(self.dataset_root_path, map_name + ".pt"),
            )
        with open(self.dataset_map_results_file_name, "w") as csv_file:
            writer = csv.DictWriter(csv_file, fieldnames=["map_name", "result"])
            writer.writerows(values_for_csv)

    def update(self, map_name, map_result: tuple[int, int, int, int], map_steps):
        for x in map_steps:
            x.to("cpu")
        if map_name in self.maps_data.keys():
            if self.maps_data[map_name][0] <= map_result:
                logging.info(
                    f"The model with result = {self.maps_data[map_name][0]} was replaced with the model with "
                    f"result = {map_result} on the map {map_name}"
                )
            self.maps_data[map_name] = (map_result, map_steps)
        else:
            self.maps_data[map_name] = (map_result, map_steps)
