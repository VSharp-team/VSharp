from collections.abc import Sequence
import torch

import os
import numpy as np

import tqdm
import logging
from ml.common_model.utils import load_dataset_state_dict
import csv
from torch_geometric.data import HeteroData
from typing import TypeAlias


MapName: TypeAlias = str
GameStatistics: TypeAlias = tuple[int, int, int, int]
GameStepHeteroData: TypeAlias = HeteroData
GameStepsOnMapInfo: TypeAlias = tuple[GameStatistics, Sequence[GameStepHeteroData]]


class FullDataset:
    def __init__(
        self,
        dataset_root_path,
        dataset_map_results_file_name,
        similar_steps_save_prob=0,
    ):
        self.dataset_map_results_file_name = dataset_map_results_file_name
        self.dataset_root_path = dataset_root_path
        self.maps_data: dict[str, GameStepsOnMapInfo] = dict()
        self.similar_steps_save_prob = similar_steps_save_prob

    def load(self):
        maps_results = load_dataset_state_dict(self.dataset_map_results_file_name)
        for file_with_map_steps in tqdm.tqdm(
            os.listdir(self.dataset_root_path), desc="data loading"
        ):
            map_steps = torch.load(
                os.path.join(self.dataset_root_path, file_with_map_steps),
                map_location="cpu",
            )
            map_name = file_with_map_steps[:-3]
            filtered_map_steps = self.filter_map_steps(map_steps)
            filtered_map_steps = self.remove_similar_steps(filtered_map_steps)
            self.maps_data[map_name] = (maps_results[map_name], filtered_map_steps)

    def remove_similar_steps(self, map_steps):
        filtered_map_steps = []
        for step in map_steps:
            if (
                len(filtered_map_steps) != 0
                and step["y_true"].size() == filtered_map_steps[-1]["y_true"].size()
            ):
                cos_d = 1 - torch.sum(
                    (step["y_true"] / torch.linalg.vector_norm(step["y_true"]))
                    * (
                        filtered_map_steps[-1]["y_true"]
                        / torch.linalg.vector_norm(filtered_map_steps[-1]["y_true"])
                    )
                )
                if (
                    cos_d < 1e-7
                    and step["game_vertex"]["x"].size()[0]
                    == filtered_map_steps[-1]["game_vertex"]["x"].size()[0]
                ):
                    step.use_for_train = np.random.choice(
                        [True, False],
                        p=[
                            self.similar_steps_save_prob,
                            1 - self.similar_steps_save_prob,
                        ],
                    )
                else:
                    step.use_for_train = True
            else:
                step.use_for_train = True
            filtered_map_steps.append(step)
        return filtered_map_steps

    def filter_map_steps(self, map_steps):
        filtered_map_steps = []
        for step in map_steps:
            if step["y_true"].size()[0] != 1 and not step["y_true"].isnan().any():
                max_ind = torch.argmax(step["y_true"])
                step["y_true"] = torch.zeros_like(step["y_true"])
                step["y_true"][max_ind] = 1.0
                filtered_map_steps.append(step)
        return filtered_map_steps

    def get_plain_data(self):
        result = []
        for _, map_steps in self.maps_data.values():
            for step in map_steps:
                if step.use_for_train:
                    result.append(step)
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

    def update(
        self,
        map_name,
        map_result: tuple[int, int, int, int],
        map_steps,
        move_to_cpu=False,
    ):
        if move_to_cpu:
            for x in map_steps:
                x.to("cpu")
        filtered_map_steps = self.filter_map_steps(map_steps)
        if map_name in self.maps_data.keys():
            if self.maps_data[map_name][0] <= map_result:
                logging.info(
                    f"The model with result = {self.maps_data[map_name][0]} was replaced with the model with "
                    f"result = {map_result} on the map {map_name}"
                )
                filtered_map_steps = self.remove_similar_steps(filtered_map_steps)
                self.maps_data[map_name] = (map_result, filtered_map_steps)
        else:
            filtered_map_steps = self.remove_similar_steps(filtered_map_steps)
            self.maps_data[map_name] = (map_result, filtered_map_steps)
