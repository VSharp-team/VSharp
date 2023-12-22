import ast
import csv
import os
import re
import typing as t
from pathlib import Path

import numpy as np
import torch

from config import GeneralConfig
from ml.common_model.paths import COMMON_MODELS_PATH, CSV_PATH, MODELS_PATH
from ml.utils import load_model


def euclidean_dist(y_pred, y_true):
    if len(y_pred) > 1:
        y_pred_min, ind1 = torch.min(y_pred, dim=0)
        y_pred_norm = y_pred - y_pred_min

        y_true_min, ind1 = torch.min(y_true, dim=0)
        y_true_norm = y_true - y_true_min
        return torch.sqrt(torch.sum((y_pred_norm - y_true_norm) ** 2))
    else:
        return 0


def get_last_epoch_num(path):
    epochs = list(map(lambda x: re.findall("[0-9]+", x), os.listdir(path)))
    return str(sorted(epochs)[-1][0])


def get_tuple_for_max(t):
    values_list = list(t)
    values_list[1] *= -1
    values_list[3] *= -1
    return tuple(values_list)


def csv2best_models(ref_model_init: t.Callable[[], torch.nn.Module]):
    best_models = {}
    for epoch_num in range(1, len(os.listdir(CSV_PATH)) + 1):
        path_to_csv = os.path.join(CSV_PATH, str(epoch_num) + ".csv")
        with open(path_to_csv, "r") as csv_file:
            csv_reader = csv.reader(csv_file)
            map_names = next(csv_reader)[1:]
            models = []
            for row in csv_reader:
                models_stat = dict()
                int_row = list(
                    map(lambda x: get_tuple_for_max(ast.literal_eval(x)), row[1:])
                )
                for i in range(len(int_row)):
                    models_stat[map_names[i]] = int_row[i]
                models.append((row[0], models_stat))
            for map_name in map_names:
                best_model = max(models, key=(lambda m: m[1][map_name]))
                best_model_name, best_model_score = best_model[0], best_model[1]
                path_to_model = os.path.join(
                    MODELS_PATH,
                    "epoch_" + str(epoch_num),
                    best_model_name + ".pth",
                )
                ref_model = load_model(Path(path_to_model), model=ref_model_init())

                ref_model.to(GeneralConfig.DEVICE)
                best_models[map_name] = (
                    ref_model,
                    best_model_score[map_name],
                    best_model_name,
                )
    return best_models


def back_prop(best_model, model, data, optimizer, criterion):
    model.train()
    data.to(GeneralConfig.DEVICE)
    optimizer.zero_grad()
    out = model(data.x_dict, data.edge_index_dict, data.edge_attr_dict)["state_vertex"]
    y_true = best_model(data.x_dict, data.edge_index_dict, data.edge_attr_dict)
    if type(y_true) is dict:
        y_true = y_true["state_vertex"]
    if abs(torch.min(y_true)) > 1:
        y_true = 1 / y_true
    loss = criterion(out, y_true)
    if loss == 0:
        return 0
    loss.backward()
    optimizer.step()
    return loss


def save_best_models2csv(best_models: dict, path):
    values_for_csv = []
    for map_name in best_models.keys():
        values_for_csv.append(
            {
                "map_name": map_name,
                "best_model_name": best_models[map_name][2],
                "result": best_models[map_name][1],
            }
        )
    with open(path, "w") as csv_file:
        writer = csv.DictWriter(
            csv_file, fieldnames=["map_name", "best_model_name", "result"]
        )
        writer.writerows(values_for_csv)


def load_best_models_dict(path, model_init: t.Callable[[], torch.nn.Module]):
    best_models = csv2best_models()
    with open(path, "r") as csv_file:
        csv_reader = csv.reader(csv_file)
        for row in csv_reader:
            if row[1] != best_models[row[0]][2]:
                path_to_model = os.path.join(COMMON_MODELS_PATH, row[1])
                ref_model = load_model(Path(path_to_model), model=model_init())
                ref_model.load_state_dict(torch.load(path_to_model))
                ref_model.to(GeneralConfig.DEVICE)
                best_models[row[0]] = (ref_model, ast.literal_eval(row[2]), row[1])
    return best_models


def load_dataset_state_dict(path):
    dataset_state_dict = {}
    with open(path, "r") as csv_file:
        csv_reader = csv.reader(csv_file)
        for row in csv_reader:
            dataset_state_dict[row[0]] = ast.literal_eval(row[1])
    return dataset_state_dict


def get_model(path_to_weights: Path, model_init: t.Callable[[], torch.nn.Module]):
    model = model_init()
    weights = torch.load(path_to_weights)
    weights["lin_last.weight"] = torch.tensor(np.random.random([1, 8]))
    weights["lin_last.bias"] = torch.tensor(np.random.random([1]))
    model.load_state_dict(weights)
    return model
