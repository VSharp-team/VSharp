import copy
import random

import numpy
import pygad.torchga
import torch
from torch import nn

import ml.models
from common.constants import BASE_NN_OUT_FEATURES_NUM, DEVICE, Constant


def load_full_model(path: str):
    return torch.load(path)


def load_model(path: str) -> torch.nn.Module:
    model = ml.models.StateModelEncoder(hidden_channels=64, out_channels=8)
    model.load_state_dict(torch.load(path), strict=False)
    model.to(DEVICE)
    model.eval()
    return model


def load_model_with_last_layer(
    path: str, last_layer_weights: list[float]
) -> torch.nn.Module:
    assert len(last_layer_weights) == BASE_NN_OUT_FEATURES_NUM
    model = ml.models.StateModelEncoder(
        hidden_channels=64, out_channels=BASE_NN_OUT_FEATURES_NUM
    )
    model.load_state_dict(torch.load(path), strict=False)
    last = model.state_encoder.lin
    new_layer = nn.Linear(in_features=BASE_NN_OUT_FEATURES_NUM, out_features=1)
    new_layer.weight.data = torch.Tensor([last_layer_weights])
    modified_last = nn.Sequential(last, new_layer)
    model.state_encoder.lin = modified_last
    model.to(DEVICE)
    model.eval()
    return model


def model_weights_with_random_last_layer(
    low, hi, model_load_path=Constant.IMPORTED_DICT_MODEL_PATH
):
    model = load_model_with_last_layer(
        model_load_path,
        [random.uniform(low, hi) for _ in range(BASE_NN_OUT_FEATURES_NUM)],
    )
    model_weights_vector = pygad.torchga.model_weights_as_vector(model=model)
    return model_weights_vector


def random_model_weights(low, hi, model_load_path=Constant.IMPORTED_DICT_MODEL_PATH):
    init_model = load_model_with_last_layer(
        model_load_path, [1 for _ in range(BASE_NN_OUT_FEATURES_NUM)]
    )
    model_weights_vector = pygad.torchga.model_weights_as_vector(model=init_model)
    net_weights = copy.deepcopy(model_weights_vector)
    net_weights = numpy.array(net_weights) + numpy.random.uniform(
        low=low, high=hi, size=model_weights_vector.size
    )

    return net_weights


def create_model_from_weights_vector(weights: list[float]):
    model = load_model_with_last_layer(
        Constant.IMPORTED_DICT_MODEL_PATH, [1 for _ in range(BASE_NN_OUT_FEATURES_NUM)]
    )

    state_dict = pygad.torchga.model_weights_as_dict(
        model=model, weights_vector=weights
    )
    model.load_state_dict(state_dict)

    return model
