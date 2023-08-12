import copy
import random
from pathlib import Path
from typing import OrderedDict

import numpy
import pygad.torchga
import torch
from numpy import typing as npt
from torch import nn

from common.constants import BASE_NN_OUT_FEATURES_NUM
from config import GeneralConfig


def load_model(path: Path, model: torch.nn.Module):
    model.load_state_dict(torch.load(path))
    model.eval()
    return model


def convert_to_export(
    old_sd: OrderedDict, new_sd: OrderedDict, last_layer_weights: list[float]
):
    for key, value in old_sd.items():
        new_sd.update({key: value})

    new_model = GeneralConfig.EXPORT_MODEL_INIT()
    new_model.load_state_dict(new_sd, strict=False)
    new_model.state_encoder.lin_last.weight.data = torch.Tensor([last_layer_weights])
    new_model.state_encoder.lin_last.bias.data = torch.Tensor([0])
    return new_model


def model_weights_with_last_layer(
    old_sd: OrderedDict, new_sd: OrderedDict, last_layer_weights: list[float]
) -> npt.NDArray:
    assert len(last_layer_weights) == BASE_NN_OUT_FEATURES_NUM
    model_2_export = convert_to_export(old_sd, new_sd, last_layer_weights)
    return pygad.torchga.model_weights_as_vector(model_2_export)


def model_weights_with_random_last_layer(
    lo: float, hi: float, old_sd: OrderedDict, new_sd: OrderedDict
) -> npt.NDArray:
    weights = model_weights_with_last_layer(
        old_sd,
        new_sd,
        last_layer_weights=[
            random.uniform(lo, hi) for _ in range(BASE_NN_OUT_FEATURES_NUM)
        ],
    )
    return weights


def create_population(
    lo: float, hi: float, model: nn.Module, population_size: int
) -> list[npt.NDArray]:
    model_weights_vector = pygad.torchga.model_weights_as_vector(model)

    net_population_weights = []
    net_population_weights.append(model_weights_vector)

    for idx in range(population_size - 1):
        net_weights = copy.deepcopy(model_weights_vector)
        net_weights = numpy.array(net_weights) + numpy.random.uniform(
            low=lo, high=hi, size=model_weights_vector.size
        )

        net_population_weights.append(net_weights)

    return net_population_weights
