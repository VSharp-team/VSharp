import copy
import random

import numpy
import pygad.torchga
import torch
from numpy import typing as npt
from torch import nn

import ml.model_modified
import ml.models
from common.constants import BASE_NN_OUT_FEATURES_NUM
from config import GeneralConfig
from ml.onnx.onnx_import import create_torch_dummy_input


def model_weights_with_last_layer(
    last_layer_weights: list[float], model: ml.models.StateGNNEncoderConvEdgeAttr
) -> npt.NDArray:
    assert len(last_layer_weights) == BASE_NN_OUT_FEATURES_NUM
    model_2_export = ml.model_modified.StateGNNEncoderConvEdgeAttrExport(
        hidden_channels=64, out_channels=BASE_NN_OUT_FEATURES_NUM
    )
    model_2_export.forward(*create_torch_dummy_input())
    model_2_export.load_state_dict(model.state_dict(), strict=False)
    model_2_export.lin_last.weight.data = torch.Tensor([last_layer_weights])
    model_2_export.to(GeneralConfig.DEVICE)
    model_2_export.eval()
    return pygad.torchga.model_weights_as_vector(model_2_export)


def model_weights_with_random_last_layer(
    lo: float, hi: float, model: ml.models.StateGNNEncoderConvEdgeAttr
) -> npt.NDArray:
    model = model_weights_with_last_layer(
        [random.uniform(lo, hi) for _ in range(BASE_NN_OUT_FEATURES_NUM)], model
    )
    return model


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
