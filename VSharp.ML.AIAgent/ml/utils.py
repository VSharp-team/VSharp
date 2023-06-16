import torch
from torch import nn

import ml.models
from common.constants import DEVICE


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
    assert len(last_layer_weights) == 8
    model = ml.models.StateModelEncoder(hidden_channels=64, out_channels=8)
    model.load_state_dict(torch.load(path), strict=False)
    last = model.state_encoder.lin
    new_layer = nn.Linear(in_features=8, out_features=1)
    new_layer.weight.data = torch.Tensor([last_layer_weights])
    modified_last = nn.Sequential(last, new_layer)
    model.state_encoder.lin = modified_last
    model.to(DEVICE)
    model.eval()
    return model
