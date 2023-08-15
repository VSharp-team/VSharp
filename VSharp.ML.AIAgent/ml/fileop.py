from pathlib import Path

import pygad
import torch

from config import GeneralConfig
from ml.onnx.onnx_import import create_torch_dummy_input


def save_model(model: torch.nn.Module, to: Path, weights=None):
    if weights is None:
        torch.save(model.state_dict(), to)
    else:
        model.forward(*create_torch_dummy_input())
        state_dict = pygad.torchga.model_weights_as_dict(model, weights)
        torch.save(state_dict, to)


def load_model_from_file(model: torch.nn.Module, file: Path):
    model.load_state_dict(torch.load(file))
    model.to(GeneralConfig.DEVICE)
    model.eval()
    return model
