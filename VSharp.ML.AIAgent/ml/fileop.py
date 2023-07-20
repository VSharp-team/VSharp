from pathlib import Path

import pygad
import torch

import ml


def save_model(model: torch.nn.Module, to: Path, weights=None):
    if weights is None:
        torch.save(model.state_dict(), to)
    else:
        model.forward(*ml.onnx.onnx_import.create_torch_dummy_input())
        state_dict = pygad.torchga.model_weights_as_dict(model, weights)
        torch.save(state_dict, to)


def load_model_from_file(model: torch.nn.Module, file: Path):
    model.load_state_dict(torch.load(file))
    model.eval()
    return model
