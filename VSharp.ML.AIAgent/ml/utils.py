import torch
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
