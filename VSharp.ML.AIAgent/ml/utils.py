import torch
import ml.models


def load_full_model(path: str):
    return torch.load(path)


def load_model(path: str) -> torch.nn.Module:
    model = ml.models.StateModelEncoder(hidden_channels=64, out_channels=8)
    model.load_state_dict(torch.load(path), strict=False)
    model.eval()
    return model
