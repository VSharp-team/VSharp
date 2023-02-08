import torch


def load_model(target_model: torch.nn.Module, path: str) -> torch.nn.Module:
    target_model.load_state_dict(torch.load(path))
    target_model.eval()  # to set dropout and batch normalization layers to evaluation mode before running inference
    return target_model
