import typing as t
from datetime import datetime
from pathlib import Path

import torch
from torch import Tensor


class PathSelectorNNProtocol(t.Protocol):
    def forward(
        self,
        game_x,
        state_x,
        edge_index_v_v,
        edge_index_history_v_s,
        edge_attr_history_v_s,
        edge_index_in_v_s,
        edge_index_s_s,
    ) -> Tensor:
        ...


def save_model(model: torch.nn.Module, /, **initargs):
    weights = model.state_dict()

    # ml.models.TAGSageSimple.model
    save_path_components = model.__module__.split(".")[:-1]

    # ml.models.TAGSageSimple.model.StateModelEncoder
    class_fullname = model.__module__ + "." + model.__class__.__name__

    # **{hidden_channels: 32, out_channels: 8}
    model_initargs = "_".join([f"{param}_{value}" for param, value in initargs.items()])

    save_dir = Path("/".join(save_path_components))

    timestamp = datetime.fromtimestamp(datetime.now().timestamp())

    suffix = ".pt"

    save_name = f"{class_fullname}{'_' + model_initargs + '_' if initargs else ''}{timestamp}{suffix}"

    torch.save(weights, save_dir / save_name)
