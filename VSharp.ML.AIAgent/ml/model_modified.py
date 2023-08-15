import torch
from torch.nn import Linear
from torch_geometric.nn import Linear

from learning.timer.wrapper import timeit

from .models import StateGNNEncoderConvEdgeAttr


class StateGNNEncoderConvEdgeAttrExport(StateGNNEncoderConvEdgeAttr):
    def __init__(self, hidden_channels, out_channels):
        super().__init__(hidden_channels, out_channels)
        self.lin_last = Linear(out_channels, 1)

    def forward(self, x_dict, edge_index_dict, edge_attr=None):
        return self.lin_last(super().forward(x_dict, edge_index_dict, edge_attr))


class StateModelEncoderExport(torch.nn.Module):
    def __init__(self, hidden_channels, out_channels):
        super().__init__()
        self.state_encoder = StateGNNEncoderConvEdgeAttrExport(
            hidden_channels, out_channels
        )

    @timeit
    def forward(self, x_dict, edge_index_dict, edge_attr=None):
        z_dict = {}
        z_dict["state_vertex"] = self.state_encoder(x_dict, edge_index_dict, edge_attr)
        z_dict["game_vertex"] = x_dict["game_vertex"]
        return z_dict
