import torch
from torch.nn import Linear
from torch_geometric.nn import Linear

from learning.timer.wrapper import timeit

from .models import (
    StateGNNEncoderConvEdgeAttr,
    StateGNNEncoderConvEdgeAttrCompact,
    StateGNNEncoderConvEdgeAttrGraphConvAll,
)
from torch.nn.functional import softmax


class StateGNNEncoderConvEdgeAttrExport(StateGNNEncoderConvEdgeAttr):
    def __init__(self, hidden_channels, out_channels):
        super().__init__(hidden_channels, out_channels)
        self.lin_last = Linear(out_channels, 1)

    def forward(
        self,
        game_x,
        state_x,
        edge_index_v_v,
        edge_index_history_v_s,
        edge_attr_history_v_s,
        edge_index_in_v_s,
        edge_index_s_s,
    ):
        return softmax(
            self.lin_last(
                super().forward(
                    game_x,
                    state_x,
                    edge_index_v_v,
                    edge_index_history_v_s,
                    edge_attr_history_v_s,
                    edge_index_in_v_s,
                    edge_index_s_s,
                )
            ),
            dim=0,
        )


class StateModelEncoderExport(torch.nn.Module):
    def __init__(self, hidden_channels, out_channels):
        super().__init__()
        self.state_encoder = StateGNNEncoderConvEdgeAttrExport(
            hidden_channels, out_channels
        )

    @timeit
    def forward(
        self,
        game_x,
        state_x,
        edge_index_v_v,
        edge_index_history_v_s,
        edge_attr_history_v_s,
        edge_index_in_v_s,
        edge_index_s_s,
    ):
        z_dict = {}
        z_dict["state_vertex"] = self.state_encoder(
            game_x,
            state_x,
            edge_index_v_v,
            edge_index_history_v_s,
            edge_attr_history_v_s,
            edge_index_in_v_s,
            edge_index_s_s,
        )
        z_dict["game_vertex"] = game_x
        return z_dict


class StateGNNEncoderONNX(StateGNNEncoderConvEdgeAttrGraphConvAll):
    def __init__(self, hidden_channels, out_channels):
        super().__init__(hidden_channels, out_channels)
        self.lin_last = Linear(out_channels, 1)

    def forward(
        self,
        game_x,
        state_x,
        edge_index_v_v,
        edge_index_history_v_s,
        edge_attr_history_v_s,
        edge_index_in_v_s,
        edge_index_s_s,
        edge_type,
    ):
        return softmax(
            self.lin_last(
                super().forward(
                    game_x,
                    state_x,
                    edge_index_v_v,
                    edge_index_history_v_s,
                    edge_attr_history_v_s,
                    edge_index_in_v_s,
                    edge_index_s_s,
                    edge_type,
                )
            ),
            dim=0,
        )


class StateModelEncoderExportONNX(torch.nn.Module):
    def __init__(self, hidden_channels, out_channels):
        super().__init__()
        self.state_encoder = StateGNNEncoderONNX(hidden_channels, out_channels)

    @timeit
    def forward(
        self,
        game_x,
        state_x,
        edge_index_v_v,
        edge_index_history_v_s,
        edge_attr_history_v_s,
        edge_index_in_v_s,
        edge_index_s_s,
        edge_type,
    ):
        z_dict = {}
        z_dict["state_vertex"] = self.state_encoder(
            game_x,
            state_x,
            edge_index_v_v,
            edge_index_history_v_s,
            edge_attr_history_v_s,
            edge_index_in_v_s,
            edge_index_s_s,
            edge_type,
        )
        z_dict["game_vertex"] = game_x
        return z_dict


class StateGNNEncoderConvEdgeAttrExportCompact(StateGNNEncoderConvEdgeAttrCompact):
    def __init__(self, hidden_channels, out_channels):
        super().__init__(hidden_channels, out_channels)
        self.lin_last = Linear(out_channels, 1)

    def forward(self, x_dict, edge_index_dict, edge_attr=None):
        return softmax(
            self.lin_last(super().forward(x_dict, edge_index_dict, edge_attr)), dim=0
        )


class StateModelEncoderExportCompact(torch.nn.Module):
    def __init__(self, hidden_channels, out_channels):
        super().__init__()
        self.state_encoder = StateGNNEncoderConvEdgeAttrExportCompact(
            hidden_channels, out_channels
        )

    @timeit
    def forward(self, x_dict, edge_index_dict, edge_attr=None):
        z_dict = {}
        z_dict["state_vertex"] = self.state_encoder(x_dict, edge_index_dict, edge_attr)
        z_dict["game_vertex"] = x_dict["game_vertex"]
        return z_dict


class StateModelEncoder(torch.nn.Module):
    def __init__(self, hidden_channels, out_channels):
        super().__init__()
        self.state_encoder = StateGNNEncoderConvEdgeAttrGraphConvAll(
            hidden_channels, out_channels
        )

    def forward(self, x_dict, edge_index_dict, edge_attr=None):
        z_dict = {}
        z_dict["state_vertex"] = self.state_encoder(
            x_dict, edge_index_dict, edge_attr, edge_type
        )
        z_dict["game_vertex"] = x_dict["game_vertex"]
        return z_dict
