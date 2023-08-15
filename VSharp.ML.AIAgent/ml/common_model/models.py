import torch
from torch import nn
from torch_geometric.nn import (
    GATConv,
    SAGEConv,
    TAGConv,
)
from torchvision.ops import MLP


class CommonModel(torch.nn.Module):
    def __init__(
        self,
        hidden_channels,
        num_gv_layers=2,
        num_sv_layers=2,
    ):
        super().__init__()
        self.tag_conv1 = TAGConv(5, hidden_channels, 2)
        self.tag_conv2 = TAGConv(6, hidden_channels, 3)
        self.gv_layers = nn.ModuleList()
        self.gv_layers.append(self.tag_conv1)
        self.gv_layers.append(SAGEConv(-1, hidden_channels))
        for i in range(num_gv_layers - 1):
            sage_gv = SAGEConv(-1, hidden_channels)
            self.gv_layers.append(sage_gv)

        self.sv_layers = nn.ModuleList()
        self.sv_layers.append(self.tag_conv2)
        self.sv_layers.append(SAGEConv(-1, hidden_channels))
        for i in range(num_sv_layers - 1):
            sage_sv = SAGEConv(-1, hidden_channels)
            self.sv_layers.append(sage_sv)

        self.history1 = GATConv((-1, -1), hidden_channels, add_self_loops=False)

        self.in1 = SAGEConv((-1, -1), hidden_channels)

        self.sv_layers2 = nn.ModuleList()
        self.sv_layers2.append(SAGEConv(-1, hidden_channels))
        for i in range(num_sv_layers - 1):
            sage_sv = SAGEConv(-1, hidden_channels)
            self.sv_layers2.append(sage_sv)

        self.mlp = MLP(hidden_channels, [1])

    def forward(self, x_dict, edge_index_dict, edge_attr_dict):
        game_x = self.gv_layers[0](
            x_dict["game_vertex"],
            edge_index_dict[("game_vertex", "to", "game_vertex")],
        ).relu()
        for layer in self.gv_layers[1:]:
            game_x = layer(
                game_x,
                edge_index_dict[("game_vertex", "to", "game_vertex")],
            ).relu()

        state_x = self.sv_layers[0](
            x_dict["state_vertex"],
            edge_index_dict[("state_vertex", "parent_of", "state_vertex")],
        ).relu()
        for layer in self.sv_layers[1:]:
            state_x = layer(
                state_x,
                edge_index_dict[("state_vertex", "parent_of", "state_vertex")],
            ).relu()

        history_x = self.history1(
            (game_x, state_x),
            edge_index_dict[("game_vertex", "history", "state_vertex")],
            edge_attr_dict,
            size=(game_x.size(0), state_x.size(0)),
        ).relu()

        in_x = self.in1(
            (game_x, history_x), edge_index_dict[("game_vertex", "in", "state_vertex")]
        ).relu()

        state_x = self.sv_layers2[0](
            in_x,
            edge_index_dict[("state_vertex", "parent_of", "state_vertex")],
        ).relu()
        for layer in self.sv_layers2[1:]:
            state_x = layer(
                state_x,
                edge_index_dict[("state_vertex", "parent_of", "state_vertex")],
            ).relu()
        x = self.mlp(in_x)
        return x
