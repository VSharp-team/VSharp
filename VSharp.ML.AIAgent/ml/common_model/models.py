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
        num_gv_hops=3,
        num_sv_hops=3,
    ):
        super().__init__()
        self.tag_conv1 = TAGConv(5, hidden_channels, num_gv_hops)
        self.tag_conv2 = TAGConv(6, hidden_channels, num_sv_hops)
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

    def forward(
        self,
        game_x,
        state_x,
        edge_index_v_v,
        edge_type_v_v,
        edge_index_history_v_s,
        edge_attr_history_v_s,
        edge_index_in_v_s,
        edge_index_s_s,
    ):
        game_x = self.gv_layers[0](
            game_x,
            edge_index_v_v,
        ).relu()
        for layer in self.gv_layers[1:]:
            game_x = layer(
                game_x,
                edge_index_v_v,
            ).relu()

        state_x = self.sv_layers[0](
            state_x,
            edge_index_s_s,
        ).relu()
        for layer in self.sv_layers[1:]:
            state_x = layer(
                state_x,
                edge_index_s_s,
            ).relu()

        history_x = self.history1(
            (game_x, state_x),
            edge_index_history_v_s,
            edge_attr_history_v_s,
            size=(game_x.size(0), state_x.size(0)),
        ).relu()

        in_x = self.in1((game_x, history_x), edge_index_in_v_s).relu()

        state_x = self.sv_layers2[0](
            in_x,
            edge_index_s_s,
        ).relu()
        for layer in self.sv_layers2[1:]:
            state_x = layer(
                state_x,
                edge_index_s_s,
            ).relu()
        x = self.mlp(state_x)
        return x


class ParallelBlocks(torch.nn.Module):
    def __init__(self, models_list, mlp_list):
        super().__init__()
        self.models_list = models_list
        self.mlp = MLP(len(models_list), mlp_list)

    def forward(
        self,
        game_x,
        state_x,
        edge_index_v_v,
        edge_type_v_v,
        edge_index_history_v_s,
        edge_attr_history_v_s,
        edge_index_in_v_s,
        edge_index_s_s,
    ):
        results_list = []
        for model in self.models_list:
            results_list.append(
                model(
                    game_x,
                    state_x,
                    edge_index_v_v,
                    edge_type_v_v,
                    edge_index_history_v_s,
                    edge_attr_history_v_s,
                    edge_index_in_v_s,
                    edge_index_s_s,
                )
            )
        results_tensor = torch.cat(results_list, dim=1)
        return results_tensor
