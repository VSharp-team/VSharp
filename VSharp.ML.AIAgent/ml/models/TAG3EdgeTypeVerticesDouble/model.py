import torch
from torch.nn import Linear
from torch_geometric.nn import TAGConv, SAGEConv, GraphConv, RGCNConv


class StateModelEncoder(torch.nn.Module):
    def __init__(self, hidden_channels, out_channels):
        super().__init__()
        self.conv1 = RGCNConv(-1, hidden_channels, 3)
        self.conv12 = TAGConv(5, hidden_channels, 3)
        self.conv2 = TAGConv(hidden_channels, hidden_channels, 3)
        self.conv3 = GraphConv((-1, -1), hidden_channels)
        self.conv32 = SAGEConv((-1, -1), hidden_channels)
        self.conv4 = SAGEConv((-1, -1), hidden_channels)
        self.conv42 = SAGEConv((-1, -1), hidden_channels)
        self.conv5 = SAGEConv(-1, hidden_channels)
        self.lin = Linear(hidden_channels, out_channels)

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
        game_x = self.conv12(game_x, edge_index_v_v).relu()

        game_x = self.conv1(game_x, edge_index_v_v, edge_type_v_v).relu()

        state_x = self.conv3(
            (game_x, state_x),
            edge_index_history_v_s,
            edge_attr_history_v_s,
        ).relu()

        state_x = self.conv32(
            (game_x, state_x),
            edge_index_history_v_s,
        ).relu()

        state_x = self.conv4(
            (game_x, state_x),
            edge_index_in_v_s,
        ).relu()

        state_x = self.conv42(
            (game_x, state_x),
            edge_index_in_v_s,
        ).relu()

        state_x = self.conv2(
            state_x,
            edge_index_s_s,
        ).relu()

        state_x = self.conv5(
            state_x,
            edge_index_s_s,
        ).relu()

        return self.lin(state_x)
