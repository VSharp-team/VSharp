import torch
from torch.nn import Linear
from torch_geometric.nn import TAGConv, GraphConv, SAGEConv


class StateGNNEncoderConvEdgeAttrCompact(torch.nn.Module):
    def __init__(self, hidden_channels, out_channels):
        super().__init__()
        self.conv1 = TAGConv(5, hidden_channels, 2)
        self.conv2 = TAGConv(hidden_channels, hidden_channels, 3) #TAGConv
        self.conv3 = GraphConv((-1, -1), hidden_channels)  # SAGEConv
        self.conv4 = SAGEConv((-1, -1), hidden_channels)
        self.lin = Linear(hidden_channels, out_channels)

    def forward(self, x_dict, edge_index_dict, edge_attr=None):

        game_x = self.conv1(
            x_dict['game_vertex'],
            edge_index_dict[('game_vertex', 'to', 'game_vertex')],
        ).relu()

        state_x = self.conv3(
            (game_x, x_dict['state_vertex']),
            edge_index_dict[('game_vertex', 'history', 'state_vertex')],
            edge_attr[('game_vertex', 'history', 'state_vertex')],
        ).relu()


        state_x = self.conv4(
            (game_x, state_x),
            edge_index_dict[('game_vertex', 'in', 'state_vertex')],
        ).relu()

        state_x = self.conv2(
            state_x,
            edge_index_dict[('state_vertex', 'parent_of', 'state_vertex')],
        ).relu()

        return self.lin(state_x)

class StateModelEncoder(torch.nn.Module):
    def __init__(self, hidden_channels, out_channels):
        super().__init__()
        self.state_encoder = StateGNNEncoderConvEdgeAttrCompact(hidden_channels, out_channels)

    def forward(self, x_dict, edge_index_dict, edge_attr=None):
        z_dict = {}
        z_dict['state_vertex'] = self.state_encoder(x_dict, edge_index_dict, edge_attr)
        z_dict['game_vertex'] = x_dict['game_vertex']
        return z_dict
