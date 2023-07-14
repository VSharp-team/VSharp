import torch
import torch.nn.functional as F
from torch import nn
from torch.nn import Linear
from torch_geometric.nn import (
    ARMAConv,
    FeaStConv,
    GATConv,
    GatedGraphConv,
    GCNConv,
    HeteroConv,
    Linear,
    ResGatedGraphConv,
    SAGEConv,
    TAGConv,
    TransformerConv,
    global_mean_pool,
    to_hetero,
)
from torchvision.ops import MLP

from timer.wrapper import timeit

from .data_loader_compact import NUM_NODE_FEATURES

NUM_PREDICTED_VALUES = 4


class ARMANet(torch.nn.Module):
    def __init__(self, hidden_channels, num_classes, num_stacks=1, num_layers=1):
        super(ARMANet, self).__init__()

        self.conv1 = ARMAConv(
            -1,
            out_channels=hidden_channels,
            num_stacks=num_stacks,
            num_layers=num_layers,
        )

        self.conv2 = ARMAConv(
            -1,
            out_channels=hidden_channels,
            num_stacks=num_stacks,
            num_layers=num_layers,
        )

        self.fc1 = nn.Linear(64, num_classes)

    """def forward(self, x, edge_index):
        x = F.relu(self.conv1(x, edge_index))
        x = F.relu(self.conv2(x, edge_index))
        #x = global_mean_pool(x, x.batch)
        x = F.dropout(x)
        x = self.fc1(x)
        return x"""

    def forward(self, x, edge_index):
        x = self.conv1(x, edge_index).relu()
        x = self.conv2(x, edge_index)
        return x


class GATNet(torch.nn.Module):
    def __init__(self, in_channels, hidden_channels=512, num_classes=2):
        super(GATNet, self).__init__()

        # self.fc0 = nn.Linear(in_channels, hidden_channels)
        self.conv1 = GATConv(in_channels, hidden_channels)
        self.conv2 = GATConv(hidden_channels, 64)
        self.fc1 = nn.Linear(64, num_classes)

        self.reset_parameters()

    def reset_parameters(self):
        self.conv1.reset_parameters()
        self.conv2.reset_parameters()

    def forward(self, x, edge_index):
        # x = F.relu(self.fc0(x))
        x = F.relu(self.conv1(x, edge_index))
        x = F.relu(self.conv2(x, edge_index))
        # x = global_mean_pool(x, data.batch)
        x = F.dropout(x, training=self.training)
        x = self.fc1(x)
        return x


class FeaStNet(torch.nn.Module):
    def __init__(
        self, in_channels, hidden_channels=512, num_classes=2, heads=1, t_inv=True
    ):
        super(FeaStNet, self).__init__()

        # self.fc0 = nn.Linear(in_channels, hidden_channels)
        self.conv1 = FeaStConv(in_channels, hidden_channels, heads=heads, t_inv=t_inv)
        self.conv2 = FeaStConv(hidden_channels, 64, heads=heads, t_inv=t_inv)
        self.fc1 = nn.Linear(64, num_classes)

        self.reset_parameters()

    def reset_parameters(self):
        self.conv1.reset_parameters()
        self.conv2.reset_parameters()

    def forward(self, data):
        x, edge_index = data.x, data.edge_index

        # x = F.relu(self.fc0(x))
        x = F.relu(self.conv1(x, edge_index))
        x = F.relu(self.conv2(x, edge_index))
        x = global_mean_pool(x, data.batch)
        x = F.dropout(x, training=self.training)
        x = self.fc1(x)
        return x


class RGGCN(torch.nn.Module):
    def __init__(self, in_channels, hidden_channels=512, num_classes=2):
        super(RGGCN, self).__init__()

        # self.fc0 = nn.Linear(in_channels, hidden_channels)
        self.conv1 = ResGatedGraphConv(in_channels, hidden_channels)
        self.conv2 = ResGatedGraphConv(hidden_channels, 64)
        self.fc1 = nn.Linear(64, num_classes)

        self.reset_parameters()

    def reset_parameters(self):
        self.conv1.reset_parameters()
        self.conv2.reset_parameters()

    def forward(self, data):
        x, edge_index = data.x, data.edge_index

        # x = F.relu(self.fc0(x))
        x = F.relu(self.conv1(x, edge_index))
        x = F.relu(self.conv2(x, edge_index))
        x = global_mean_pool(x, data.batch)
        x = F.dropout(x, training=self.training)
        x = self.fc1(x)
        return x


class UniMP(torch.nn.Module):
    def __init__(self, in_channels, hidden_channels=512, num_classes=2):
        super(UniMP, self).__init__()
        # self.fc0 = nn.Linear(in_channels, hidden_channels)
        self.conv1 = TransformerConv(in_channels, hidden_channels)
        self.conv2 = TransformerConv(hidden_channels, 64)
        self.fc1 = nn.Linear(64, num_classes)

        self.reset_parameters()

    def reset_parameters(self):
        self.conv1.reset_parameters()
        self.conv2.reset_parameters()

    def forward(self, data):
        x, edge_index = data.x, data.edge_index
        # x = F.relu(self.fc0(x))
        x = F.relu(self.conv1(x, edge_index))
        x = F.relu(self.conv2(x, edge_index))
        x = global_mean_pool(x, data.batch)
        x = F.dropout(x, training=self.training)
        x = self.fc1(x)
        return x


class HeteroGNN(torch.nn.Module):
    def __init__(self, metadata, hidden_channels, out_channels, num_layers):
        super().__init__()

        self.convs = torch.nn.ModuleList()
        for _ in range(num_layers):
            conv = HeteroConv(
                {
                    edge_type: SAGEConv((-1, -1), hidden_channels)
                    for edge_type in metadata[1]
                }
            )
            self.convs.append(conv)

        self.lin = Linear(hidden_channels, out_channels)

    def forward(self, x_dict, edge_index_dict):
        for conv in self.convs:
            x_dict = conv(x_dict, edge_index_dict)
            x_dict = {key: F.leaky_relu(x) for key, x in x_dict.items()}
        return self.lin(x_dict["author"])


class GCN_SimpleNoEdgeLabels(torch.nn.Module):
    def __init__(self):
        super().__init__()
        self.conv1 = GCNConv(NUM_NODE_FEATURES, 16)
        self.conv2 = GCNConv(16, 1)

    def forward(self, data):
        x, edge_index = data.x, data.edge_index

        x = self.conv1(x, edge_index)
        x = F.relu(x)
        x = F.dropout(x, training=self.training)
        x = self.conv2(x, edge_index)

        return F.log_softmax(x, dim=1)


class GNN_MultipleOutput(torch.nn.Module):
    def __init__(self, hidden_channels, out_channels):
        super().__init__()
        self.conv1 = SAGEConv((-1, -1), hidden_channels)
        self.conv2 = SAGEConv((-1, -1), out_channels)

    def forward(self, x, edge_index):
        x1 = self.conv1(x, edge_index).relu()
        x1 = self.conv2(x1, edge_index)

        x2 = self.conv1(x, edge_index).relu()
        x2 = self.conv2(x2, edge_index)

        x3 = self.conv1(x, edge_index).relu()
        x3 = self.conv2(x3, edge_index)

        x4 = self.conv1(x, edge_index).relu()
        x4 = self.conv2(x4, edge_index)
        return x1, x2, x3, x4


class GCN_SimpleMultipleOutput(torch.nn.Module):
    # https://discuss.pytorch.org/t/a-model-with-multiple-outputs/10440
    def __init__(self, hidden_channels):
        super(GCN_SimpleMultipleOutput, self).__init__()
        torch.manual_seed(12345)
        self.conv1 = GCNConv(NUM_NODE_FEATURES, hidden_channels)
        self.conv1.add_self_loops = False
        self.conv2 = GCNConv(hidden_channels, NUM_PREDICTED_VALUES)
        self.conv2.add_self_loops = False

    def forward(self, x, edge_index):
        x1 = self.conv1(x, edge_index)
        x1 = F.relu(x1)
        x1 = F.dropout(x1, training=self.training)
        x1 = self.conv2(x1, edge_index)

        x2 = self.conv1(x, edge_index)
        x2 = F.relu(x2)
        x2 = F.dropout(x2, training=self.training)
        x2 = self.conv2(x2, edge_index)

        x3 = self.conv1(x, edge_index)
        x3 = F.relu(x3)
        x3 = F.dropout(x3, training=self.training)
        x3 = self.conv2(x3, edge_index)

        x4 = self.conv1(x, edge_index)
        x4 = F.relu(x4)
        x4 = F.dropout(x4, training=self.training)
        x4 = self.conv2(x4, edge_index)

        return (
            F.log_softmax(x1, dim=1),
            F.log_softmax(x2, dim=1),
            F.log_softmax(x3, dim=1),
            F.log_softmax(x4, dim=1),
        )


class GNN_Het(torch.nn.Module):
    def __init__(self, hidden_channels, out_channels):
        super().__init__()
        self.conv1 = SAGEConv((-1, -1), hidden_channels)
        self.conv2 = SAGEConv((-1, -1), out_channels)
        # GATConv

    def forward(self, x, edge_index):
        x = self.conv1(x, edge_index).relu()
        x = self.conv2(x, edge_index)
        return x


class ARMA_Het(torch.nn.Module):
    def __init__(self, hidden_channels, out_channels):
        super().__init__()
        self.conv1 = ARMAConv(-1, hidden_channels)
        self.conv2 = ARMAConv(-1, out_channels)

    def forward(self, x, edge_index):
        x = self.conv1(x, edge_index).relu()
        x = self.conv2(x, edge_index)
        return x


class GNN_Het_EA(torch.nn.Module):
    def __init__(self, hidden_channels, out_channels):
        super().__init__()
        self.conv1 = SAGEConv((-1, -1), hidden_channels)
        self.conv2 = SAGEConv((-1, -1), out_channels)

    def forward(self, x, edge_index, edge_attr):
        x = self.conv1(x, edge_index, edge_attr).relu()
        x = self.conv2(x, edge_index, edge_attr)
        return x


class GCN(torch.nn.Module):
    def __init__(self, hidden_channels):
        super(GCN, self).__init__()
        torch.manual_seed(12345)
        self.conv1 = GCNConv(NUM_NODE_FEATURES, hidden_channels)
        self.conv2 = GCNConv(hidden_channels, hidden_channels)
        self.conv3 = GCNConv(hidden_channels, hidden_channels)
        self.lin = Linear(hidden_channels, 3421)

    def forward(self, x, edge_index, batch):
        x = self.conv1(x, edge_index)
        x = x.relu()
        x = self.conv2(x, edge_index)
        x = x.relu()
        x = self.conv3(x, edge_index)

        x = global_mean_pool(x, batch)  # [batch_size, hidden_channels]

        x = F.dropout(x, p=0.5, training=self.training)
        x = self.lin(x)

        return x


class GAT(torch.nn.Module):
    def __init__(self, hidden_channels, out_channels):
        super().__init__()
        self.conv1 = GATConv((-1, -1), hidden_channels, add_self_loops=False)
        self.lin1 = Linear(-1, hidden_channels)
        self.conv2 = GATConv((-1, -1), out_channels, add_self_loops=False)
        self.lin2 = Linear(-1, out_channels)

    def forward(self, x, edge_index):
        x = self.conv1(x, edge_index) + self.lin1(x)
        x = x.relu()
        x = self.conv2(x, edge_index) + self.lin2(x)
        return x


class VertexGNNEncoder(torch.nn.Module):
    def __init__(self, hidden_channels, out_channels):
        super().__init__()

        self.conv1 = SAGEConv(-1, hidden_channels)
        self.conv2 = SAGEConv(hidden_channels, hidden_channels)
        self.lin = Linear(hidden_channels, out_channels)

    def forward(self, x, edge_index):
        x = self.conv1(x, edge_index).relu()
        x = self.conv2(x, edge_index).relu()
        return self.lin(x)


class StateGNNEncoder(torch.nn.Module):
    def __init__(self, hidden_channels, out_channels):
        super().__init__()
        self.conv1 = SAGEConv((-1, -1), hidden_channels)
        self.conv2 = SAGEConv((-1, -1), hidden_channels)
        self.conv3 = SAGEConv((-1, -1), hidden_channels)
        self.conv4 = SAGEConv((-1, -1), hidden_channels)
        self.lin = Linear(hidden_channels, out_channels)

    def forward(self, x_dict, edge_index_dict):
        game_x = self.conv1(
            x_dict["game_vertex"],
            edge_index_dict[("game_vertex", "to", "game_vertex")],
        ).relu()

        state_x = self.conv2(
            x_dict["state_vertex"],
            edge_index_dict[("state_vertex", "parent_of", "state_vertex")],
        ).relu()

        state_x = self.conv3(
            (game_x, state_x),
            edge_index_dict[("game_vertex", "history", "state_vertex")],
        ).relu()

        state_x = self.conv4(
            (game_x, state_x),
            edge_index_dict[("game_vertex", "in", "state_vertex")],
        ).relu()

        return self.lin(state_x)


class StateGNNEncoderConv(torch.nn.Module):
    def __init__(self, hidden_channels, out_channels):
        super().__init__()
        # self.conv1 = GCNConv(5, hidden_channels)
        # self.conv2 = GCNConv(6, hidden_channels)
        # GravNetConv
        # self.conv1 = GravNetConv(-1, hidden_channels, 2, 2, 2)
        # self.conv2 = GravNetConv(-1, hidden_channels, 2, 2, 2)
        # GatedGraphConv
        self.conv1 = TAGConv(5, hidden_channels, 10)
        self.conv12 = TAGConv(hidden_channels, hidden_channels, 10)  # TAGConv
        self.conv22 = TAGConv(hidden_channels, hidden_channels, 10)  # TAGConv
        self.conv2 = TAGConv(6, hidden_channels, 10)  # TAGConv
        self.conv3 = SAGEConv((-1, -1), hidden_channels)  # SAGEConv
        self.conv32 = SAGEConv((-1, -1), hidden_channels)
        self.conv4 = SAGEConv((-1, -1), hidden_channels)
        self.conv42 = SAGEConv((-1, -1), hidden_channels)
        self.lin = Linear(hidden_channels, out_channels)

    def forward(self, x_dict, edge_index_dict):
        game_x = self.conv1(
            x_dict["game_vertex"],
            edge_index_dict[("game_vertex", "to", "game_vertex")],
        ).relu()

        game_x = self.conv12(
            game_x,
            edge_index_dict[("game_vertex", "to", "game_vertex")],
        ).relu()

        state_x = self.conv2(
            x_dict["state_vertex"],
            edge_index_dict[("state_vertex", "parent_of", "state_vertex")],
        ).relu()

        state_x = self.conv22(
            state_x,
            edge_index_dict[("state_vertex", "parent_of", "state_vertex")],
        ).relu()

        state_x = self.conv3(
            (game_x, state_x),
            edge_index_dict[("game_vertex", "history", "state_vertex")],
        ).relu()

        state_x = self.conv32(
            (game_x, state_x),
            edge_index_dict[("game_vertex", "history", "state_vertex")],
        ).relu()

        state_x = self.conv4(
            (game_x, state_x),
            edge_index_dict[("game_vertex", "in", "state_vertex")],
        ).relu()

        state_x = self.conv42(
            (game_x, state_x),
            edge_index_dict[("game_vertex", "in", "state_vertex")],
        ).relu()

        return self.lin(state_x)


class VerStateModel(torch.nn.Module):
    def __init__(self, metadata, hidden_channels, out_channels):
        super().__init__()
        self.vertex_encoder = VertexGNNEncoder(hidden_channels, out_channels)
        self.state_encoder = StateGNNEncoder(hidden_channels, out_channels)
        self.decoder = GNN_Het(hidden_channels, out_channels)
        self.decoder = to_hetero(self.decoder, metadata, aggr="sum")

    def forward(self, x_dict, edge_index_dict):
        z_dict = {}
        # x_dict['game_vertex'] = self.user_emb(x_dict['game_vertex'])
        z_dict["state_vertex"] = self.state_encoder(x_dict, edge_index_dict)
        z_dict["game_vertex"] = x_dict["game_vertex"]
        # print(edge_index_dict)
        # z_dict['state_vertex'] = self.state_encoder(
        #    x_dict['state_vertex'],
        #    edge_index_dict[('state_vertex', 'parent_of', 'state_vertex')],
        # )

        # return self.decoder(z_dict, edge_index_dict) # TODO: process separately
        return z_dict


class StateModelEncoder(torch.nn.Module):
    def __init__(self, hidden_channels, out_channels):
        super().__init__()
        # self.vertex_encoder = VertexGNNEncoder(hidden_channels, out_channels)
        self.state_encoder = StateGNNEncoderConv(hidden_channels, out_channels)
        # self.decoder = GNN_Het(hidden_channels, out_channels)
        # self.decoder = to_hetero(self.decoder, metadata, aggr='sum')

    @timeit
    def forward(self, x_dict, edge_index_dict):
        z_dict = {}
        # x_dict['game_vertex'] = self.user_emb(x_dict['game_vertex'])
        z_dict["state_vertex"] = self.state_encoder(x_dict, edge_index_dict)
        z_dict["game_vertex"] = x_dict["game_vertex"]
        # print(edge_index_dict)
        # z_dict['state_vertex'] = self.state_encoder(
        #    x_dict['state_vertex'],
        #    edge_index_dict[('state_vertex', 'parent_of', 'state_vertex')],
        # )

        # return self.decoder(z_dict, edge_index_dict) # TODO: process separately
        return z_dict


class SAGEConvModel(torch.nn.Module):
    def __init__(
        self,
        hidden_channels,
        num_gv_layers=2,
        num_sv_layers=2,
        num_history_layers=2,
        num_in_layers=2,
    ):
        super().__init__()
        self.gv_layers = nn.ModuleList()
        self.gv_layers.append(SAGEConv(-1, hidden_channels))
        for i in range(num_gv_layers - 1):
            sage_gv = SAGEConv(-1, hidden_channels)
            self.gv_layers.append(sage_gv)

        self.sv_layers = nn.ModuleList()
        self.sv_layers.append(SAGEConv(-1, hidden_channels))
        for i in range(num_sv_layers - 1):
            sage_sv = SAGEConv(-1, hidden_channels)
            self.sv_layers.append(sage_sv)

        # self.history_layers = nn.ModuleList()
        # for i in range(num_history_layers):
        #     sage_h = SAGEConv((-1, -1), hidden_channels)
        #     self.history_layers.append(sage_h)
        # self.history1 = SAGEConv((-1, -1), hidden_channels)
        self.history1 = GATConv((-1, -1), hidden_channels, add_self_loops=False)
        # self.history2 = GATConv((-1, -1), hidden_channels, add_self_loops=False)

        self.in1 = SAGEConv((-1, -1), hidden_channels)

        self.sv_layers2 = nn.ModuleList()
        self.sv_layers2.append(SAGEConv(-1, hidden_channels))
        for i in range(num_sv_layers - 1):
            sage_sv = SAGEConv(-1, hidden_channels)
            self.sv_layers2.append(sage_sv)

        # self.in2 = SAGEConv((-1, -1), hidden_channels)

        # self.in_layers = nn.ModuleList()
        # for i in range(num_in_layers):
        #     sage_in = SAGEConv((-1, -1), hidden_channels)
        #     self.in_layers.append(sage_in)

        self.mlp = MLP(hidden_channels, [1])

    @timeit
    def forward(self, x_dict, edge_index_dict, edge_attr_dict):
        # print(x_dict)
        # print(edge_attr_dict)
        game_x = self.gv_layers[0](
            x_dict["game_vertex"],
            edge_index_dict[("game_vertex", "to", "game_vertex")],
        ).relu()
        for layer in self.gv_layers[1:]:
            game_x = layer(
                game_x,
                edge_index_dict[("game_vertex", "to", "game_vertex")],
            ).relu()
        # print(game_x.size())

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
            size=(game_x.size(0), state_x.size(0))
            ).relu()
        
        # history_x = self.history1(
        #     (state_x, game_x),
        #     edge_index_dict[("state_vertex", "history", "game_vertex")], 
        #     edge_attr_dict,
        #     size=(state_x.size(0), game_x.size(0))
        #     ).relu()
        
        # history_x = self.history1((game_x, state_x),
        #     edge_index_dict[("game_vertex", "history", "state_vertex")]).relu()
        #history_x = self.history2((history_x, game_x),
        #    edge_index_dict[("state_vertex", "history", "game_vertex")]).relu()

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
        # in_x = self.in2(
        #    (game_x, in_x),
        #    edge_index_dict[("state_vertex", "in", "game_vertex")]
        # ).relu()

        # print(state_x.size())
        # history_x = self.history_layers[0](
        #     (game_x, state_x),
        #     edge_index_dict[("game_vertex", "history", "state_vertex")]

        #     # torch.cat((edge_index_dict[("game_vertex", "history", "state_vertex")],
        #     # edge_index_dict[("state_vertex", "history", "game_vertex")]), dim=1)
        # ).relu()
        # for layer in self.history_layers[1:]:
        #     history_x = layer(
        #     (game_x, history_x),
        #     edge_index_dict[("game_vertex", "history", "state_vertex")]

        #     # torch.cat((edge_index_dict[("game_vertex", "history", "state_vertex")],
        #     # edge_index_dict[("state_vertex", "history", "game_vertex")]), dim=1)
        #     ).relu()
        # # print(history_x.size())

        # in_x = self.in_layers[0](
        #     (game_x, history_x),
        #     edge_index_dict[("game_vertex", "in", "state_vertex")]

        #     # torch.cat((edge_index_dict[("game_vertex", "in", "state_vertex")],
        #     # edge_index_dict[("state_vertex", "in", "game_vertex")]), dim=1)
        # ).relu()
        # for layer in self.in_layers[1:]:
        #     in_x = layer(
        #     (game_x, in_x),
        #     edge_index_dict[("game_vertex", "in", "state_vertex")]

        # torch.cat((edge_index_dict[("game_vertex", "in", "state_vertex")],
        # edge_index_dict[("state_vertex", "in", "game_vertex")]), dim=1)
        # ).relu()
        # print(in_x.size())
        # print(in_x)
        x = self.mlp(in_x)
        # print(x.size())
        return x


class GatedGCNModel(torch.nn.Module):
    def __init__(self, hidden_channels):
        super().__init__()
        self.gatedgcn_gv1 = GatedGraphConv(hidden_channels, 1)
        self.gatedgcn_sv1 = GatedGraphConv(hidden_channels, 1)
        self.gatedgcn_common1 = GatedGraphConv(hidden_channels, 1)
        self.gatedgcn_common2 = GatedGraphConv(hidden_channels, 1)
        self.sage_conv1 = SAGEConv((-1, -1), hidden_channels)
        self.sage_conv2 = SAGEConv((-1, -1), hidden_channels)
        # self.gatedgcn_common3 = GatedGraphConv(hidden_channels, 1)
        self.mlp = MLP(hidden_channels, [20, 1])

    @timeit
    def forward(self, x_dict, edge_index_dict):
        game_x = self.gatedgcn_gv1(
            x_dict["game_vertex"],
            edge_index_dict[("game_vertex", "to", "game_vertex")],
        ).relu()
        print(game_x.size())
        state_x = self.gatedgcn_sv1(
            x_dict["state_vertex"],
            edge_index_dict[("state_vertex", "parent_of", "state_vertex")],
        ).relu()
        print(state_x.size())

        common_x = self.sage_conv1(
            (game_x, state_x),
            edge_index_dict[("game_vertex", "history", "state_vertex")]
            # torch.cat(
            # (edge_index_dict[("game_vertex", "history", "state_vertex")],
            # edge_index_dict[("state_vertex", "history", "game_vertex")]), 1
            # )
        ).relu()
        print(common_x.size())
        common_x = self.sage_conv2(
            (game_x, common_x),
            edge_index_dict[("game_vertex", "in", "state_vertex")]
            # torch.cat((
            # edge_index_dict[("game_vertex", "in", "state_vertex")],
            # edge_index_dict[("state_vertex", "in", "game_vertex")]), 1
            # )
        ).relu()

        return self.mlp(common_x)
