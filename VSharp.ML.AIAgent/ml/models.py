import torch


from torch_geometric.nn import GATConv, Linear

from data_loader import NUM_NODE_FEATURES
from torch.nn import Linear
import torch.nn.functional as F
from torch_geometric.nn import GCNConv, SAGEConv, HeteroConv
from torch_geometric.nn import global_mean_pool

NUM_PREDICTED_VALUES = 4

class HeteroGNN(torch.nn.Module):
    def __init__(self, metadata, hidden_channels, out_channels, num_layers):
        super().__init__()

        self.convs = torch.nn.ModuleList()
        for _ in range(num_layers):
            conv = HeteroConv({
                edge_type: SAGEConv((-1, -1), hidden_channels)
                for edge_type in metadata[1]
            })
            self.convs.append(conv)

        self.lin = Linear(hidden_channels, out_channels)

    def forward(self, x_dict, edge_index_dict):
        for conv in self.convs:
            x_dict = conv(x_dict, edge_index_dict)
            x_dict = {key: F.leaky_relu(x) for key, x in x_dict.items()}
        return self.lin(x_dict['author'])

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
    #https://discuss.pytorch.org/t/a-model-with-multiple-outputs/10440
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

        return F.log_softmax(x1, dim=1), F.log_softmax(x2, dim=1), \
               F.log_softmax(x3, dim=1), F.log_softmax(x4, dim=1)


class GNN_Het(torch.nn.Module):
    def __init__(self, hidden_channels, out_channels):
        super().__init__()
        self.conv1 = SAGEConv((-1, -1), hidden_channels)
        self.conv2 = SAGEConv((-1, -1), out_channels)

    def forward(self, x, edge_index):
        x = self.conv1(x, edge_index).relu()
        x = self.conv2(x, edge_index)
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