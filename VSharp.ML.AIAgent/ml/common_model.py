import os.path
from collections import namedtuple

import torch
import torch.nn.functional as F
from common.constants import DEVICE
from ml import data_loader_compact
from ml.models import GNN_Het
from torch_geometric.data import HeteroData
from torch_geometric.loader import DataLoader
from torch_geometric.nn import to_hetero
from torch import nn
from torch.nn import Linear
from torch_geometric.nn import (
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
from conn.socket_manager import game_server_socket_manager
from ml.model_wrappers.protocols import Predictor



class CommonModel(torch.nn.Module):
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

        self.history1 = GATConv((-1, -1), hidden_channels, add_self_loops=False)

        self.in1 = SAGEConv((-1, -1), hidden_channels)

        self.sv_layers2 = nn.ModuleList()
        self.sv_layers2.append(SAGEConv(-1, hidden_channels))
        for i in range(num_sv_layers - 1):
            sage_sv = SAGEConv(-1, hidden_channels)
            self.sv_layers2.append(sage_sv)

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
            (game_x, history_x),
            edge_index_dict[("game_vertex", "in", "state_vertex")]
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


def euclidean_dist(y_pred, y_true):
    y_pred_min, ind1 = torch.min(y_pred, dim=0)
    y_pred_norm = y_pred - y_pred_min

    y_true_min, ind1 = torch.min(y_true, dim=0)
    y_true_norm = y_true - y_true_min

    return torch.sqrt(torch.sum((y_pred_norm - y_true_norm) ** 2))


lr = 0.0001
model = CommonModel(32)
model.to(DEVICE)
optimizer = torch.optim.Adam(model.parameters(), lr=lr)
criterion = euclidean_dist


class CommonModelPredictor(Predictor):
    def __init__(self, model: torch.nn.Module, best_models: dict) -> None:
        self.model = model
        self._name = str(sum(weights_flat))
        self._hash = tuple(weights_flat).__hash__()

    def name(self) -> str:
        return self._name

    def predict(self, input: GameState):
        hetero_input, state_map = ServerDataloaderHeteroVector.convert_input_to_tensor(
            input
        )
        assert self.model is not None

        next_step_id = PredictStateVectorHetGNN.predict_state_single_out(
            self.model, hetero_input, state_map
        )

        back_prop(best_models[input.map_name], self.model, hetero_input)

        del hetero_input
        return next_step_id


def back_prop(best_model, model, data):
    model.train()
    data.to(DEVICE)
    optimizer.zero_grad()

    out = model(data.x_dict, data.edge_index_dict, data.edge_attr_dict)
    y_true = best_model(data.x_dict, data.edge_index_dict, data.edge_attr_dict)

    loss = criterion(out, y_true)
    loss.backward()
    optimizer.step()
