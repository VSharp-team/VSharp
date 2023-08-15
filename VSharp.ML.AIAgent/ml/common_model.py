import torch
import os
import re

from torch import nn
from torch_geometric.nn import (
    GATConv,
    SAGEConv,
)
from torchvision.ops import MLP

from common.game import GameState
from ml.data_loader_compact import ServerDataloaderHeteroVector
from ml.model_wrappers.protocols import Predictor
from ml.predict_state_vector_hetero import PredictStateVectorHetGNN
import csv

from learning.play_game import play_game
from config import GeneralConfig
from connection.game_server_conn.utils import MapsType
from ml.models import SAGEConvModel

csv_path = "../report/epochs_tables/"
models_path = "../report/epochs_best/"


class CommonModel(torch.nn.Module):
    def __init__(
        self,
        hidden_channels,
        num_gv_layers=2,
        num_sv_layers=2,
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


def euclidean_dist(y_pred, y_true):
    y_pred_min, ind1 = torch.min(y_pred, dim=0)
    y_pred_norm = y_pred - y_pred_min

    y_true_min, ind1 = torch.min(y_true, dim=0)
    y_true_norm = y_true - y_true_min

    return torch.sqrt(torch.sum((y_pred_norm - y_true_norm) ** 2))


class CommonModelWrapper(Predictor):
    def __init__(self, model: torch.nn.Module, best_models: dict) -> None:
        self.model = model
        self.best_models = best_models
        self._model = model

    def name(self) -> str:
        return "MY AWESOME MODEL"

    def model(self):
        return self._model

    def predict(self, input: GameState, map_name):
        hetero_input, state_map = ServerDataloaderHeteroVector.convert_input_to_tensor(
            input
        )
        assert self.model is not None

        next_step_id = PredictStateVectorHetGNN.predict_state_single_out(
            self.model, hetero_input, state_map
        )

        back_prop(self.best_models[map_name], self.model, hetero_input)

        del hetero_input
        return next_step_id


def get_last_epoch_num(path):
    epochs = list(map(lambda x: re.findall("[0-9]+", x), os.listdir(path)))
    return str(sorted(epochs)[-1][0])


def csv2best_models():
    best_models = {}
    values = []
    models_names = []

    with open(csv_path + get_last_epoch_num(csv_path) + ".csv", "r") as csv_file:
        csv_reader = csv.reader(csv_file)
        map_names = next(csv_reader)[1:]
        for row in csv_reader:
            models_names.append(row[0])
            int_row = list(map(lambda x: int(x), row[1:]))
            values.append(int_row)
        val, ind = torch.max(torch.tensor(values), dim=0)
        for i in range(len(map_names)):
            best_models[map_names[i]] = models_names[ind[i]]
        return best_models


def back_prop(best_model, model, data):
    model.train()
    data.to(GeneralConfig.DEVICE)
    optimizer.zero_grad()
    ref_model = SAGEConvModel(16)
    ref_model.load_state_dict(
        torch.load(
            models_path
            + "epoch_"
            + get_last_epoch_num(models_path)
            + "/"
            + best_model
            + ".pth"
        )
    )
    ref_model.to(GeneralConfig.DEVICE)
    out = model(data.x_dict, data.edge_index_dict, data.edge_attr_dict)
    y_true = ref_model(data.x_dict, data.edge_index_dict, data.edge_attr_dict)

    loss = criterion(out, y_true)
    loss.backward()
    optimizer.step()


model = CommonModel(32)
model.to(GeneralConfig.DEVICE)

cmwrapper = CommonModelWrapper(model, csv2best_models())
lr = 0.0001
epochs = 10
optimizer = torch.optim.Adam(model.parameters(), lr=lr)
criterion = euclidean_dist


def train():
    for epoch in range(epochs):
        # some function with some parameters
        play_game(
            with_predictor=cmwrapper,
            max_steps=GeneralConfig.MAX_STEPS,
            maps_type=MapsType.TRAIN,
        )


train()
