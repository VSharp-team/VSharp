import os.path
from collections import namedtuple

import torch
import torch.nn.functional as F
from torch_geometric.data import HeteroData
from torch_geometric.loader import DataLoader
from torch_geometric.nn import to_hetero

from config import GeneralConfig
from ml import data_loader_compact
from ml.models import GNN_Het

StateVectorMapping = namedtuple("StateVectorMapping", ["state", "vector"])


class PredictStateVectorHetGNN:
    """predicts ExpectedStateNumber using Heterogeneous GNN"""

    def __init__(self):
        self.state_maps = {}
        self.start()

    def start(self):
        dataset = data_loader_compact.get_data_hetero_vector()
        torch.manual_seed(12345)

        split_at = round(len(dataset) * 0.85)

        train_dataset = dataset[:split_at]
        test_dataset = dataset[split_at:]

        print(f"Number of training graphs: {len(train_dataset)}")
        print(f"Number of test graphs: {len(test_dataset)}")

        train_loader = DataLoader(
            train_dataset, batch_size=1, shuffle=True
        )  # TODO: add learning by batches!
        test_loader = DataLoader(test_dataset, batch_size=1, shuffle=False)

        model = GNN_Het(hidden_channels=64, out_channels=8)
        model = to_hetero(model, dataset[0].metadata(), aggr="sum")
        optimizer = torch.optim.Adam(model.parameters(), lr=0.01)

        for epoch in range(1, 31):
            self.train(model, train_loader, optimizer)
            train_acc = self.tst(model, train_loader)
            test_acc = self.tst(model, test_loader)
            print(
                f"Epoch: {epoch:03d}, Train Loss: {train_acc:.4f}, Test Loss: {test_acc:.4f}"
            )

        self.save(model, "./saved_models")

    # loss function from link prediction example
    def weighted_mse_loss(self, pred, target, weight=None):
        weight = 1.0 if weight is None else weight[target].to(pred.dtype)
        return (weight * (pred - target.to(pred.dtype)).pow(2)).mean()

    def train(self, model, train_loader, optimizer):
        model.train()

        for data in train_loader:  # Iterate in batches over the training dataset.
            out = model(data.x_dict, data.edge_index_dict)
            pred = out["state_vertex"]
            target = data.y
            loss = F.mse_loss(pred, target)
            loss.backward()  # Derive gradients.
            optimizer.step()  # Update parameters based on gradients.
            optimizer.zero_grad()  # Clear gradients.

    def tst(self, model, loader):
        model.eval()
        for data in loader:
            out = model(data.x_dict, data.edge_index_dict)
            pred = out["state_vertex"]
            target = data.y
            loss = F.mse_loss(pred, target)
        return loss

    @staticmethod
    def predict_state(model, data: HeteroData, state_map: dict[int, int]) -> int:
        """Gets state id from model and heterogeneous graph
        data.state_map - maps real state id to state index"""
        state_map = {v: k for k, v in state_map.items()}  # inversion for prediction
        out = model(data.x_dict, data.edge_index_dict)
        return state_map[int(out["state_vertex"].argmax(dim=0)[0])]

    @staticmethod
    def predict_state_with_dict(
        model: torch.nn.Module, data: HeteroData, state_map: dict[int, int]
    ) -> int:
        """Gets state id from model and heterogeneous graph
        data.state_map - maps real state id to state index"""

        data.to(GeneralConfig.DEVICE)
        reversed_state_map = {v: k for k, v in state_map.items()}

        with torch.no_grad():
            out = model.forward(data.x_dict, data.edge_index_dict, data.edge_attr_dict)

        remapped = []

        for index, vector in enumerate(out["state_vertex"]):
            state_vector_mapping = StateVectorMapping(
                state=reversed_state_map[index],
                vector=(vector.detach().cpu().numpy()).tolist(),
            )
            remapped.append(state_vector_mapping)

        return max(remapped, key=lambda mapping: sum(mapping.vector)).state

    def predict_state_single_out(
        model: torch.nn.Module, data: HeteroData, state_map: dict[int, int]
    ) -> int:
        """Gets state id from model and heterogeneous graph
        data.state_map - maps real state id to state index"""

        data.to(GeneralConfig.DEVICE)
        reversed_state_map = {v: k for k, v in state_map.items()}

        with torch.no_grad():
            out = model.forward(data.x_dict, data.edge_index_dict, data.edge_attr_dict)

        remapped = []
        if type(out) is dict:
            out = out["state_vertex"]
        for index, vector in enumerate(out):
            state_vector_mapping = StateVectorMapping(
                state=reversed_state_map[index],
                vector=(vector.detach().cpu().numpy()).tolist(),
            )
            remapped.append(state_vector_mapping)

        return max(remapped, key=lambda mapping: sum(mapping.vector)).state

    def save(self, model, dir):
        filepath = os.path.join(dir, "GNN_state_pred_het_dict")
        # case 1
        torch.save(model.state_dict(), filepath)
        # case 2
        filepath = os.path.join(dir, "GNN_state_pred_het_full")
        torch.save(model, filepath)
