import torch
from torch.autograd import Variable
from torch_geometric.nn import to_hetero, to_hetero_with_bases
from torch_geometric.data import HeteroData

import data_loader
from models import GCN, GCN_SimpleMultipleOutput, GNN_MultipleOutput, GAT, GNN_Het
from random import shuffle
from torch_geometric.loader import DataLoader
import torch.nn.functional as F


class PredictStateHetGNN:
    """predicts ExpectedStateNumber using Heterogeneous GNN"""

    def __init__(self):
        self.start()

    def start(self):
        dataset = data_loader.get_data_hetero()
        torch.manual_seed(12345)
        # shuffle(dataset)

        split_at = round(len(dataset) * 0.85)

        train_dataset = dataset[:split_at]
        test_dataset = dataset[split_at:]

        print(f'Number of training graphs: {len(train_dataset)}')
        print(f'Number of test graphs: {len(test_dataset)}')

        train_loader = DataLoader(train_dataset, batch_size=1, shuffle=True) # TODO: add learning by batches!
        test_loader = DataLoader(test_dataset, batch_size=1, shuffle=False)

        # device = torch.device('cuda' if torch.cuda.is_available() else 'cpu')
        model = GNN_Het(hidden_channels=64, out_channels=1)
        model = to_hetero(model, dataset[0].metadata(), aggr='sum')
        # model = to_hetero_with_bases(model, dataset[0].metadata(), num_bases=3)
        optimizer = torch.optim.Adam(model.parameters(), lr=0.01)
        # criterion = torch.nn.CrossEntropyLoss()

        for epoch in range(1, 201):
            self.train(model, train_loader, optimizer)
            train_acc = self.tst(model, train_loader)
            test_acc = self.tst(model, test_loader)
            print(f'Epoch: {epoch:03d}, Train Acc: {train_acc:.4f}, Test Acc: {test_acc:.4f}')

    # loss function from link prediction example
    def weighted_mse_loss(self, pred, target, weight=None):
        weight = 1. if weight is None else weight[target].to(pred.dtype)
        return (weight * (pred - target.to(pred.dtype)).pow(2)).mean()

    def train(self, model, train_loader, optimizer):
        model.train()

        for data in train_loader:  # Iterate in batches over the training dataset.
            out = model(data.x_dict, data.edge_index_dict)
            pred = out['state_vertex']
            target = torch.zeros(pred.size())
            state_to_move = data.y[0][0]
            # TODO: more intellectual
            target[state_to_move][0] = 5000.  # should be substantially greater than the total number of states!
            loss = F.mse_loss(pred, target)
            loss.backward()  # Derive gradients.
            optimizer.step()  # Update parameters based on gradients.
            optimizer.zero_grad()  # Clear gradients.

    def train_single_value(self, model, train_loader, optimizer):
        # It does not work properly now --- no actual learning!!!'''
        model.train()

        for data in train_loader:  # Iterate in batches over the training dataset.
            out = model(data.x_dict, data.edge_index_dict)
            pred = out['state_vertex'].argmax(dim=0)[0].float()
            target = torch.tensor(data.y[0][0]).float()
            # loss = self.weighted_mse_loss(torch.tensor(pred, dtype=float),
            # torch.tensor(pred, dtype=float)).sqrt()
            loss = self.weighted_mse_loss(pred, target)
            # print("loss", loss)
            # loss = Variable(loss, requires_grad=True) #TODO: dirty hack, fix it later
            loss.requires_grad = True
            # print(loss)
            # loss = criterion(torch.tensor(pred, dtype=float), torch.tensor(data.y[0][0]))  # Compute the loss.
            loss.backward()  # Derive gradients.
            optimizer.step()  # Update parameters based on gradients.
            optimizer.zero_grad()  # Clear gradients.

    def tst(self, model, loader):
        model.eval()
        correct = 0
        for data in loader:
            pred = self.predict_state(model, data)
            target = data.y[0][0]
            #print(pred, target)
            correct += int((pred == target))
        return correct / len(loader.dataset)

    @staticmethod
    def predict_state(model, data: HeteroData) -> int:
        '''Gets state id from model and heterogeneous graph'''
        out = model(data.x_dict, data.edge_index_dict)
        return int(out['state_vertex'].argmax(dim=0)[0])


if __name__ == '__main__':
    PredictStateHetGNN()
