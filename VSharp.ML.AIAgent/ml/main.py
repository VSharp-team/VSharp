import torch
from torch.autograd import Variable
from torch_geometric.nn import to_hetero, to_hetero_with_bases

import data_loader
from models import GCN, GCN_SimpleMultipleOutput, GNN_MultipleOutput, GAT, GNN_Het
from random import shuffle
from torch_geometric.loader import DataLoader
import torch.nn.functional as F


def main():
    dataset = data_loader.get_data()
    torch.manual_seed(12345)
    #shuffle(dataset)

    split_at = round(len(dataset)*0.85)

    train_dataset = dataset[:split_at]
    test_dataset = dataset[split_at:]

    print(f'Number of training graphs: {len(train_dataset)}')
    print(f'Number of test graphs: {len(test_dataset)}')

    train_loader = DataLoader(train_dataset, batch_size=64, shuffle=True)
    test_loader = DataLoader(test_dataset, batch_size=64, shuffle=False)

    #device = torch.device('cuda' if torch.cuda.is_available() else 'cpu')
    model = GCN(hidden_channels=64)
    optimizer = torch.optim.Adam(model.parameters(), lr=0.01)
    criterion = torch.nn.CrossEntropyLoss()

    for epoch in range(1, 201):
        train(model, train_loader, criterion, optimizer)
        train_acc = tst(model, train_loader)
        test_acc = tst(model, test_loader)
        print(f'Epoch: {epoch:03d}, Train Acc: {train_acc:.4f}, Test Acc: {test_acc:.4f}')

def train(model, train_loader, criterion, optimizer):
    model.train()

    for data in train_loader:  # Iterate in batches over the training dataset.
         out = model(data.x, data.edge_index, data.batch)  # Perform a single forward pass.
         loss = criterion(out, data.y)  # Compute the loss.
         loss.backward()  # Derive gradients.
         optimizer.step()  # Update parameters based on gradients.
         optimizer.zero_grad()  # Clear gradients.

def tst(model, loader):
     model.eval()
     correct = 0
     for data in loader:
         out = model(data.x, data.edge_index, data.batch)
         pred = out.argmax(dim=1)
         #for (p, e) in zip(pred, data.y):
             #print(p, e)
         correct += int((pred == data.y).sum())
     return correct / len(loader.dataset) 



