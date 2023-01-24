import torch

from agent.n_agent import get_server_maps
from ml.torch_model_wrapper import TorchModelWrapper
from ml.models import GCN

from r_learn import MutationProportions, r_learn
from r_learn import DEFAULT_URL


def main():
    model = GCN(hidden_channels=64)
    optimizer = torch.optim.Adam(model.parameters(), lr=0.01)
    criterion = torch.nn.CrossEntropyLoss()

    epochs = 3
    max_steps = 10

    models = [TorchModelWrapper(model, optimizer, criterion) for _ in range(10)]

    maps = get_server_maps(DEFAULT_URL)
    proportions = MutationProportions(4, 1, 1, 2, 2)

    r_learn(epochs, max_steps, models, maps, proportions)


if __name__ == "__main__":
    main()
