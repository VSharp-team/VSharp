import torch

from agent.connection_manager import ConnectionManager
from agent.n_agent import get_server_maps
from ml.torch_model_wrapper import TorchModelWrapper
from ml.mutation_gen import MutationProportions
from ml.mutation_gen import MutatorConfig
from ml.models import GCN

from r_learn import r_learn


def main():
    socket_urls = ["ws://0.0.0.0:8080/gameServer"]
    cm = ConnectionManager(socket_urls)

    model = GCN(hidden_channels=64)
    optimizer = torch.optim.Adam(model.parameters(), lr=0.01)
    criterion = torch.nn.CrossEntropyLoss()

    epochs = 2
    max_steps = 2
    n_models = 1

    models = [TorchModelWrapper(model, optimizer, criterion) for _ in range(n_models)]

    maps = get_server_maps(cm)

    mutator_config = MutatorConfig(
        proportions=MutationProportions(
            n_tops=4,
            averaged_n_tops=1,
            n_averaged_all=1,
            random_n_tops_averaged_mutations=2,
            random_all_averaged_mutations=2,
        ),
        mutation_volume=2,
        mutation_freq=2,
    )

    r_learn(epochs, max_steps, models, maps, mutator_config, cm)

    cm.close()


if __name__ == "__main__":
    main()
