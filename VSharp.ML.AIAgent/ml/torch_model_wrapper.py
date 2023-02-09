import numpy as np
import torch
from torch_geometric.data import Data
from typing import Callable


from common.game import State, GameState
from common.messages import Reward

LossFunction = (
    torch.nn.L1Loss
    | torch.nn.MSELoss
    | torch.nn.CrossEntropyLoss
    | torch.nn.CTCLoss
    | torch.nn.NLLLoss
    | torch.nn.PoissonNLLLoss
    | torch.nn.GaussianNLLLoss
    | torch.nn.KLDivLoss
    | torch.nn.BCELoss
    | torch.nn.BCEWithLogitsLoss
    | torch.nn.MarginRankingLoss
    | torch.nn.HingeEmbeddingLoss
    | torch.nn.MultiLabelMarginLoss
    | torch.nn.HuberLoss
    | torch.nn.SmoothL1Loss
    | torch.nn.SoftMarginLoss
    | torch.nn.MultiLabelSoftMarginLoss
    | torch.nn.CosineEmbeddingLoss
    | torch.nn.MultiMarginLoss
    | torch.nn.TripletMarginLoss
    | torch.nn.TripletMarginWithDistanceLoss
)


class TorchModelWrapper:
    """
    обертка над моделью (например, из TensorFlow)
    - обучает модель
    - возвращает статистику обучения
    """

    def __init__(
        self,
        torch_model: torch.nn.Module,
        optimiser: torch.optim.Optimizer,
        criterion: LossFunction,
    ) -> None:
        self.model = torch_model
        self.optimiser = optimiser
        self.criterion = criterion

    @staticmethod
    def convert_input_to_tensor(input: GameState):
        """
        Converts game env to tensors

        input later can be changed to <GameState, History, ...>
        """

        nodes = []  # ?
        edges = []  # ?
        edge_attr_ = []  # ?

        x = torch.tensor(np.array(nodes), dtype=torch.float)
        edge_index = torch.tensor(edges, dtype=torch.long)
        edge_attr = torch.tensor(np.array(edge_attr_), dtype=torch.long)
        data = Data(
            x=x,
            edge_index=edge_index.t().contiguous(),
            edge_attr=edge_attr,
        )
        return data

    @staticmethod
    def convert_expected_to_tensor(input: Reward) -> Data:
        return input

    def train(
        self,
        input: GameState,  # type can be changed in the future
        actual_reward_closure: Callable[[State], Reward],
    ):
        encoded_input = TorchModelWrapper.convert_input_to_tensor(input)
        ...
        encoded_expected = actual_reward_closure(0)
        ...

    def predict(input: GameState):
        ...


def average_n_models(models: list[TorchModelWrapper]) -> TorchModelWrapper:
    ...  # TODO: implement
    return models[0]


def mutate_random(
    model: TorchModelWrapper, mutation_volume: float, mutation_freq: float
) -> TorchModelWrapper:
    ...  # TODO: implement
    return model
