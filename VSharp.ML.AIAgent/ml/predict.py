from collections import namedtuple

import torch
from torch_geometric.data import HeteroData

from config import GeneralConfig

StateVectorMapping = namedtuple("StateVectorMapping", ["state", "vector"])


def predict_state_with_dict(
    model: torch.nn.Module, data: HeteroData, state_map: dict[int, int]
) -> int:
    """Gets state id from model and heterogeneous graph
    data.state_map - maps real state id to state index"""

    data.to(GeneralConfig.DEVICE)
    reversed_state_map = {v: k for k, v in state_map.items()}

    with torch.no_grad():
        out = model(
            data.x_dict["game_vertex"],
            data.x_dict["state_vertex"],
            data.edge_index_dict["game_vertex_to_game_vertex"],
            data["game_vertex_to_game_vertex"].edge_type,
            data["game_vertex_history_state_vertex"].edge_index,
            data["game_vertex_history_state_vertex"].edge_attr,
            data["game_vertex_in_state_vertex"].edge_index,
            data["state_vertex_parent_of_state_vertex"].edge_index,
        )

    remapped = []

    for index, vector in enumerate(out["state_vertex"]):
        state_vector_mapping = StateVectorMapping(
            state=reversed_state_map[index],
            vector=(vector.detach().cpu().numpy()).tolist(),
        )
        remapped.append(state_vector_mapping)

    return max(remapped, key=lambda mapping: sum(mapping.vector)).state, out


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
