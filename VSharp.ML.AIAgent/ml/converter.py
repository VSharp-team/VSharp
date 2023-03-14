from operator import itemgetter

import numpy as np
import torch
from common.game import GameState
from torch_geometric.data import HeteroData


def convert_input_to_tensor(input: GameState):
    """
    Converts game env to tensors
    input later can be changed to <GameState, History, ...>
    """
    mp = input.Map
    data = HeteroData()
    nodes_vertex_set = set()
    nodes_state_set = set()
    nodes_vertex = []
    nodes_state = []
    edges_index_v_v = []
    edges_index_s_s = []
    edges_index_s_v = []
    edges_index_v_s = []
    edges_attr_s_v = []  # 0 - history # 1 - state in
    edges_attr_v_s = []
    edges_attr_v_v = []

    for m in mp:
        # process vertices
        vertex_from, vertex_to = m.VertexFrom.__dict__, m.VertexTo.__dict__
        for v in [vertex_from, vertex_to]:
            vertex_id = v["Id"]
            if vertex_id not in nodes_vertex_set:
                nodes_vertex_set.add(vertex_id)
                nodes_vertex.append(
                    np.array(
                        [
                            vertex_id,
                            int(v["InCoverageZone"]),
                            v["BasicBlockSize"],
                            int(v["CoveredByTest"]),
                            int(v["VisitedByState"]),
                            int(v["TouchedByState"]),
                        ]
                    )
                )
        # proccess edge
        edges_index_v_v.append(np.array([vertex_from["Id"], vertex_to["Id"]]))
        edges_attr_v_v.append(np.array([m.Label.Token]))
    # dealing with states
    nodes_number = 0  # unique id for every node!
    for m in mp:
        vertex_from, vertex_to = m.VertexFrom.__dict__, m.VertexTo.__dict__
        for v in [vertex_from, vertex_to]:
            states = v["States"]
            if states:  # proccess states independently
                for s in states:
                    dct = s.__dict__
                    sid = dct["Id"] + nodes_number
                    if sid not in nodes_state_set:
                        nodes_state_set.add(sid)
                        nodes_state.append(
                            np.array(
                                [
                                    sid,
                                    dct["Position"],
                                    dct["PredictedUsefulness"],
                                    dct["PathConditionSize"],
                                    dct["VisitedAgainVertices"],
                                    dct["VisitedNotCoveredVerticesInZone"],
                                    dct["VisitedNotCoveredVerticesOutOfZone"],
                                ]
                            )
                        )
                        # fix state position
                        edges_index_s_v.append(np.array([sid, vertex_id]))
                        edges_attr_s_v.append(np.array(1))
                        edges_index_v_s.append(np.array([vertex_id, sid]))
                        edges_attr_v_s.append(np.array(1))
                        # history edges
                        history = dct["History"]
                        if history:
                            for h in history:
                                edges_index_s_v.append(np.array([sid, h]))
                                edges_attr_s_v.append(np.array(0))
                        # children edges
                        children = dct["Children"]
                        if children:
                            for c in children:
                                edges_index_s_s.append(
                                    np.array([sid, c + nodes_number])
                                )

    nodes_vertex = sorted(nodes_vertex, key=itemgetter(0))
    nodes_state = sorted(nodes_state, key=itemgetter(0))
    data["game_vertex"].x = torch.tensor(np.array(nodes_vertex), dtype=torch.float)
    data["state_vertex"].x = torch.tensor(np.array(nodes_state), dtype=torch.float)
    data["game_vertex", "to", "game_vertex"].edge_index = (
        torch.tensor(np.array(edges_index_v_v), dtype=torch.long).t().contiguous()
    )
    data["state_vertex", "to", "game_vertex"].edge_index = (
        torch.tensor(np.array(edges_index_s_v), dtype=torch.long).t().contiguous()
    )
    data["game_vertex", "to", "state_vertex"].edge_index = (
        torch.tensor(np.array(edges_index_v_s), dtype=torch.long).t().contiguous()
    )
    if edges_index_s_s:
        data["state_vertex", "parent_of", "state_vertex"].edge_index = (
            torch.tensor(np.array(edges_index_s_s), dtype=torch.long).t().contiguous()
        )
        # print(data['state', 'parent_of', 'state'].edge_index)
    data["game_vertex", "to", "game_vertex"].edge_attr = torch.tensor(
        np.array(edges_attr_v_v), dtype=torch.long
    )
    data["state_vertex", "to", "game_vertex"].edge_attr = torch.tensor(
        np.array(edges_attr_s_v), dtype=torch.long
    )
    # print(data)
    return data
