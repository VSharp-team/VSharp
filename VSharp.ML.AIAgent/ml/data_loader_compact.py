import argparse
import json
import os.path
from os import walk

import numpy as np
import torch
from common.game import GameState
from torch_geometric.data import HeteroData

NUM_NODE_FEATURES = 6
EXPECTED_FILENAME = "expectedResults.txt"
GAMESUFFIX = "_gameState"
STATESUFFIX = "_statesInfo"


class ServerDataloaderHeteroVector:
    def __init__(self, data_dir):
        self.data_dir = data_dir
        self.graph_types_and_data = {}
        self.dataset = []
        self.process_directory(data_dir)
        self.__process_files()

    @staticmethod
    def convert_input_to_tensor(input: GameState) -> tuple[HeteroData, dict[int, int]]:
        """
        Converts game env to tensors
        """
        graphVertices = input.GraphVertices
        game_states = input.States
        game_edges = input.Map
        data = HeteroData()
        nodes_vertex = []
        nodes_state = []
        edges_index_v_v = []
        edges_index_s_s = []
        edges_index_s_v_in = []
        edges_index_v_s_in = []
        edges_index_s_v_history = []
        edges_index_v_s_history = []
        edges_attr_v_v = []

        state_map: dict[int, int] = {}  # Maps real state id to its index
        vertex_map: dict[int, int] = {}  # Maps real vertex id to its index
        vertex_index = 0
        state_index = 0

        # vertex nodes
        for vertex in graphVertices:
            if vertex.Id in vertex_map:
                continue
            vertex_map[vertex.Id] = vertex_index  # maintain order in tensors
            nodes_vertex.append(
                np.array(
                    [
                        int(vertex.InCoverageZone),
                        vertex.BasicBlockSize,
                        int(vertex.CoveredByTest),
                        int(vertex.VisitedByState),
                        int(vertex.TouchedByState),
                    ]
                )
            )
            vertex_index += 1
        # vertex -> vertex edges
        for e in game_edges:
            edges_index_v_v.append(
                np.array([vertex_map[e.VertexFrom], vertex_map[e.VertexTo]])
            )
            edges_attr_v_v.append(
                np.array([e.Label.Token])
            )  # TODO: consider token in a model

        # state nodes
        for state in game_states:
            if state.Id in state_map:
                continue
            state_map[state.Id] = state_index
            nodes_state.append(
                np.array(
                    [
                        state.Position,
                        state.PredictedUsefulness,
                        state.PathConditionSize,
                        state.VisitedAgainVertices,
                        state.VisitedNotCoveredVerticesInZone,
                        state.VisitedNotCoveredVerticesOutOfZone,
                    ]
                )
            )
            # history edges: state -> vertex and back
            for h in state.History:  # TODO: process NumOfVisits as edge label
                v_to = vertex_map[h.GraphVertexId]
                edges_index_s_v_history.append(np.array([state_index, v_to]))
                edges_index_v_s_history.append(np.array([v_to, state_index]))
            state_index += 1

        # state and its childen edges: state -> state
        for state in game_states:
            for child_id in state.Children:
                edges_index_s_s.append(
                    np.array([state_map[state.Id], state_map[child_id]])
                )

        # state position edges: vertex -> state and back
        for vertex in graphVertices:
            for state in vertex.States:
                edges_index_s_v_in.append(
                    np.array([state_map[state], vertex_map[vertex.Id]])
                )
                edges_index_v_s_in.append(
                    np.array([vertex_map[vertex.Id], state_map[state]])
                )

        data["game_vertex"].x = torch.tensor(np.array(nodes_vertex), dtype=torch.float)
        data["state_vertex"].x = torch.tensor(np.array(nodes_state), dtype=torch.float)
        data["game_vertex", "to", "game_vertex"].edge_index = (
            torch.tensor(np.array(edges_index_v_v), dtype=torch.long).t().contiguous()
        )
        data["state_vertex", "in", "game_vertex"].edge_index = (
            torch.tensor(np.array(edges_index_s_v_in), dtype=torch.long)
            .t()
            .contiguous()
        )
        data["game_vertex", "in", "state_vertex"].edge_index = (
            torch.tensor(np.array(edges_index_v_s_in), dtype=torch.long)
            .t()
            .contiguous()
        )
        data["state_vertex", "history", "game_vertex"].edge_index = (
            torch.tensor(np.array(edges_index_s_v_history), dtype=torch.long)
            .t()
            .contiguous()
        )
        data["game_vertex", "history", "state_vertex"].edge_index = (
            torch.tensor(np.array(edges_index_v_s_history), dtype=torch.long)
            .t()
            .contiguous()
        )
        if edges_index_s_s:
            data["state_vertex", "parent_of", "state_vertex"].edge_index = (
                torch.tensor(np.array(edges_index_s_s), dtype=torch.long)
                .t()
                .contiguous()
            )
        return data, state_map

    @staticmethod
    def get_expected_value(file_path: str, state_map: dict[int, int]) -> torch.Tensor:
        """Get tensor for states"""
        expected = {}
        with open(file_path) as f:
            data = json.load(f)
            state_set = set()
            for d in data:
                sid = d["StateId"]
                if sid not in state_set:
                    state_set.add(sid)
                    values = list(d.values())
                    expected[values[0]] = np.array(values[1:])
        ordered = []
        ordered_by_index = list(zip(*sorted(state_map.items(), key=lambda x: x[1])))[0]
        for k in ordered_by_index:
            ordered.append(expected[k])
        return torch.tensor(np.array(ordered), dtype=torch.float)

    def process_directory(self, data_dir):
        example_dirs = next(walk(data_dir), (None, [], None))[1]
        example_dirs.sort()
        print(example_dirs)
        for fldr in example_dirs:
            fldr_path = os.path.join(data_dir, fldr)
            graphs_to_convert = []
            for f in os.listdir(fldr_path):
                if GAMESUFFIX in f:
                    graphs_to_convert.append(f)
            graphs_to_convert.sort(key=lambda x: int(x.split("_")[0]))
            self.graph_types_and_data[fldr] = graphs_to_convert

    def __process_files(self):
        for k, v in self.graph_types_and_data.items():
            for file in v:
                with open(os.path.join(self.data_dir, k, file)) as f:
                    print(os.path.join(self.data_dir, k, file))
                    data = json.load(f)
                    graph, state_map = self.convert_input_to_tensor(
                        GameState.from_dict(data)
                    )
                    if graph is not None:
                        # add_expected values
                        expected = self.get_expected_value(
                            os.path.join(
                                self.data_dir, k, file.split("_")[0] + STATESUFFIX
                            ),
                            state_map,
                        )
                        graph.y = expected
                        self.dataset.append(graph)


def parse_cmd_line_args():
    parser = argparse.ArgumentParser(
        prog="V# pytorch-geometric data conversion", description="Symbolic execution"
    )
    parser.add_argument("--dataset", required=True, help="Dataset folder")
    parser.add_argument(
        "--mode", help="heterogeneous or homogeneous graph model (het|hom)"
    )


def get_data_hetero_vector():
    dl = ServerDataloaderHeteroVector("../../GNN_V#/Serialized_almost_all")
    return dl.dataset
