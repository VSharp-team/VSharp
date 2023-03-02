import argparse
import json
import os.path
from os import walk
from typing import Dict, Tuple

import numpy as np
import torch
from common.game import GameState
from torch_geometric.data import Data, HeteroData

# NUM_NODE_FEATURES = 49
NUM_NODE_FEATURES = 6
EXPECTED_FILENAME = "expectedResults.txt"


class DataLoader:  # TODO: inheritance and more ways to load (different predictions)
    def __init__(self, data_dir):
        self.data_dir = data_dir
        self.graph_types_and_expected: Dict[
            str, Dict[int, float]
        ] = {}  # Dict[example name, Dict[graph №, expected]]
        self.graph_types_and_data = {}
        self.dataset = []
        self.process_directory(data_dir)
        self.__process_files()

    def process_directory(self, data_dir):
        example_dirs = next(walk(data_dir), (None, [], None))[1]
        print(example_dirs)
        for fldr in example_dirs:
            fldr_path = os.path.join(data_dir, fldr)
            graphs_to_convert = []
            for f in os.listdir(fldr_path):
                if f != EXPECTED_FILENAME:
                    graphs_to_convert.append(f)
                else:
                    self.graph_types_and_expected[fldr] = self.get_expected_values(
                        fldr_path
                    )
            graphs_to_convert.sort(key=lambda x: int(x))
            self.graph_types_and_data[fldr] = graphs_to_convert

    def __process_files(self):
        for k, v in self.graph_types_and_data.items():
            for file in v:
                print(os.path.join(self.data_dir, k, file))
                graph = self.convert_file_to_graph_homo(
                    os.path.join(self.data_dir, k, file),
                    self.graph_types_and_expected[k][int(file)],
                )
                self.dataset.append(graph)

    @staticmethod
    def get_expected_values(fldr_path: str) -> Dict[int, float]:
        """Get TotalReachableRewardFromCurrentState for every graph
        Headers: GraphID ExpectedStateNumber ExpectedRewardForStep TotalReachableRewardFromCurrentState
        """
        expected = {}
        with open(os.path.join(fldr_path, EXPECTED_FILENAME)) as f:
            next(f)
            for line in f:
                split = line.split()
                expected[int(split[0])] = int(split[-1])
        return expected

    @staticmethod
    def convert_file_to_graph_homo(filepath, expected) -> Data:
        """File headers:
        Nodes: #VertexId InCoverageZone BasicBlockSize CoveredByTest
          VisitedByState TouchedByState (State_i_ID State_i_Position)*
        Edges: #VertexFrom VertexTo Terminal(0-CFG, 1-Call, 2-Return)"""
        nodes = []
        edges = []
        edge_attr_ = []
        with open(filepath) as f:
            parse_edges = False
            for line in f:
                if "#" not in line:  # skip headers
                    if not parse_edges:  # parse nodes
                        arr = np.zeros(NUM_NODE_FEATURES)
                        split = np.array(line.split()[1:])
                        arr[0 : split.size] = split
                        nodes.append(arr)
                    else:  # parse edges
                        split = list(map(lambda x: int(x), line.split()))
                        edges.append(split[:-1])
                        edge_attr_.append(split[2])
                else:
                    if "#Edges" in line:
                        parse_edges = True
        x = torch.tensor(np.array(nodes), dtype=torch.float)
        edge_index = torch.tensor(edges, dtype=torch.long)
        edge_attr = torch.tensor(np.array(edge_attr_), dtype=torch.long)
        data = Data(
            x=x, edge_index=edge_index.t().contiguous(), edge_attr=edge_attr, y=expected
        )
        return data


class ServerDataloaderHetero(DataLoader):
    def __init__(self, data_dir):
        self.data_dir = data_dir
        self.graph_types_and_expected: Dict[
            str, Dict[int, np.array]
        ] = {}  # Dict[example name, Dict[graph №, expected]]
        self.graph_types_and_data = {}
        self.dataset = []
        self.process_directory(data_dir)
        self.__process_files()

    def __process_files(self):
        for k, v in self.graph_types_and_data.items():
            for file in v:
                with open(os.path.join(self.data_dir, k, file)) as f:
                    print(os.path.join(self.data_dir, k, file))
                    data = json.load(f)
                    graph, state_map = self.convert_input_to_tensor(
                        GameState.from_dict(data)
                    )
                    # add_expected values
                    expected = self.graph_types_and_expected[k][int(file)][0]
                    graph.y = state_map[expected]
                self.dataset.append(graph)

    def get_expected_values(self, fldr_path: str) -> Dict[int, np.array]:
        """Get TotalReachableRewardFromCurrentState for every graph
        Headers: GraphID ExpectedStateNumber ExpectedRewardForCoveredInStep ExpectedRewardForVisitedInstructionsInStep
        TotalReachableRewardFromCurrentState"""
        expected = {}
        with open(os.path.join(fldr_path, EXPECTED_FILENAME)) as f:
            next(f)
            for line in f:
                split = line.split()
                expected[int(split[0])] = np.array(split[1:], dtype=int)
        return expected

    @staticmethod
    def convert_input_to_tensor(input: GameState) -> Tuple[HeteroData, Dict[int, int]]:
        """
        Converts game env to tensors and state_map
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

        state_map: Dict[int, int] = {}  # Maps real state id to its index
        vertex_map: Dict[int, int] = {}  # Maps real vertex id to its index
        vertex_index = 0
        state_index = 0
        for m in mp:
            # process vertices
            vertex_from, vertex_to = m.VertexFrom.__dict__, m.VertexTo.__dict__
            for v in [vertex_from, vertex_to]:
                vertex_id = int(v["Id"])
                if vertex_id not in nodes_vertex_set:
                    nodes_vertex_set.add(vertex_id)
                    vertex_map[vertex_id] = vertex_index
                    vertex_index = vertex_index + 1
                    nodes_vertex.append(
                        np.array(
                            [
                                int(v["InCoverageZone"]),
                                v["BasicBlockSize"],
                                int(v["CoveredByTest"]),
                                int(v["VisitedByState"]),
                                int(v["TouchedByState"]),
                            ]
                        )
                    )
            # proccess edge
            edges_index_v_v.append(
                np.array(
                    [
                        vertex_map[int(vertex_from["Id"])],
                        vertex_map[int(vertex_to["Id"])],
                    ]
                )
            )
            edges_attr_v_v.append(np.array([m.Label.Token]))
        # dealing with states
        # nodes_number = 0 #unique id for every node!
        states_info = {}
        for m in mp:
            vertex_from, vertex_to = m.VertexFrom.__dict__, m.VertexTo.__dict__
            for v in [vertex_from, vertex_to]:
                vertex_id = v["Id"]
                states = v["States"]
                if states:  # proccess states independently
                    for s in states:
                        dct = s.__dict__
                        sid = int(dct["Id"])
                        states_info[sid] = dct
                        if sid not in nodes_state_set:
                            nodes_state_set.add(sid)
                            state_map[sid] = state_index
                            state_index = state_index + 1
                            nodes_state.append(
                                np.array(
                                    [
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
                            edges_index_s_v.append(
                                np.array([state_map[sid], vertex_map[vertex_id]])
                            )
                            edges_attr_s_v.append(np.array(1))
                            edges_index_v_s.append(
                                np.array([vertex_map[vertex_id], state_map[sid]])
                            )
                            edges_attr_v_s.append(np.array(1))
                            # children edges
                            history = dct["History"]
                            if history:
                                for h in history:
                                    edges_index_s_v.append(
                                        np.array(
                                            [
                                                state_map[sid],
                                                vertex_map[int(h.GraphVertexId)],
                                            ]
                                        )
                                    )
                                    edges_attr_s_v.append(np.array(0))
        # children edges --- add after all states
        for k, v in states_info.items():
            children = v["Children"]
            if children:
                for c in children:
                    edges_index_s_s.append(np.array([state_map[k], state_map[int(c)]]))

        # nodes_vertex =  sorted(nodes_vertex, key=itemgetter(0))
        # nodes_state = sorted(nodes_state, key=itemgetter(0))
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
                torch.tensor(np.array(edges_index_s_s), dtype=torch.long)
                .t()
                .contiguous()
            )
            # print(data['state', 'parent_of', 'state'].edge_index)
        data["game_vertex", "to", "game_vertex"].edge_attr = torch.tensor(
            np.array(edges_attr_v_v), dtype=torch.long
        )
        data["state_vertex", "to", "game_vertex"].edge_attr = torch.tensor(
            np.array(edges_attr_s_v), dtype=torch.long
        )

        # data.state_map = state_map
        return data, state_map


def parse_cmd_line_args():
    parser = argparse.ArgumentParser(
        prog="V# pytorch-geometric data conversion", description="Symbolic execution"
    )
    parser.add_argument("--dataset", required=True, help="Dataset folder")
    parser.add_argument(
        "--mode", help="heterogeneous or homogeneous graph model (het|hom)"
    )


def get_data():
    dl = DataLoader("../../GNN_V#/")
    return dl.dataset


def get_data_hetero():
    dl = ServerDataloaderHetero("../../../GNN_V#/Serialized_test_simple")
    return dl.dataset


if __name__ == "__main__":
    get_data_hetero()
