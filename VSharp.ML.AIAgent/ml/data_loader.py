import argparse
import json
import os.path
from torch_geometric.data import Data
from os import walk
from typing import Dict
import torch
import numpy as np

from common.game import GameState

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
    def get_expected_values(fldr_path: str) -> Dict[int, int]:
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
                    graph = self.convert_input_to_tensor(GameState.from_dict(data))
                    # add_expected values
                    graph.y = self.graph_types_and_expected[k][int(file)]
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


def parse_cmd_line_args():
    parser = argparse.ArgumentParser(
        prog="V# pytorch-geometric data conversion", description="Symbolic execution"
    )
    parser.add_argument("--dataset", required=True, help="Dataset folder")
    parser.add_argument(
        "--mode", help="heterogeneous or homogeneous graph model (het|hom)"
    )


def get_data_hetero(path: str):
    dl = ServerDataloaderHetero(data_dir=path)
    return dl.dataset
