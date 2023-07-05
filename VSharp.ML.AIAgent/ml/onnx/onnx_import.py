import json

import onnx
import onnxruntime
import torch

from common.constants import DUMMY_INPUT_PATH
from common.game import GameState
from ml.data_loader_compact import ServerDataloaderHeteroVector


def create_dummy_hetero_data():
    with open(DUMMY_INPUT_PATH, "r") as dummy_file:
        dummy_input = json.load(dummy_file)
    dummy_input = GameState.from_json(dummy_input)
    hetero_data, _ = ServerDataloaderHeteroVector.convert_input_to_tensor(dummy_input)

    return hetero_data


def create_onnx_dummy_input():
    hetero_data = create_dummy_hetero_data()

    return {
        "x_dict": hetero_data.x_dict,
        "edge_index_dict": hetero_data.edge_index_dict,
    }


def create_onnxruntime_dummy_input():
    hetero_data = create_dummy_hetero_data()

    return {
        "game_vertex": hetero_data.x_dict["game_vertex"],
        "state_vertex": hetero_data.x_dict["state_vertex"],
        "gv2gv": hetero_data.edge_index_dict[("game_vertex", "to", "game_vertex")],
        "sv_in_gv": hetero_data.edge_index_dict[("state_vertex", "in", "game_vertex")],
        "gv_in_sv": hetero_data.edge_index_dict[("game_vertex", "in", "state_vertex")],
        "sv_his_gv": hetero_data.edge_index_dict[
            ("state_vertex", "history", "game_vertex")
        ],
        "gv_his_sv": hetero_data.edge_index_dict[
            ("game_vertex", "history", "state_vertex")
        ],
        "sv_parentof_sv": hetero_data.edge_index_dict[
            ("state_vertex", "parent_of", "state_vertex")
        ],
    }


def create_torch_dummy_input():
    hetero_data = create_dummy_hetero_data()
    return hetero_data.x_dict, hetero_data.edge_index_dict


def export_onnx_model(model: torch.nn.Module, save_path: str):
    torch.onnx.export(
        model=model,
        args=create_onnx_dummy_input(),
        f=save_path,
        verbose=False,
        export_params=True,
        input_names=["x_dict", "edge_index_dict"],
        # input_names=[
        #     "game_vertex",
        #     "state_vertex",
        #     "gv2gv",
        #     "sv_in_gv",
        #     "gv_in_sv",
        #     "sv_his_gv",
        #     "gv_his_sv",
        #     "sv_parentof_sv",
        # ],
    )

    torch_model_out = model(*create_torch_dummy_input())
    check_onnx_model(save_path, check_against=torch_model_out)


def check_onnx_model(path: str, check_against):
    model = onnx.load(path)
    onnx.checker.check_model(model)
    print(onnx.helper.printable_graph(model.graph))

    # ort_session = onnxruntime.InferenceSession(path)
    # ort_inputs = create_onnxruntime_dummy_input()
    # ort_outs = ort_session.run(None, ort_inputs)

    # print(ort_outs == check_against)

    print("ONNX model loaded succesfully")
