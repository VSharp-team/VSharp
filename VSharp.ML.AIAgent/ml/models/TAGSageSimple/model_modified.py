from torch_geometric.nn import Linear
from torch.nn.functional import softmax
from .model import StateModelEncoder


class StateModelEncoderLastLayer(StateModelEncoder):
    def __init__(self, hidden_channels, out_channels):
        super().__init__(hidden_channels, out_channels)
        self.lin_last = Linear(out_channels, 1)

    def forward(
        self,
        game_x,
        state_x,
        edge_index_v_v,
        edge_type_v_v,
        edge_index_history_v_s,
        edge_attr_history_v_s,
        edge_index_in_v_s,
        edge_index_s_s,
    ):
        return softmax(
            self.lin_last(
                super().forward(
                    game_x=game_x,
                    state_x=state_x,
                    edge_index_v_v=edge_index_v_v,
                    edge_type_v_v=edge_type_v_v,
                    edge_index_history_v_s=edge_index_history_v_s,
                    edge_attr_history_v_s=edge_attr_history_v_s,
                    edge_index_in_v_s=edge_index_in_v_s,
                    edge_index_s_s=edge_index_s_s,
                )
            ),
            dim=0,
        )
