from ml.data_loader_compact import ServerDataloaderHeteroVector
from ml.models.TAGSageSimple.model import StateModelEncoder
from ml.het_gnn_test_train import HetGNNTestTrain


def get_data_hetero_vector():
    dl = ServerDataloaderHeteroVector("../serialized")
    return dl.dataset


if __name__ == "__main__":
    # get_data_hetero_vector()
    pr = HetGNNTestTrain(StateModelEncoder, 32)
    pr.train_and_save("../dataset", 20, "./ml/models/")
