from agent.connection_manager import ConnectionManager
from agent.n_agent import get_validation_maps
from common.constants import Constant
from logger.setup import setup_loggers
from ml.model_wrappers.genetic_learner import GeneticLearner
from ml.utils import load_full_model
from r_learn import r_learn_iteration


def main():
    setup_loggers()
    socket_urls = [Constant.DEFAULT_GAMESERVER_URL]
    cm = ConnectionManager(socket_urls)

    loaded_model = load_full_model(Constant.IMPORTED_FULL_MODEL_PATH)

    epochs = 30
    max_steps = 250
    n_models = 15

    GeneticLearner.set_model(loaded_model, 8)
    test_model = GeneticLearner([0.69, 0.54, -0.29, 0.09, 0.1, -0.32, -0.16, 0.03])
    test_maps = get_validation_maps(cm)

    for i in range(epochs):
        r_learn_iteration([test_model], test_maps, max_steps, cm)

    cm.close()


if __name__ == "__main__":
    main()
