from agent.connection_manager import ConnectionManager
from agent.n_agent import get_validation_maps
from common.constants import Constant
from logger.setup import setup_loggers
from ml.model_wrappers.genetic_learner import GeneticLearner
from ml.mutation_gen import MutationProportions, Mutator, MutatorConfig
from ml.utils import load_full_model
from r_learn import r_learn


def main():
    setup_loggers()
    socket_urls = [Constant.DEFAULT_GAMESERVER_URL]
    cm = ConnectionManager(socket_urls)

    loaded_model = load_full_model(Constant.IMPORTED_FULL_MODEL_PATH)

    epochs = 30
    max_steps = 250
    n_models = 15

    GeneticLearner.set_model(loaded_model, 8)
    models = [GeneticLearner() for _ in range(n_models)]

    maps = get_validation_maps(cm)
    mutator_config = MutatorConfig(
        proportions=MutationProportions(
            n_tops=4,
            average_of_n_tops=1,
            average_of_all=1,
            mutate_average_of_n_tops=2,
            mutate_average_of_all=2,
        ),
        mutation_volume=0.25,
        mutation_freq=0.1,
    )

    r_learn(
        epochs, max_steps, models, maps, Mutator(mutator_config, GeneticLearner), cm
    )

    cm.close()


if __name__ == "__main__":
    main()
