from agent.connection_manager import ConnectionManager
from agent.n_agent import get_validation_maps
from ml.mutation_gen import MutationProportions, MutatorConfig, Mutator
from ml.utils import load_full_model
from r_learn import r_learn
from ml.model_wrappers.genetic_learner import GeneticLearner


def main():
    socket_urls = ["ws://0.0.0.0:8080/gameServer"]
    cm = ConnectionManager(socket_urls)

    loaded_model = load_full_model(
        "VSharp.ML.AIAgent/ml/imported/GNN_state_pred_het_full"
    )

    epochs = 20
    max_steps = 1000
    n_models = 10

    GeneticLearner.set_model(loaded_model, 8)
    models = [GeneticLearner() for _ in range(n_models)]

    maps = get_validation_maps(cm)
    mutator_config = MutatorConfig(
        proportions=MutationProportions(
            n_tops=4,
            averaged_n_tops=1,
            n_averaged_all=1,
            random_n_tops_averaged_mutations=2,
            random_all_averaged_mutations=2,
        ),
        mutation_volume=0.25,
        mutation_freq=0.02,
    )

    r_learn(
        epochs, max_steps, models, maps, Mutator(mutator_config, GeneticLearner), cm
    )

    cm.close()


if __name__ == "__main__":
    main()
