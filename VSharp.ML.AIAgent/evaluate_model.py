from contextlib import closing, suppress
import argparse

from common.game import MoveReward
from common.utils import compute_coverage_percent
from agent.connection_manager import ConnectionManager
from agent.n_agent import NAgent
from ml.utils import load_full_model
from ml.model_wrappers.protocols import Predictor
from ml.model_wrappers.genetic_learner import GeneticLearner


def main():
    parser = argparse.ArgumentParser()
    parser.add_argument("--map_id", type=int, help="game map id", default=0)
    parser.add_argument("--steps", type=int, help="amount of steps", default=10)
    args = parser.parse_args()

    socket_urls = ["ws://0.0.0.0:8080/gameServer"]
    cm = ConnectionManager(socket_urls)

    model = load_full_model("VSharp.ML.AIAgent/ml/imported/GNN_state_pred_het_full")
    predictor: Predictor = GeneticLearner()
    predictor.set_model(model)

    chosen_map = args.map_id
    steps = args.steps

    with closing(NAgent(cm, map_id_to_play=chosen_map, steps=steps, log=True)) as agent:
        cumulative_reward = MoveReward(0, 0)
        steps_count = 0
        with suppress(NAgent.GameOver):
            for _ in range(steps):
                game_state = agent.recv_state_or_throw_gameover()
                next_step_id = predictor.predict(game_state)
                agent.send_step(
                    next_state_id=next_step_id,
                    predicted_usefullness=42.0,  # left it a constant for now
                )

                reward = agent.recv_reward_or_throw_gameover()
                cumulative_reward += reward.ForMove
                steps_count += 1

            _ = agent.recv_state_or_throw_gameover()  # wait for gameover
            steps_count += 1

        model_result = (cumulative_reward, steps_count)

    print("=" * 80)
    print(f"Reward: {model_result[0]}")
    print(f"Steps: {model_result[1]}")
    print(
        f"Coverage: {compute_coverage_percent(game_state, model_result[0].ForCoverage):.2f}"
    )


if __name__ == "__main__":
    main()
