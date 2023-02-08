from contextlib import closing
import argparse

from common.game import MoveReward
from common.utils import compute_coverage_percent
from agent.connection_manager import ConnectionManager
from agent.n_agent import NAgent
from ml.models import GCN
from ml.utils import load_model
from ml.predict_state_hetero import PredictStateHetGNN
from ml.data_loader import ServerDataloaderHetero


def main():
    parser = argparse.ArgumentParser()
    parser.add_argument("--map_id", type=int, help="game map id", default=0)
    parser.add_argument("--steps", type=int, help="amount of steps", default=10)
    args = parser.parse_args()

    socket_urls = ["ws://0.0.0.0:8080/gameServer"]
    cm = ConnectionManager(socket_urls)

    model = GCN(hidden_channels=64)
    model = load_model(model, "<path/to/model>")

    chosen_map = args.map_id
    steps = args.steps

    with closing(NAgent(cm, map_id_to_play=chosen_map, steps=steps, log=True)) as agent:
        cumulative_reward = MoveReward(0, 0)
        steps_count = 0
        try:
            for _ in range(steps):
                game_state = agent.recv_state_or_throw_gameover()
                next_step_id = PredictStateHetGNN.predict_state(
                    model, ServerDataloaderHetero.convert_input_to_tensor(game_state)
                )
                agent.send_step(
                    next_state_id=next_step_id,
                    predicted_usefullness=42.0,  # left it a constant for now
                )

                reward = agent.recv_reward_or_throw_gameover()
                cumulative_reward += reward.ForMove
                steps_count += 1

            _ = agent.recv_state_or_throw_gameover()  # wait for gameover
            steps_count += 1

        except NAgent.GameOver:
            pass

        model_result = (cumulative_reward, steps_count)

    print("=" * 80)
    print(f"Reward: {model_result[0]}")
    print(f"Steps: {model_result[1]}")
    print(
        f"Coverage: {compute_coverage_percent(game_state, model_result[0].ForCoverage):.2f}"
    )


if __name__ == "__main__":
    main()
