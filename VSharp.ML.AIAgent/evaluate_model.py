from contextlib import closing, suppress
import argparse

from common.game import MoveReward
from common.utils import compute_coverage_percent
from common.utils import get_states
from agent.connection_manager import ConnectionManager
from agent.n_agent import NAgent
from agent.n_agent import get_validation_maps
from ml.utils import load_full_model
from ml.predict_state_hetero import PredictStateHetGNN
from ml.data_loader import ServerDataloaderHetero


def main():
    parser = argparse.ArgumentParser()
    parser.add_argument("--map_id", type=int, help="game map id", default=5)
    parser.add_argument("--steps", type=int, help="amount of steps", default=20000)
    args = parser.parse_args()

    socket_urls = ["ws://0.0.0.0:8080/gameServer"]
    cm = ConnectionManager(socket_urls)
    maps = get_validation_maps(cm)

    model = load_full_model("ml/imported/GNN_state_pred_het_full")

    chosen_map = args.map_id
    steps = args.steps

    with closing(NAgent(cm, map_id_to_play=chosen_map, steps=steps)) as agent:
        cumulative_reward = MoveReward(0, 0)
        steps_count = 0
        with suppress(NAgent.GameOver):
            for _ in range(steps):
                game_state = agent.recv_state_or_throw_gameover()
                input, state_map = ServerDataloaderHetero.convert_input_to_tensor(
                    game_state
                )
                next_step_id = PredictStateHetGNN.predict_state(model, input, state_map)
                print(
                    f"step: {steps_count}, available states: {get_states(game_state)}, picked: {next_step_id}"
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

        model_result = (cumulative_reward, steps_count)

    print("=" * 80)
    print(f"Reward: {model_result[0]}")
    print(f"Steps: {model_result[1]}")
    print(
        f"Coverage: {compute_coverage_percent(game_state, model_result[0].ForCoverage):.2f}"
    )


if __name__ == "__main__":
    main()
