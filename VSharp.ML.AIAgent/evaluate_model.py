from contextlib import closing

from common.game import MoveReward
from agent.connection_manager import ConnectionManager
from agent.n_agent import NAgent
from agent.connection_manager import ConnectionManager
from ml.models import GCN


def main():
    socket_urls = ["ws://0.0.0.0:8080/gameServer"]
    cm = ConnectionManager(socket_urls)

    model = GCN(hidden_channels=64)
    chosen_map = 0
    steps = 10

    with closing(NAgent(cm, map_id_to_play=chosen_map, steps=steps, log=True)) as agent:
        cumulative_reward = MoveReward(0, 0)
        steps_count = 0
        try:
            for _ in range(steps):
                game_state = agent.recv_state_or_throw_gameover()
                # next_step_id = model.predict(input=game_state)
                next_step_id = 0
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


if __name__ == "__main__":
    main()
