import argparse
from contextlib import closing, suppress

from agent.connection_manager import ConnectionManager
from agent.n_agent import NAgent, get_validation_maps
from common.game import MoveReward
from common.utils import compute_coverage_percent, get_states
from constants import Constant
from logger.setup import global_logger, setup_loggers
from ml.data_loader import ServerDataloaderHetero
from ml.predict_state_hetero import PredictStateHetGNN
from ml.utils import load_full_model


def main():
    setup_loggers()
    logger = global_logger()
    parser = argparse.ArgumentParser()
    parser.add_argument("--map_id", type=int, help="game map id", default=0)
    parser.add_argument("--steps", type=int, help="amount of steps", default=10)
    args = parser.parse_args()

    socket_urls = [Constant.DEFAULT_GAMESERVER_URL]
    cm = ConnectionManager(socket_urls)
    maps = get_validation_maps(cm)

    model = load_full_model(Constant.IMPORTED_FULL_MODEL_PATH)

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
                next_step_id, vector = PredictStateHetGNN.predict_state(
                    model, input, state_map
                )
                logger.debug(
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

    coverage_percent = compute_coverage_percent(game_state, model_result[0].ForCoverage)
    logger.info(
        f"finished: in {steps} steps "
        f"with reward: {cumulative_reward} "
        f"and coverage: "
        f"{coverage_percent:.2f}"
    )


if __name__ == "__main__":
    main()
