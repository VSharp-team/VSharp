import json
import os
from shutil import rmtree

from common.constants import TEMP_EPOCH_INFERENCE_TIMES_DIR

from .wrapper import EPOCH_TIMES, MAP_TIMES


def create_temp_epoch_inference_dir():
    dir_to_create = TEMP_EPOCH_INFERENCE_TIMES_DIR
    if os.path.exists(dir_to_create):
        rmtree(dir_to_create)
    os.mkdir(dir_to_create)


def remove_temp_epoch_inference_dir():
    dir_to_remove = TEMP_EPOCH_INFERENCE_TIMES_DIR
    if os.path.exists(dir_to_remove):
        rmtree(dir_to_remove)


def get_map_inference_times() -> list[float]:
    times = MAP_TIMES
    return times


def dump_and_reset_epoch_times(file_postfix: str):
    with open(
        TEMP_EPOCH_INFERENCE_TIMES_DIR / f"inference_times_{file_postfix}.json", "w+"
    ) as inference_times_file:
        json.dump(EPOCH_TIMES, inference_times_file)
        EPOCH_TIMES.clear()


def load_times_array() -> list[float]:
    times = []
    for file in os.listdir(TEMP_EPOCH_INFERENCE_TIMES_DIR):
        with open(TEMP_EPOCH_INFERENCE_TIMES_DIR / file, "r") as inference_stats_file:
            data: list[float] = json.load(inference_stats_file)
        times += data

    return times
