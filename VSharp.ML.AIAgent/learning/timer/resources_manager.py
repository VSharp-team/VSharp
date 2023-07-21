from contextlib import contextmanager

from .utils import create_temp_epoch_inference_dir, remove_temp_epoch_inference_dir
from .wrapper import MAP_TIMES


@contextmanager
def manage_inference_stats():
    create_temp_epoch_inference_dir()
    try:
        yield
    finally:
        remove_temp_epoch_inference_dir()


@contextmanager
def manage_map_inference_times_array():
    try:
        yield
    finally:
        MAP_TIMES.clear()
