import json
from pathlib import Path

from numpy import typing as npt


def save_weights(w: npt.NDArray, to: Path):
    file_to_create = to / f"{sum(w)}.txt"
    if not file_to_create.exists():
        with open(file_to_create, "w+") as weights_file:
            json.dump(list(w), weights_file)
