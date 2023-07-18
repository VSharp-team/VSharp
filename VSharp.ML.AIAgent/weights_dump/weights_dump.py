import json

from numpy import typing as npt


def save_weights(w: npt.NDArray, to: str):
    with open(to / f"{sum(w)}.txt", "w+") as weights_file:
        json.dump(list(w), weights_file)
