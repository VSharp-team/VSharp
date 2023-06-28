import os
import pathlib
from shutil import rmtree

from common.constants import Constant


def append_to_tables_file(s: str):
    with open(Constant.TABLES_LOG_FILE, "a") as file:
        file.write(s)


def clean_tables_file():
    open(Constant.TABLES_LOG_FILE, "w").close()


def clean_log_file():
    open(Constant.APP_LOG_FILE, "w").close()


EPOCH_BEST_DIR = "./epochs_best"


def create_epochs_best_dir():
    dir_to_create = EPOCH_BEST_DIR
    if os.path.exists(dir_to_create):
        rmtree(dir_to_create)
    os.mkdir(dir_to_create)


def create_epoch_subdir(epoch_num) -> pathlib.Path:
    new_dir = pathlib.Path(EPOCH_BEST_DIR) / f"epoch_{epoch_num}"
    os.mkdir(new_dir)
    return new_dir
