import os
import pathlib
from shutil import rmtree

from common.constants import (
    APP_LOG_FILE,
    BASE_REPORT_DIR,
    EPOCH_BEST_DIR,
    LEADERS_TABLES_LOG_FILE,
    TABLES_LOG_FILE,
)


def rewrite_best_tables_file(s: str):
    with open(LEADERS_TABLES_LOG_FILE, "w") as file:
        file.write(s)


def init_leader_tables_file():
    open(LEADERS_TABLES_LOG_FILE, "w").close()


def append_to_tables_file(s: str):
    with open(TABLES_LOG_FILE, "a") as file:
        file.write(s)


def init_tables_file():
    open(TABLES_LOG_FILE, "w").close()


def init_log_file():
    open(APP_LOG_FILE, "w").close()


def init_epochs_best_dir():
    dir_to_create = EPOCH_BEST_DIR
    if os.path.exists(dir_to_create):
        rmtree(dir_to_create)
    os.mkdir(dir_to_create)


def create_epoch_subdir(epoch_num) -> pathlib.Path:
    new_dir = EPOCH_BEST_DIR / f"epoch_{epoch_num}"
    os.mkdir(new_dir)
    return new_dir


def create_report_dir():
    if BASE_REPORT_DIR.exists():
        rmtree(BASE_REPORT_DIR)
    BASE_REPORT_DIR.mkdir()
