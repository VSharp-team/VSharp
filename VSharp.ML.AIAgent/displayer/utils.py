from common.constants import Constant


def append_to_tables_file(s: str):
    with open(Constant.TABLES_LOG_FILE, "a") as file:
        file.write(s)


def clean_tables_file():
    open(Constant.TABLES_LOG_FILE, "w").close()


def clean_log_file():
    open(Constant.APP_LOG_FILE, "w").close()
