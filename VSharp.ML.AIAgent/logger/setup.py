import logging

from constants import Constant


def global_logger():
    logger = logging.getLogger()
    logger.setLevel(logging.DEBUG)

    ch = logging.StreamHandler()
    ch.setLevel(logging.DEBUG)
    logger.addHandler(ch)

    return logger


def table_logger():
    logger = logging.getLogger(Constant.Loggers.TABLE_LOGGER)
    logger.setLevel(logging.INFO)
    fh = logging.FileHandler(Constant.LOG_DIR / "tables_global.log", mode="w")

    ch = logging.StreamHandler()
    ch.setLevel(logging.INFO)
    logger.addHandler(ch)

    fh.setLevel(logging.INFO)
    logger.addHandler(fh)

    return logger


def setup_logger(name: str, with_level=logging.DEBUG):
    # create logger
    logger = logging.getLogger(name)
    logger.setLevel(with_level)

    ch = logging.StreamHandler()
    ch.setLevel(logging.DEBUG)

    fh = logging.FileHandler(Constant.LOG_DIR / "global.log", mode="w")
    fh.setLevel(logging.DEBUG)
    logger.addHandler(fh)

    # create formatter
    formatter = logging.Formatter("%(asctime)s [%(levelname)s] %(name)s: %(message)s")

    # add formatter to ch
    ch.setFormatter(formatter)

    # add ch to logger
    logger.addHandler(ch)

    return logger


def setup_loggers():
    setup_logger(Constant.Loggers.AGENT_LOGGER, logging.INFO)
    setup_logger(Constant.Loggers.ML_LOGGER, logging.INFO)
