import logging

import ml.models


class GeneralConfig:
    SERVER_COUNT = 8
    MAX_STEPS = 3000
    LOGGER_LEVEL = logging.INFO
    MODEL_INIT = lambda: ml.models.SAGEConvModel(16)


class BrokerConfig:
    BROKER_PORT = 8080


class ServerConfig:
    VSHARP_INSTANCES_START_PORT = 8100


class FeatureConfig:
    VERBOSE_TABLES = True
    SHOW_SUCCESSORS = True
    NAME_LEN = 7
    N_BEST_SAVED_EACH_GEN = 2
