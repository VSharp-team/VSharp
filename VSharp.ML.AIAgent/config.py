import logging
from dataclasses import dataclass
from pathlib import Path
from shutil import rmtree

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


@dataclass(slots=True, frozen=True)
class DumpByTimeoutFeature:
    enabled: bool
    timeout_seconds: int
    save_path: Path

    def create_save_path_if_not_exists(self):
        if self.enabled:
            if self.save_path.exists():
                rmtree(self.save_path)
            self.save_path.mkdir()


@dataclass(slots=True, frozen=True)
class SaveEpochsCoveragesFeature:
    enabled: bool
    save_path: Path

    def create_save_path_if_not_exists(self):
        if self.enabled:
            if self.save_path.exists():
                rmtree(self.save_path)
            self.save_path.mkdir()


class FeatureConfig:
    VERBOSE_TABLES = True
    SHOW_SUCCESSORS = True
    NAME_LEN = 7
    N_BEST_SAVED_EACH_GEN = 2
    DISABLE_MESSAGE_CHECKS = True
    DUMP_BY_TIMEOUT = DumpByTimeoutFeature(
        enabled=True, timeout_seconds=1200, save_path=Path("./report/timeouted_agents/")
    )
    SAVE_EPOCHS_COVERAGES = SaveEpochsCoveragesFeature(
        enabled=True, save_path=Path("./report/epochs_tables/")
    )
