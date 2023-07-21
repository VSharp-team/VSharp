from enum import Enum


class ParentSelectionType(Enum):
    STEADY_STATE_SELECTION = "sss"
    ROULETTE_WHEEL_SELECTION = "rws"
    STOCHASTIC_UNIVERSAL_SELECTION = "sus"
    RANK_SELECTION = "rank"
    RANDOM = "random"
    TOURNAMENT = "tournament"

    def __get__(self, instance, owner):
        return self.value
