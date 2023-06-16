from enum import Enum


class MutationType(Enum):
    RANDOM = "random"
    SWAP = "swap"
    INVERSION = "inversion"
    SCRAMBLE = "scramble"
    ADAPTIVE = "adaptive"

    def __get__(self, instance, owner):
        return self.value
