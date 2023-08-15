from enum import Enum


class CrossoverType(Enum):
    SINGLE_POINT = "single_point"
    TWO_POINTS = "two_points"
    UNIFORM = "uniform"
    SCATTERED = "scattered"

    def __get__(self, instance, owner):
        return self.value
