from dataclasses import dataclass


@dataclass
class Name2ResultViewModel:
    model_name: str
    pretty_result: str


@dataclass
class Interval:
    left: float
    right: float

    def pretty(self):
        return f"{self.left:.2f}%-{self.right:.2f}%, diff={self.right - self.left:.2f}%"


@dataclass
class Name2Stats:
    mutable_name: str
    av_coverage: float
    interval: Interval


@dataclass
class CoverageStats:
    euc_dist2_full_cov: float
    average_cov: float
    median_cov: float
    interval: Interval
