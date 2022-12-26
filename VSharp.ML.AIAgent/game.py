from dataclasses import dataclass
from dataclasses_json import dataclass_json
from typing import List


@dataclass_json
@dataclass
class State:
    Id: int
    Position: int
    PredictedUsefulness: float
    PathConditionSize: int
    VisitedAgainVertices: int
    VisitedNotCoveredVerticesInZone: int
    VisitedNotCoveredVerticesOutOfZone: int


@dataclass_json
@dataclass
class GameMapVertex:
    Uid: int
    Id: int
    InCoverageZone: bool
    BasicBlockSize: int
    CoveredByTest: bool
    VisitedByState: bool
    TouchedByState: bool
    States: List[State]


@dataclass_json
@dataclass
class GameEdgeLabel:
    Token: int


@dataclass_json
@dataclass
class GameMapEdge:
    VertexFrom: GameMapVertex
    VertexTo: GameMapVertex
    Label: GameEdgeLabel


@dataclass_json
@dataclass
class GameState:
    Map: List[GameMapEdge]


@dataclass_json
@dataclass
class Reward:
    StepReward: int
    MaxPossibleReward: int


@dataclass_json
@dataclass
class GameMap:
    Id: int
    CoverageToStart: int
    AssemblyFullName: str
    CoverageZone: bool
    NameOfObjectToCover: str
