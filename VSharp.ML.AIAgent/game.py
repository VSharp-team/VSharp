from dataclasses import dataclass
from dataclasses_json import dataclass_json


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
    History: list[int]
    Children: list[int]

    def __hash__(self) -> int:
        return self.Id.__hash__()


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
    States: list[State]


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
    Map: list[GameMapEdge]


@dataclass_json
@dataclass
class GameMap:
    Id: int
    CoverageToStart: int
    AssemblyFullName: str
    CoverageZone: bool
    NameOfObjectToCover: str
