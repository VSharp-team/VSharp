namespace VSharp.Interpreter.IL

open System.Diagnostics

type searchMode =
    | DFSMode
    | BFSMode

type coverageZone =
    | MethodZone
    | ClassZone
    | ModuleZone

type explorationMode =
    | TestCoverageMode of coverageZone * searchMode
    | StackTraceReproductionMode of StackTrace

type executionMode =
    | ConcolicMode
    | SymbolicMode

type siliOptions =
    {explorationMode : explorationMode; executionMode : executionMode; bound : uint32}
