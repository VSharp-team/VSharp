namespace VSharp.Interpreter.IL

open System.Diagnostics

type searchMode =
    | DFSMode
    | BFSMode
    | ShortestDistanceBasedMode
    | GuidedMode of searchMode

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

type SiliOptions = {
     explorationMode : explorationMode
     executionMode : executionMode
     recThreshold : uint32
     timeout : int
     }
