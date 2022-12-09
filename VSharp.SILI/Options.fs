namespace VSharp.Interpreter.IL

open System.Diagnostics
open System.IO

type searchMode =
    | DFSMode
    | BFSMode
    | ShortestDistanceBasedMode
    | RandomShortestDistanceBasedMode
    | ContributedCoverageMode
    | FairMode of searchMode
    | InterleavedMode of searchMode * int * searchMode * int
    | ConcolicMode of searchMode
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
    outputDirectory : DirectoryInfo
    recThreshold : uint32
    timeout : int
    visualize : bool
    releaseBranches : bool
    maxBufferSize : int
    checkAttributes : bool
    collectContinuousDump : bool
    stopOnCoverageAchieved : int
}
