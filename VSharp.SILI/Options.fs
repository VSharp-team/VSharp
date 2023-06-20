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

type coverageZone =
    | MethodZone
    | ClassZone
    | ModuleZone

type explorationMode =
    | TestCoverageMode of coverageZone * searchMode
    | StackTraceReproductionMode of StackTrace

type SiliOptions = {
    explorationMode : explorationMode
    outputDirectory : DirectoryInfo
    recThreshold : uint32
    timeout : int
    solverTimeout : int
    visualize : bool
    releaseBranches : bool
    maxBufferSize : int
    checkAttributes : bool
    stopOnCoverageAchieved : int
}
