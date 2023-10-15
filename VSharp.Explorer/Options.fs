namespace VSharp.Explorer

open System.Diagnostics
open System.IO

type searchMode =
    | DFSMode
    | BFSMode
    | ShortestDistanceBasedMode
    | RandomShortestDistanceBasedMode
    | ContributedCoverageMode
    | ExecutionTreeMode
    | FairMode of searchMode
    | InterleavedMode of searchMode * int * searchMode * int

type coverageZone =
    | MethodZone
    | ClassZone
    | ModuleZone

type explorationMode =
    | TestCoverageMode of coverageZone * searchMode
    | StackTraceReproductionMode of StackTrace

type fuzzerIsolation =
    | Process

type FuzzerOptions = {
    isolation: fuzzerIsolation
    coverageZone: coverageZone
}

type SVMOptions = {
    explorationMode : explorationMode
    recThreshold : uint
    solverTimeout : int
    visualize : bool
    releaseBranches : bool
    maxBufferSize : int
    prettyChars : bool // If true, 33 <= char <= 126, otherwise any char
    checkAttributes : bool
    stopOnCoverageAchieved : int
    randomSeed : int
    stepsLimit : uint
}

type explorationModeOptions =
    | Fuzzing of FuzzerOptions
    | SVM of SVMOptions
    | Combined of SVMOptions * FuzzerOptions

type ExplorationOptions = {
    timeout : System.TimeSpan
    outputDirectory : DirectoryInfo
    explorationModeOptions : explorationModeOptions
}
with
    member this.fuzzerOptions =
        match this.explorationModeOptions with
        | Fuzzing x -> x
        | Combined (_, x) -> x
        | _ -> failwith ""

    member this.svmOptions =
        match this.explorationModeOptions with
        | SVM x -> x
        | Combined (x, _) -> x
        | _ -> failwith ""

    member this.coverageZone =
        match this.explorationModeOptions with
        | SVM x ->
            match x.explorationMode with
            | TestCoverageMode (coverageZone, _) -> coverageZone 
            | StackTraceReproductionMode _ -> failwith ""
        | Combined (_, x) -> x.coverageZone
        | Fuzzing x -> x.coverageZone
