namespace VSharp.Explorer

open System.Diagnostics
open System.IO
open VSharp.ML.GameServer.Messages

type searchMode =
    | DFSMode
    | BFSMode
    | ShortestDistanceBasedMode
    | RandomShortestDistanceBasedMode
    | ContributedCoverageMode
    | ExecutionTreeMode
    | FairMode of searchMode
    | InterleavedMode of searchMode * int * searchMode * int
    | AIMode

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

[<Struct>]
type Oracle =
    val Predict: GameState -> uint<stateId>
    val Feedback: Feedback -> unit
    new (predict, feedback) = {Predict=predict; Feedback = feedback}

/// <summary>
/// Options used in AI agent training.
/// </summary>
/// <param name="stepsToSwitchToAI">Number of steps of default searcher prior to switch to AI mode.</param>
/// <param name="stepsToPlay">Number of steps to play in AI mode.</param>
/// <param name="defaultSearchStrategy">Default searcher that will be used to play few initial steps.</param>
/// <param name="serializeSteps">Determine whether steps should be serialized.</param>
/// <param name="mapName">Name of map to play.</param>
/// <param name="mapName">Name of map to play.</param>
type AIAgentTrainingOptions =
    {
        stepsToSwitchToAI: uint<step>
        stepsToPlay: uint<step>
        defaultSearchStrategy: searchMode
        serializeSteps: bool
        mapName: string
        oracle: Option<Oracle>
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
    aiAgentTrainingOptions: Option<AIAgentTrainingOptions>
    pathToModel: Option<string>
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
