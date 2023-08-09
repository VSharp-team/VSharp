namespace VSharp.Interpreter.IL

open System.Diagnostics
open System.IO
open VSharp.IL
open VSharp.IL.Serializer
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
    
[<Struct>]
type Oracle =
    val Predict: GameState -> uint*float
    val Feedback: Feedback -> unit
    new (predict, feedback) = {Predict=predict; Feedback = feedback}

type SiliOptions = {
    explorationMode : explorationMode
    outputDirectory : DirectoryInfo
    recThreshold : uint
    timeout : int
    solverTimeout : int
    visualize : bool
    releaseBranches : bool
    maxBufferSize : int
    checkAttributes : bool
    stopOnCoverageAchieved : int
    oracle: Option<Oracle>
    coverageToSwitchToAI: uint
    stepsToPlay: uint
    serialize: bool
    pathToSerialize: string
    randomSeed : int
    stepsLimit : uint
}
