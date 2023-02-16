module VSharp.ML.GameServer.Messages

open System.Collections.Generic
open System.Text
open System.Text.Json
open System.Text.Json.Serialization

[<Struct>]
type RawInputMessage =
    val MessageType: string
    val MessageBody: string
    [<JsonConstructor>]
    new (messageType, messageBody) = {MessageBody = messageBody; MessageType = messageType}

type IRawOutgoingMessageBody = interface end

type GameOverMessageBody () =    
     interface IRawOutgoingMessageBody    
    
[<Struct>]
type RawOutgoingMessage =
    val MessageType: string
    val MessageBody: obj //IRawOutgoingMessageBody
    new (messageType, messageBody) = {MessageBody = messageBody; MessageType = messageType}
       
[<Struct>]
type GameStartParams =
    val MapId: uint
    val StepsToPlay: uint
    
    [<JsonConstructor>]
    new (mapId, stepsToPlay) = {MapId = mapId; StepsToPlay = stepsToPlay}

[<Struct>]
type GameStep =
    val StateId: uint
    val PredictedStateUsefulness: float
    
    [<JsonConstructor>]
    new (stateId, predictedStateUsefulness) = {StateId = stateId; PredictedStateUsefulness = predictedStateUsefulness}
        
type InputMessage =
    | GetTrainMaps
    | GetValidationMaps 
    | Start of GameStartParams
    | Step of GameStep    

[<Struct>]
type StateHistoryElem =
    val GraphVertexId: uint
    val NumOfVisits: uint
    new (graphVertexId, numOfVisits) =
        {
            GraphVertexId = graphVertexId
            NumOfVisits = numOfVisits
        }

[<Struct>]
type State =
    val Id: uint
    val Position: uint
    val PredictedUsefulness: float
    val PathConditionSize: uint
    val VisitedAgainVertices: uint
    val VisitedNotCoveredVerticesInZone: uint
    val VisitedNotCoveredVerticesOutOfZone: uint
    val History: array<StateHistoryElem>
    val Children: array<uint> 
    new(id,
        position,
        predictedUsefulness,
        pathConditionSize,
        visitedAgainVertices,
        visitedNotCoveredVerticesInZone,
        visitedNotCoveredVerticesOutOfZone,
        history,
        children) =
        {
            Id = id
            Position = position
            PredictedUsefulness = predictedUsefulness
            PathConditionSize = pathConditionSize
            VisitedAgainVertices = visitedAgainVertices
            VisitedNotCoveredVerticesInZone = visitedNotCoveredVerticesInZone
            VisitedNotCoveredVerticesOutOfZone = visitedNotCoveredVerticesOutOfZone
            History = history
            Children = children
        }
    
[<Struct>]    
type GameMapVertex =
    val Uid: uint
    val Id: uint
    val InCoverageZone: bool
    val BasicBlockSize: uint
    val CoveredByTest: bool
    val VisitedByState: bool
    val TouchedByState: bool
    val States: State[]
    new (uid,
         id,
         inCoverageZone,
         basicBlockSize,
         coveredByTest,
         visitedByState,
         touchedByState,
         states) =
        {
            Uid = uid
            Id = id
            InCoverageZone = inCoverageZone
            BasicBlockSize = basicBlockSize
            CoveredByTest = coveredByTest
            VisitedByState = visitedByState
            TouchedByState = touchedByState
            States = states
        }

[<Struct>]
type GameEdgeLabel =
    val Token: int
    new (token) = {Token = token}

[<Struct>]
type GameMapEdge =
    val VertexFrom: GameMapVertex
    val VertexTo: GameMapVertex
    val Label: GameEdgeLabel
    new (vFrom, vTo, label) = {VertexFrom = vFrom; VertexTo = vTo; Label = label}
    
[<Struct>]
type GameState =
    interface IRawOutgoingMessageBody
    val Map: GameMapEdge[]
    new (map) = {Map = map}
    
type [<Measure>] coverageReward
type [<Measure>] visitedInstructionsReward
type [<Measure>] maxPossibleReward
    
[<Struct>]
type MoveReward =
    val ForCoverage: uint<coverageReward>
    val ForVisitedInstructions: uint<visitedInstructionsReward>
    new (forCoverage, forVisitedInstructions) = {ForCoverage = forCoverage; ForVisitedInstructions = forVisitedInstructions}
[<Struct>]    

type Reward =
    interface IRawOutgoingMessageBody
    val ForMove: MoveReward
    val MaxPossibleReward: uint<maxPossibleReward>
    new (forMove, maxPossibleReward) = {ForMove = forMove; MaxPossibleReward = maxPossibleReward}
    new (forCoverage, forVisitedInstructions, maxPossibleReward) = {ForMove = MoveReward(forCoverage,forVisitedInstructions); MaxPossibleReward = maxPossibleReward}

type Feedback =
    | MoveReward of Reward
    | IncorrectPredictedStateId of uint
    | ServerError of string

type CoverageZone =
    | Method = 0
    | Class = 1

type [<Measure>] step
type [<Measure>] percent

[<Struct>]
type GameMap =
    val Id: uint
    val MaxSteps: uint<step>
    val CoverageToStart: uint<percent>
    val AssemblyFullName: string
    val CoverageZone: CoverageZone
    val NameOfObjectToCover: string
    new (id, maxSteps, coverageToStart, assembly, coverageZone, objectToCover) =
        {
            Id = id
            MaxSteps = maxSteps
            CoverageToStart = coverageToStart
            AssemblyFullName = assembly
            CoverageZone = coverageZone
            NameOfObjectToCover = objectToCover
        }

[<Struct>]
 type ServerErrorMessageBody =
     interface IRawOutgoingMessageBody
     val ErrorMessage: string
     new (errorMessage) = {ErrorMessage = errorMessage}

[<Struct>]
 type MapsMessageBody =
     interface IRawOutgoingMessageBody
     val Maps: array<GameMap>
     new (maps) = {Maps = maps}
     
     
[<Struct>]
 type IncorrectPredictedStateIdMessageBody =
     interface IRawOutgoingMessageBody
     val StateId: uint
     new (stateId) = {StateId = stateId}     
 
type OutgoingMessage =
    | GameOver
    | Maps of seq<GameMap>
    | MoveReward of Reward
    | IncorrectPredictedStateId of uint
    | ReadyForNextStep of GameState
    | ServerError of string
    
let trainMaps, validationMaps =
    let trainMaps = Dictionary<_,_>()
    let validationMaps = Dictionary<_,_>()
    
    let add' (maps:Dictionary<_,_>) =
        let mutable firstFreeMapId = 0u
        fun maxSteps coverageToStart pathToDll coverageZone objectToCover ->
            maps.Add(firstFreeMapId, GameMap(firstFreeMapId, maxSteps, coverageToStart, pathToDll, coverageZone, objectToCover))
            firstFreeMapId <- firstFreeMapId + 1u
    
    let add = add' trainMaps 10000000u<step>
    add 0u<percent>  "VSharp.ML.GameMaps.dll" CoverageZone.Method "BinarySearch"
    add 25u<percent> "VSharp.ML.GameMaps.dll" CoverageZone.Method "BinarySearch"
    add 50u<percent> "VSharp.ML.GameMaps.dll" CoverageZone.Method "BinarySearch"
    add 75u<percent> "VSharp.ML.GameMaps.dll" CoverageZone.Method "BinarySearch"
    
    add 0u<percent>  "VSharp.ML.GameMaps.dll" CoverageZone.Method "Switches1"
    add 15u<percent> "VSharp.ML.GameMaps.dll" CoverageZone.Method "Switches2"
    // too slow
    //add 25u "VSharp.ML.GameMaps.dll" CoverageZone.Method "Switches1"
    // too slow
    //add 50u "VSharp.ML.GameMaps.dll" CoverageZone.Method "Switches1"
    
    add 0u<percent>  "VSharp.ML.GameMaps.dll" CoverageZone.Method "Switches2"
    add 15u<percent> "VSharp.ML.GameMaps.dll" CoverageZone.Method "Switches2"
    // too slow
    //add 25u "VSharp.ML.GameMaps.dll" CoverageZone.Method "Switches2"
    // too slow
    //add 50u "VSharp.ML.GameMaps.dll" CoverageZone.Method "Switches2"
    
    add 0u<percent>  "VSharp.ML.GameMaps.dll" CoverageZone.Method "NestedFors"
    add 25u<percent> "VSharp.ML.GameMaps.dll" CoverageZone.Method "NestedFors"
    
    add 0u<percent>  "VSharp.ML.GameMaps.dll" CoverageZone.Class "KMPSearch"
    add 20u<percent> "VSharp.ML.GameMaps.dll" CoverageZone.Class "KMPSearch"
        
    add 0u<percent>  "VSharp.ML.GameMaps.dll" CoverageZone.Method "SearchWords"
    // too slow
    //add 20u "VSharp.ML.GameMaps.dll" CoverageZone.Method "SearchWords"
    // too slow
    //add 40u "VSharp.ML.GameMaps.dll" CoverageZone.Method "SearchWords"
    // too slow
    //add 60u "VSharp.ML.GameMaps.dll" CoverageZone.Method "SearchWords"
    
    add 0u<percent>  "VSharp.ML.GameMaps.dll" CoverageZone.Method "BellmanFord"
    add 20u<percent> "VSharp.ML.GameMaps.dll" CoverageZone.Method "BellmanFord"
    add 40u<percent> "VSharp.ML.GameMaps.dll" CoverageZone.Method "BellmanFord"
    add 60u<percent> "VSharp.ML.GameMaps.dll" CoverageZone.Method "BellmanFord"
    
    add 0u<percent>  "VSharp.ML.GameMaps.dll" CoverageZone.Method "bsPartition"
    add 20u<percent> "VSharp.ML.GameMaps.dll" CoverageZone.Method "bsPartition"
    add 40u<percent> "VSharp.ML.GameMaps.dll" CoverageZone.Method "bsPartition"
    add 60u<percent> "VSharp.ML.GameMaps.dll" CoverageZone.Method "bsPartition"
    
    //Moved to verify 
    //add 10u  "VSharp.ML.GameMaps.dll" CoverageZone.Method "multiply_matrix"
    //too slow
    //add 20u "VSharp.ML.GameMaps.dll" CoverageZone.Method "multiply_matrix"
    //add 40u "VSharp.ML.GameMaps.dll" CoverageZone.Method "multiply_matrix"
    //add 60u "VSharp.ML.GameMaps.dll" CoverageZone.Method "multiply_matrix"
    
    add 0u<percent>  "VSharp.ML.GameMaps.dll" CoverageZone.Method "mergeSort"
    add 20u<percent> "VSharp.ML.GameMaps.dll" CoverageZone.Method "mergeSort"
    //25
    add 40u<percent> "VSharp.ML.GameMaps.dll" CoverageZone.Method "mergeSort"
    
    add 0u<percent>  "VSharp.ML.GameMaps.dll" CoverageZone.Method "matrixInverse"
    add 25u<percent> "VSharp.ML.GameMaps.dll" CoverageZone.Method "matrixInverse"
    
    add 0u<percent>  "VSharp.ML.GameMaps.dll" CoverageZone.Method "adjoint"
    add 20u<percent> "VSharp.ML.GameMaps.dll" CoverageZone.Method "adjoint"
    add 50u<percent>  "VSharp.ML.GameMaps.dll" CoverageZone.Method "adjoint"
    
    add 0u<percent>  "VSharp.ML.GameMaps.dll" CoverageZone.Method "determinant"
    add 50u<percent> "VSharp.ML.GameMaps.dll" CoverageZone.Method "determinant"
    
    add 0u<percent>  "VSharp.ML.GameMaps.dll" CoverageZone.Method "getCofactor"
    add 50u<percent> "VSharp.ML.GameMaps.dll" CoverageZone.Method "getCofactor"
    
    add 0u<percent>  "VSharp.ML.GameMaps.dll" CoverageZone.Method "fillRemaining"
    add 20u<percent> "VSharp.ML.GameMaps.dll" CoverageZone.Method "fillRemaining"
    add 40u<percent> "VSharp.ML.GameMaps.dll" CoverageZone.Method "fillRemaining"
    //too slow
    //add 80u "VSharp.ML.GameMaps.dll" CoverageZone.Method "fillRemaining"
    
    add 0u<percent>  "VSharp.ML.GameMaps.dll" CoverageZone.Method "solveWordWrap"
    add 20u<percent> "VSharp.ML.GameMaps.dll" CoverageZone.Method "solveWordWrap"
    add 80u<percent> "VSharp.ML.GameMaps.dll" CoverageZone.Method "solveWordWrap"
    
    add 0u<percent>  "VSharp.ML.GameMaps.dll" CoverageZone.Method "waysToIncreaseLCSBy1"
    add 30u<percent> "VSharp.ML.GameMaps.dll" CoverageZone.Method "waysToIncreaseLCSBy1"
    add 70u<percent> "VSharp.ML.GameMaps.dll" CoverageZone.Method "waysToIncreaseLCSBy1"
    
    add 0u<percent>  "VSharp.ML.GameMaps.dll" CoverageZone.Method "BinaryMaze1BFS"
    add 30u<percent> "VSharp.ML.GameMaps.dll" CoverageZone.Method "BinaryMaze1BFS"
    add 70u<percent> "VSharp.ML.GameMaps.dll" CoverageZone.Method "BinaryMaze1BFS"
    
    add 0u<percent>  "VSharp.ML.GameMaps.dll" CoverageZone.Method "findShortestPathLength"
    add 30u<percent> "VSharp.ML.GameMaps.dll" CoverageZone.Method "findShortestPathLength"
    add 80u<percent> "VSharp.ML.GameMaps.dll" CoverageZone.Method "findShortestPathLength"
    
    add 0u<percent>  "VSharp.ML.GameMaps.dll" CoverageZone.Method "countIslands"
    add 30u<percent> "VSharp.ML.GameMaps.dll" CoverageZone.Method "countIslands"
    
    add 0u<percent>  "VSharp.ML.GameMaps.dll" CoverageZone.Method "MatrixQueryModifyMatrix"
    add 50u<percent> "VSharp.ML.GameMaps.dll" CoverageZone.Method "MatrixQueryModifyMatrix"
    
    add 0u<percent> "VSharp.ML.GameMaps.dll" CoverageZone.Method "AhoCorasickMain"
    add 0u<percent> "VSharp.ML.GameMaps.dll" CoverageZone.Method "KMPSearchMain"
    add 0u<percent> "VSharp.ML.GameMaps.dll" CoverageZone.Method "BinarySearchMain"
    add 0u<percent> "VSharp.ML.GameMaps.dll" CoverageZone.Method "MatrixMultiplicationMain"
    add 0u<percent> "VSharp.ML.GameMaps.dll" CoverageZone.Method "MergeSortMain"
    add 0u<percent> "VSharp.ML.GameMaps.dll" CoverageZone.Method "MatrixInverseMain"
    add 0u<percent> "VSharp.ML.GameMaps.dll" CoverageZone.Method "SudokuMain"
    add 0u<percent> "VSharp.ML.GameMaps.dll" CoverageZone.Method "WordWrapMain"
    add 0u<percent> "VSharp.ML.GameMaps.dll" CoverageZone.Method "LCSMain"
    add 0u<percent> "VSharp.ML.GameMaps.dll" CoverageZone.Method "BinaryMaze1Main"
    add 0u<percent> "VSharp.ML.GameMaps.dll" CoverageZone.Method "BinaryMaze2Main"
    add 0u<percent> "VSharp.ML.GameMaps.dll" CoverageZone.Method "IslandsMain"
    add 0u<percent> "VSharp.ML.GameMaps.dll" CoverageZone.Method "MatrixQueryMain"
    
    //add 0u  "VSharp.ML.GameMaps.dll" CoverageZone.Method "KruskalMST"
    //add 20u "VSharp.ML.GameMaps.dll" CoverageZone.Method "KruskalMST"
    //add 40u "VSharp.ML.GameMaps.dll" CoverageZone.Method "KruskalMST"
    //add 60u "VSharp.ML.GameMaps.dll" CoverageZone.Method "KruskalMST"
     
    let add = add' validationMaps
    
    add 20000u<step> 0u<percent> "VSharp.ML.GameMaps.dll" CoverageZone.Method "LoanExamBuild"
    add 1000u<step> 0u<percent> "VSharp.ML.GameMaps.dll" CoverageZone.Method "multiply_matrix"
    add 100000u<step> 0u<percent> "VSharp.ML.GameMaps.dll" CoverageZone.Method "ApplyMoveAndCheckOtherValidMoves"
    add 100000u<step> 0u<percent> "VSharp.ML.GameMaps.dll" CoverageZone.Method "CheckMate1"
    add 100000u<step> 0u<percent> "VSharp.ML.GameMaps.dll" CoverageZone.Method "CheckMoveIsValidAndApply"
          
    trainMaps, validationMaps

let (|MsgTypeStart|MsgTypeStep|MsgGetTrainMaps|MsgGetValidationMaps|) (str:string) =
    let normalized = str.ToLowerInvariant().Trim()
    if normalized = "start"
    then MsgTypeStart
    elif normalized = "step"
    then MsgTypeStep
    elif normalized = "gettrainmaps"
    then MsgGetTrainMaps
    elif normalized = "getvalidationmaps"
    then MsgGetValidationMaps
    else failwithf $"Unexpected message type %s{str}"
    
let deserializeInputMessage (messageData:byte[]) =    
    let rawInputMessage =
        let str = Encoding.UTF8.GetString messageData
        str |> JsonSerializer.Deserialize<RawInputMessage>
    match rawInputMessage.MessageType with
    | MsgTypeStart -> Start (JsonSerializer.Deserialize<GameStartParams> rawInputMessage.MessageBody)
    | MsgTypeStep -> Step (JsonSerializer.Deserialize<GameStep>(rawInputMessage.MessageBody))
    | MsgGetTrainMaps -> GetTrainMaps
    | MsgGetValidationMaps -> GetValidationMaps

let serializeOutgoingMessage (message:OutgoingMessage) =
    match message with
    | GameOver -> RawOutgoingMessage("GameOver", box (GameOverMessageBody()))
    | Maps maps -> RawOutgoingMessage("Maps", MapsMessageBody (Array.ofSeq maps))
    | MoveReward reward -> RawOutgoingMessage("MoveReward", reward)
    | IncorrectPredictedStateId stateId -> RawOutgoingMessage("IncorrectPredictedStateId", IncorrectPredictedStateIdMessageBody stateId)
    | ReadyForNextStep state -> RawOutgoingMessage("ReadyForNextStep", state)
    | ServerError errorMessage -> RawOutgoingMessage("ServerError", ServerErrorMessageBody errorMessage)
    |> JsonSerializer.Serialize
        

   