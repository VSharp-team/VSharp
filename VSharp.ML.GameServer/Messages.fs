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
    new (stateId, predictedUsefulness) = {StateId = stateId; PredictedStateUsefulness = predictedUsefulness}
        
type InputMessage =
    | GetAllMaps 
    | Start of GameStartParams
    | Step of GameStep    

[<Struct>]
type State =
    val Id: uint
    val Position: uint
    val PredictedUsefulness: float
    val PathConditionSize: uint
    val VisitedAgainVertices: uint
    val VisitedNotCoveredVerticesInZone: uint
    val VisitedNotCoveredVerticesOutOfZone: uint
    val History: array<uint>
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
    
[<Struct>]
type GameMap =
    val Id: uint
    val CoverageToStart: uint
    val AssemblyFullName: string
    val CoverageZone: CoverageZone
    val NameOfObjectToCover: string
    new (id, coverageToStart, assembly, coverageZone, objectToCover) =
        {
            Id = id
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
    
let mapsSettings =
    let d = Dictionary<_,_>()
    let add =
        let mutable firstFreeMapId = 0u
        fun coverageToStart pathToDll coverageZone objectToCover ->
            d.Add(firstFreeMapId, GameMap(firstFreeMapId, coverageToStart, pathToDll, coverageZone, objectToCover))
            firstFreeMapId <- firstFreeMapId + 1u
    
    add 0u  "VSharp.ML.GameMaps.dll" CoverageZone.Method "BinarySearch"
    add 25u "VSharp.ML.GameMaps.dll" CoverageZone.Method "BinarySearch"
    add 50u "VSharp.ML.GameMaps.dll" CoverageZone.Method "BinarySearch"
    
    add 0u  "VSharp.ML.GameMaps.dll" CoverageZone.Method "Switches1"
    add 25u "VSharp.ML.GameMaps.dll" CoverageZone.Method "Switches1"
    add 50u "VSharp.ML.GameMaps.dll" CoverageZone.Method "Switches1"
    
    add 0u  "VSharp.ML.GameMaps.dll" CoverageZone.Method "Switches2"
    add 25u "VSharp.ML.GameMaps.dll" CoverageZone.Method "Switches2"
    add 50u "VSharp.ML.GameMaps.dll" CoverageZone.Method "Switches2"
    
    add 0u  "VSharp.ML.GameMaps.dll" CoverageZone.Method "NestedFors"
    add 25u "VSharp.ML.GameMaps.dll" CoverageZone.Method "NestedFors"
    add 50u "VSharp.ML.GameMaps.dll" CoverageZone.Method "NestedFors"
    
    add 0u  "VSharp.ML.GameMaps.dll" CoverageZone.Class "KMPSearch"
    add 20u "VSharp.ML.GameMaps.dll" CoverageZone.Class "KMPSearch"
    add 40u "VSharp.ML.GameMaps.dll" CoverageZone.Class "KMPSearch"
    add 60u "VSharp.ML.GameMaps.dll" CoverageZone.Class "KMPSearch"
    
    add 0u  "VSharp.ML.GameMaps.dll" CoverageZone.Class "AhoCorasick"
    add 20u "VSharp.ML.GameMaps.dll" CoverageZone.Class "AhoCorasick"
    add 40u "VSharp.ML.GameMaps.dll" CoverageZone.Class "AhoCorasick"
    add 60u "VSharp.ML.GameMaps.dll" CoverageZone.Class "AhoCorasick"
    
    add 0u  "VSharp.ML.GameMaps.dll" CoverageZone.Method "BellmanFord"
    add 20u "VSharp.ML.GameMaps.dll" CoverageZone.Method "BellmanFord"
    add 40u "VSharp.ML.GameMaps.dll" CoverageZone.Method "BellmanFord"
    add 60u "VSharp.ML.GameMaps.dll" CoverageZone.Method "BellmanFord"
    
    add 0u "VSharp.ML.GameMaps.dll" CoverageZone.Method "AhoCorasickMain"
    add 0u "VSharp.ML.GameMaps.dll" CoverageZone.Method "KMPSearchMain"
    
    add 0u  "VSharp.ML.GameMaps.dll" CoverageZone.Method "KruskalMST"
    add 20u "VSharp.ML.GameMaps.dll" CoverageZone.Method "KruskalMST"
    add 40u "VSharp.ML.GameMaps.dll" CoverageZone.Method "KruskalMST"
    add 60u "VSharp.ML.GameMaps.dll" CoverageZone.Method "KruskalMST"
       
    d

let (|MsgTypeStart|MsgTypeStep|MsgGetAllMaps|) (str:string) =
    let normalized = str.ToLowerInvariant().Trim()
    if normalized = "start"
    then MsgTypeStart
    elif normalized = "step"
    then MsgTypeStep
    elif normalized = "getallmaps"
    then MsgGetAllMaps
    else failwithf $"Unexpected message type %s{str}"
    
let deserializeInputMessage (messageData:byte[]) =
    let rawInputMessage =
        let str = Encoding.UTF8.GetString messageData
        str |> JsonSerializer.Deserialize<RawInputMessage>
    match rawInputMessage.MessageType with
    | MsgTypeStart -> Start (JsonSerializer.Deserialize<GameStartParams> rawInputMessage.MessageBody)
    | MsgTypeStep -> Step (JsonSerializer.Deserialize<GameStep> rawInputMessage.MessageBody)
    | MsgGetAllMaps -> GetAllMaps

let serializeOutgoingMessage (message:OutgoingMessage) =
    match message with
    | GameOver -> RawOutgoingMessage("GameOver", box (GameOverMessageBody()))
    | Maps maps -> RawOutgoingMessage("Maps", MapsMessageBody maps)
    | MoveReward reward -> RawOutgoingMessage("MoveReward", reward)
    | IncorrectPredictedStateId stateId -> RawOutgoingMessage("IncorrectPredictedStateId", IncorrectPredictedStateIdMessageBody stateId)
    | ReadyForNextStep state -> RawOutgoingMessage("ReadyForNextStep", state)
    | ServerError errorMessage -> RawOutgoingMessage("ServerError", ServerErrorMessageBody errorMessage)
    |> JsonSerializer.Serialize
        

   