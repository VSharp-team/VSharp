module VSharp.ML.GameServer.Messages

open System.Collections.Generic
open System.Text.Json
open System.Text.Json.Serialization
open VSharp.IL.Serializer
open VSharp.Interpreter.IL

[<Struct>]
type RawInputMessage =
    val MessageType: string
    val MessageBody: string
    [<JsonConstructor>]
    new (messageType, messageBody) = {MessageBody = messageBody; MessageType = messageType}

[<Struct>]
type RawOutgoingMessage =
    val MessageType: string
    val MessageBody: string
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

let toSiliZone zone =
    match zone with
    | CoverageZone.Method -> coverageZone.MethodZone
    | CoverageZone.Class -> coverageZone.ClassZone
    
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
        let str = UTF8.toString messageData
        str |> JsonSerializer.Deserialize<RawInputMessage>
    match rawInputMessage.MessageType with
    | MsgTypeStart -> Start (JsonSerializer.Deserialize<GameStartParams> rawInputMessage.MessageBody)
    | MsgTypeStep -> Step (JsonSerializer.Deserialize<GameStep> rawInputMessage.MessageBody)
    | MsgGetAllMaps -> GetAllMaps

let serializeOutgoingMessage (message:OutgoingMessage) =
    match message with
    | GameOver -> RawOutgoingMessage("GameOver", "")
    | Maps maps -> RawOutgoingMessage("Maps", JsonSerializer.Serialize maps)
    | MoveReward reward -> RawOutgoingMessage("MoveReward", JsonSerializer.Serialize reward)
    | IncorrectPredictedStateId stateId -> RawOutgoingMessage("IncorrectPredictedStateId", JsonSerializer.Serialize stateId)
    | ReadyForNextStep state -> RawOutgoingMessage("ReadyForNextStep", JsonSerializer.Serialize state)
    | ServerError errorMessage -> RawOutgoingMessage("ServerError", errorMessage)
    |> JsonSerializer.Serialize
        

   