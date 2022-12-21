module VSharp.ML.GameServer.Messages

open System.Collections.Generic
open System.Dynamic
open System.Text.Json.Serialization
open VSharp
open VSharp.Interpreter.IL

[<Struct>]
type RawInputMessage =
    val MessageType: string
    val MessageBody: string
    [<JsonConstructor>]
    new (messageType, messageBody) = {MessageBody = messageBody; MessageType = messageType}
    
type CoverageZone =
    | Method = 0
    | Class = 1
    
let toSiliZone zone =
    match zone with
    | CoverageZone.Method -> coverageZone.MethodZone
    | CoverageZone.Class -> coverageZone.ClassZone
    
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
type GameStartParams =
    val MapId: uint
    val StepsToPlay: uint
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
    d.Add(0u, GameMap(0u,0u,"path_to_dll.0",CoverageZone.Method,"method_to_test_0"))
    d.Add(1u, GameMap(1u,0u,"path_to_dll.1",CoverageZone.Method,"method_to_test_1"))
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
        let str = UTF8.toString messageData
        str |> System.Text.Json.JsonSerializer.Deserialize<RawInputMessage>
    match rawInputMessage.MessageType with
    | MsgTypeStart -> Start (System.Text.Json.JsonSerializer.Deserialize<GameStartParams> rawInputMessage.MessageBody)
    | MsgTypeStep -> Step (System.Text.Json.JsonSerializer.Deserialize<GameStep> rawInputMessage.MessageBody)
    | MsgGetAllMaps -> GetAllMaps


   