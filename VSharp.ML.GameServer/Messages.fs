module VSharp.ML.GameServer.Messages

open System.Collections.Generic
open VSharp.Interpreter.IL

[<Struct>]
type RawInputMessage =
    val MessageType: string
    val MessageBody: string
    new (_type, _body) = {MessageBody = _body; MessageType = _type}
    
type GameMap =
    | LoanExam
    | BinarySearch
    
type InputMessage =
    | Start of GameMap * uint
    | Step of uint * float

[<Struct>]
type MapInitialParams =
    val AssemblyFullName: string
    val CoverageZone: coverageZone
    val NameOfObjectToCover: string
    new (assembly, coverageZone, objectToCover) =
        {
            AssemblyFullName = assembly
            CoverageZone = coverageZone
            NameOfObjectToCover = objectToCover
        }

let mapsSettings =
    let d = Dictionary<_,_>()
    d.Add(BinarySearch,MapInitialParams("",coverageZone.MethodZone,""))
    d
    
let (|MsgTypeStart|MsgTypeStep|) (str:string) =
    let normalized = str.ToLowerInvariant().Trim()
    if normalized = "start"
    then MsgTypeStart
    elif normalized = "step"
    then MsgTypeStep
    else failwithf $"Unexpected message type %s{str}"
    
let deserializeInputMessage (messageData:byte[]) =
    let rawInputMessage =
        UTF8.toString messageData
        |> System.Text.Json.JsonSerializer.Deserialize<RawInputMessage>
    match rawInputMessage.MessageType with
    | MsgTypeStart -> Start (System.Text.Json.JsonSerializer.Deserialize<GameMap * uint> rawInputMessage.MessageBody)
    | MsgTypeStep -> Step (System.Text.Json.JsonSerializer.Deserialize<uint * float> rawInputMessage.MessageBody)


   