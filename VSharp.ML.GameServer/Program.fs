open System.IO
open System.Text.Json
open Microsoft.FSharp.Core
open Suave
open Suave.Operators
open Suave.Filters
open Suave.Logging
open Suave.Sockets
open Suave.Sockets.Control
open Suave.WebSocket
open VSharp.IL.Serializer
open VSharp.Interpreter.IL
open VSharp.ML.GameServer.Messages
open VSharp.Runner
   
let ws (webSocket : WebSocket) (context: HttpContext) =
  
  socket {
    let mutable loop = true
    let oracle = fun (gameState:GameState) ->
        let res = 
            socket {
                let byteResponse =
                    System.Text.Json.JsonSerializer.Serialize gameState
                    |> System.Text.Encoding.UTF8.GetBytes
                    |> ByteSegment
                do! webSocket.send Text byteResponse true
                let! msg = webSocket.read()
                let res = 
                    match msg with
                    | (Text, data, true) ->
                        let msg = deserializeInputMessage data
                        match msg with
                        | Step stepParams -> (stepParams.StateId, stepParams.PredictedStateUsefulness)
                        | _ -> failwithf $"Unexpected message: %A{msg}"
                    | _ -> failwithf $"Unexpected message: %A{msg}"
                return res
            }
        match Async.RunSynchronously res with
        | Choice1Of2 (i, f) -> (i,f)
        | Choice2Of2 error -> failwithf $"Error: %A{error}"
        
    (*let provideReward =
        fun (stepReward: int, maxPossibleReward: int) ->
            socket {
                let res = webSocket.send
                ()
            }
            |> Async.StartAsTask |> fun x -> x.
      *)  
    let sendResponse (responseString:string) =
        let byteResponse =
            responseString
            |> System.Text.Encoding.UTF8.GetBytes
            |> ByteSegment
        webSocket.send Text byteResponse true
        
    while loop do
        let! msg = webSocket.read()
        match msg with
        | (Text, data, true) ->
                let message = deserializeInputMessage data
                match message with
                | GetAllMaps ->
                    do! sendResponse (JsonSerializer.Serialize (Array.ofSeq mapsSettings.Values))
                | Start gameStartParams ->
                    let settings = mapsSettings.[gameStartParams.MapId]
                    let assembly = RunnerProgram.ResolveAssembly <| FileInfo settings.AssemblyFullName
                    match settings.CoverageZone with
                    | MethodZone ->
                        let method = RunnerProgram.ResolveMethod(assembly, settings.NameOfObjectToCover)
                        VSharp.TestGenerator.Cover(method, oracle = oracle) |> ignore
                    | ClassZone ->
                        let _type = RunnerProgram.ResolveType(assembly, settings.NameOfObjectToCover)
                        VSharp.TestGenerator.Cover(_type, oracle = oracle) |> ignore
                    | x -> failwithf $"Unexpected coverage zone: %A{x}"
                | x -> failwithf $"Unexpected message: %A{x}"    
                
                                
                do! sendResponse ""
                
        | (Close, _, _) ->
                let emptyResponse = [||] |> ByteSegment
                do! webSocket.send Close emptyResponse true
                loop <- false
        | _ -> ()
    }
  
let app: WebPart =
    choose [
        path "/gameServer" >=> handShake ws
    ]
    
[<EntryPoint>]
let main _ =
    (*let ws1 = new Socket(AddressFamily.InterNetworkV6, SocketType.Stream, ProtocolType.Tcp)
    let adr = IPAddress.IPv6Any
    let endpoint = IPEndPoint(adr,8080)
    ws1.Bind(endpoint)
    ws1.Listen(100)
    let handler = ws1.Accept() 
    while true do
        handler.Receive()  
    *)
    startWebServer {defaultConfig with logger = Targets.create Verbose [||]} app
    (*    let r =
            "response string"
            |> System.Text.Encoding.UTF8.GetBytes
            |> ByteSegment
        let x = (ws1.Send(r))
        ()
    *)
    0