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
open VSharp
open VSharp.IL.Serializer
open VSharp.Interpreter.IL
open VSharp.ML.GameServer.Messages
open VSharp.Runner
   
let ws (webSocket : WebSocket) (context: HttpContext) =
  
  socket {
    let mutable loop = true
    
    let sendResponse (responseString:string) =
        let byteResponse =
            responseString
            |> System.Text.Encoding.UTF8.GetBytes
            |> ByteSegment
        webSocket.send Text byteResponse true
        
    let oracle =
        let feedback =
            fun (feedback: Reward) ->
                let res =
                    socket {
                        do! sendResponse (JsonSerializer.Serialize feedback) 
                    }
                match Async.RunSynchronously res with
                | Choice1Of2 () -> ()
                | Choice2Of2 error -> failwithf $"Error: %A{error}"
                
        let predict =
            fun (gameState:GameState) ->
                let res = 
                    socket {
                        let byteResponse =
                            JsonSerializer.Serialize gameState
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
        
        Oracle(predict,feedback)
        
    while loop do
        let! msg = webSocket.read()
        match msg with
        | (Text, data, true) ->
                let message = deserializeInputMessage data
                match message with
                | GetAllMaps ->
                    do! sendResponse (JsonSerializer.Serialize mapsSettings.Values)
                | Start gameStartParams ->
                    let settings = mapsSettings.[gameStartParams.MapId]
                    let assembly = RunnerProgram.ResolveAssembly <| FileInfo settings.AssemblyFullName
                    match settings.CoverageZone with
                    | CoverageZone.Method ->
                        let method = RunnerProgram.ResolveMethod(assembly, settings.NameOfObjectToCover)
                        VSharp.TestGenerator.Cover(method, oracle = oracle, searchStrategy=SearchStrategy.AI, coverageToSwitchToAI = settings.CoverageToStart) |> ignore                        
                    | CoverageZone.Class ->
                        let _type = RunnerProgram.ResolveType(assembly, settings.NameOfObjectToCover)
                        VSharp.TestGenerator.Cover(_type, oracle = oracle, searchStrategy=SearchStrategy.AI, coverageToSwitchToAI = settings.CoverageToStart) |> ignore
                    | x -> failwithf $"Unexpected coverage zone: %A{x}"
                    do! sendResponse "GameOver" ///!!!
                | x -> failwithf $"Unexpected message: %A{x}"
                
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
    startWebServer {defaultConfig with logger = Targets.create Verbose [||]} app
    0