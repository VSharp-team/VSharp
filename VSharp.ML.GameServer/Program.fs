open System.IO
open Suave
open Suave.Operators
open Suave.Filters
open Suave.Logging
open Suave.Sockets
open Suave.Sockets.Control
open Suave.WebSocket
open VSharp.Interpreter.IL
open VSharp.ML.GameServer.Messages
open VSharp.Runner
   
let ws (webSocket : WebSocket) (context: HttpContext) =
  socket {
    let mutable loop = true
    while loop do
        let! msg = webSocket.read()
        match msg with
        | (Text, data, true) ->
                let message = deserializeInputMessage data
                match message with
                | Start (gameMapName, coverageToStartGame) ->
                    let settings = mapsSettings.[gameMapName]
                    let assembly = RunnerProgram.ResolveAssembly <| FileInfo settings.AssemblyFullName
                    match settings.CoverageZone with
                    | MethodZone ->
                        let method = RunnerProgram.ResolveMethod(assembly, settings.NameOfObjectToCover)
                        VSharp.TestGenerator.Cover(method) |> ignore
                    | ClassZone ->
                        let _type = RunnerProgram.ResolveType(assembly, settings.NameOfObjectToCover)
                        VSharp.TestGenerator.Cover(_type) |> ignore
                    | x -> failwithf $"Unexpected coverage zone %A{x}"
                    
                | Step (stateToMove, predictedUsefulness) -> ()
                
                let response = sprintf "response to %s" ""
                let byteResponse =
                          response
                          |> System.Text.Encoding.UTF8.GetBytes
                          |> ByteSegment
                do! webSocket.send Text byteResponse true
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