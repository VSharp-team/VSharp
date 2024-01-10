open System.Collections.Generic
open System.IO
open System.Reflection
open Argu
open Microsoft.FSharp.Core
open Suave
open Suave.Operators
open Suave.Filters
open Suave.Logging
open Suave.Sockets
open Suave.Sockets.Control
open Suave.WebSocket
open VSharp
open VSharp.Core
open VSharp.Explorer
open VSharp.ML.GameServer.Messages
open VSharp.ML.GameServer.Maps
open VSharp.Runner
   
type Mode =
    | Server = 0
    | Generator = 1
type CliArguments =
    | [<Unique>] Port of int
    | [<Unique>] DatasetBasePath of string
    | [<Unique>] DatasetDescription of string
    | [<Unique; Mandatory>] Mode of Mode
    | [<Unique>] OutFolder of string
    | [<Unique>] StepsToSerialize of uint
    interface IArgParserTemplate with
        member s.Usage =
            match s with
            | Port _ -> "Port to communicate with game client."
            | DatasetBasePath _ -> "Full path to dataset root directory. Dll location is <DatasetBasePath>/<AssemblyFullName>"
            | DatasetDescription _ -> "Full paths to JSON-file with dataset description."
            | Mode _ -> "Mode to run application. Server --- to train network, Generator --- to generate data for training."
            | OutFolder _ -> "Folder to store generated data."
            | StepsToSerialize _ -> "Maximal number of steps for each method to serialize."
            
let mutable inTrainMode = true

let loadGameMaps (datasetDescriptionFilePath:string) =
    let jsonString = File.ReadAllText datasetDescriptionFilePath
    System.Text.Json.JsonSerializer.Deserialize<Dictionary<uint32, GameMap>> jsonString

let ws outputDirectory (webSocket : WebSocket) (context: HttpContext) =
  let mutable loop = true
  
  socket {
    
    let sendResponse (message:OutgoingMessage) =
        let byteResponse =
            serializeOutgoingMessage message
            |> System.Text.Encoding.UTF8.GetBytes
            |> ByteSegment
        webSocket.send Text byteResponse true
        
    let oracle =
        let feedback =
            fun (feedback: Feedback) ->
                let res =
                    socket {
                        let message =
                            match feedback with
                            | Feedback.ServerError s -> OutgoingMessage.ServerError s
                            | Feedback.MoveReward reward -> OutgoingMessage.MoveReward reward
                            | Feedback.IncorrectPredictedStateId i -> OutgoingMessage.IncorrectPredictedStateId i
                        do! sendResponse message
                    }
                match Async.RunSynchronously res with
                | Choice1Of2 () -> ()
                | Choice2Of2 error -> failwithf $"Error: %A{error}"
                
        let predict =
            let mutable cnt = 0u
            fun (gameState:GameState) ->
                let toDot drawHistory =
                    let file = Path.Join ("dot",$"{cnt}.dot")
                    let vertices = ResizeArray<_>()
                    let edges = ResizeArray<_>()
                    for v in gameState.GraphVertices do
                        let color = if v.CoveredByTest
                                    then "green"
                                    elif v.VisitedByState
                                    then "red"
                                    elif v.TouchedByState
                                    then "yellow"
                                    else "white"
                        vertices.Add($"{v.Id} [label={v.Id}, shape=box, style=filled, fillcolor={color}]")
                        for s in v.States do
                            edges.Add($"99{s}00 -> {v.Id} [label=L]")
                    for s in gameState.States do
                        vertices.Add($"99{s.Id}00 [label={s.Id}, shape=circle]")
                        for v in s.Children do
                            edges.Add($"99{s.Id}00 -> 99{v}00 [label=ch]")
                        if drawHistory
                        then 
                            for v in s.History do
                                edges.Add($"99{s.Id}00 -> {v.GraphVertexId} [label={v.NumOfVisits}]")
                    for e in gameState.Map do
                        edges.Add($"{e.VertexFrom}->{e.VertexTo}[label={e.Label.Token}]")
                    let dot =
                        seq
                            {
                                "digraph g{"
                                yield! vertices
                                yield! edges
                                "}"
                            }
                    File.WriteAllLines(file,dot)
                    cnt <- cnt + 1u
                //toDot false
                let res = 
                    socket {
                        do! sendResponse (ReadyForNextStep gameState)
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
                | Choice1Of2 (i, f) -> (i, f)
                | Choice2Of2 error -> failwithf $"Error: %A{error}"
        
        Oracle(predict,feedback)
    
    while loop do
        let! msg = webSocket.read()
        match msg with
        | (Text, data, true) ->
                let message = deserializeInputMessage data
                match message with
                | ServerStop -> loop <- false
                | GetTrainMaps ->
                    inTrainMode <- true
                    do! sendResponse (Maps trainMaps.Values)
                | GetValidationMaps ->
                    inTrainMode <- false
                    do! sendResponse (Maps validationMaps.Values)
                | Start gameStartParams ->
                    let settings =
                        if inTrainMode
                        then trainMaps.[gameStartParams.MapId]
                        else validationMaps.[gameStartParams.MapId]
                    let assembly = RunnerProgram.TryLoadAssembly <| FileInfo settings.AssemblyFullName
                    
                    let actualCoverage,testsCount,errorsCount =                         
                        let method = RunnerProgram.ResolveMethod(assembly, settings.NameOfObjectToCover)
                        let options = VSharpOptions(timeout = 15 * 60, outputDirectory = outputDirectory, oracle = oracle, searchStrategy = SearchStrategy.AI, coverageToSwitchToAI = uint settings.CoverageToStart, stepsToPlay = gameStartParams.StepsToPlay, solverTimeout=2)
                        let statistics = TestGenerator.Cover(method, options)
                        let actualCoverage = 
                            try 
                                let testsDir = statistics.OutputDir
                                let _expectedCoverage = 100
                                let exploredMethodInfo = AssemblyManager.NormalizeMethod method
                                let status,actualCoverage,message = VSharp.Test.TestResultChecker.Check(testsDir, exploredMethodInfo :?> MethodInfo, _expectedCoverage)
                                printfn $"Actual coverage for {settings.MapName}: {actualCoverage}"
                                System.Nullable (if actualCoverage < 0 then 0u else uint actualCoverage)
                            with
                            e ->
                                printfn $"Coverage checking problem:{e.Message} \n {e.StackTrace}"
                                System.Nullable(0u)

                        actualCoverage, statistics.TestsCount * 1u<test>, statistics.ErrorsCount *1u<error>
                    
                    Application.reset()
                    API.Reset()
                    HashMap.hashMap.Clear()
                    do! sendResponse (GameOver (actualCoverage, testsCount, errorsCount))
                | x -> failwithf $"Unexpected message: %A{x}"
                
        | (Close, _, _) ->
                let emptyResponse = [||] |> ByteSegment
                do! webSocket.send Close emptyResponse true
                loop <- false
        | _ -> ()
    }
  
let app port : WebPart =
    choose [
        path "/gameServer" >=> handShake (ws port)
    ]

let generateDataForPretraining outputDirectory datasetBasePath (maps:Dictionary<uint32,GameMap>) stepsToSerialize =
    for kvp in maps do
        if kvp.Value.CoverageToStart = 0u<percent>
        then
            printfn $"Generation for {kvp.Value.MapName} started."
            let assembly = RunnerProgram.TryLoadAssembly <| FileInfo(Path.Combine (datasetBasePath, kvp.Value.AssemblyFullName)) 
            let method = RunnerProgram.ResolveMethod(assembly, kvp.Value.NameOfObjectToCover)
            let options = VSharpOptions(timeout = 5 * 60, outputDirectory = outputDirectory, searchStrategy = SearchStrategy.ExecutionTreeContributedCoverage, stepsLimit = stepsToSerialize, solverTimeout=2, mapName = kvp.Value.MapName, serialize = true)
            let statistics = TestGenerator.Cover(method, options)
            printfn $"Generation for {kvp.Value.MapName} finished."
            Application.reset()
            API.Reset()
            HashMap.hashMap.Clear()

[<EntryPoint>]
let main args =
    let parser = ArgumentParser.Create<CliArguments>(programName = "VSharp.ML.GameServer.Runner.exe")
    let args = parser.Parse args
    
    let mode = args.GetResult <@Mode@>
        
    let port =
        match args.TryGetResult <@Port@> with
        | Some port -> port
        | None -> 8100
    
    let datasetBasePath = 
        match args.TryGetResult <@DatasetBasePath@> with
        | Some path -> path
        | None -> ""
        
    let datasetDescription = 
        match args.TryGetResult <@DatasetDescription@> with
        | Some path -> path
        | None -> ""

    let stepsToSerialize =
        match args.TryGetResult <@StepsToSerialize@> with
        | Some steps -> steps
        | None -> 500u
        
    let outputDirectory =
        Path.Combine(Directory.GetCurrentDirectory(), string port)
    
    if Directory.Exists outputDirectory
    then Directory.Delete(outputDirectory,true)
    let testsDirInfo = Directory.CreateDirectory outputDirectory
    printfn $"outputDir: {outputDirectory}"                
    
    //let s = System.Text.Json.JsonSerializer.Serialize(trainMaps)
    //printfn $"{s}"
    
    let maps = loadGameMaps datasetDescription 
    
    match mode with
    | Mode.Server -> 
        startWebServer {defaultConfig with
                            logger = Targets.create Verbose [||]
                            bindings = [HttpBinding.createSimple HTTP "127.0.0.1" port]} (app outputDirectory)
    | Mode.Generator ->
        generateDataForPretraining outputDirectory datasetBasePath maps stepsToSerialize
        
    0