namespace VSharp.Explorer

open System.Collections.Generic
open Microsoft.ML.OnnxRuntime
open System
open System.Text
open System.Text.Json
open VSharp
open VSharp.IL.Serializer
open VSharp.ML.GameServer.Messages

type AIMode =
    | Runner
    | TrainingSendModel
    | TrainingSendEachStep


type internal AISearcher(oracle: Oracle, aiAgentTrainingMode: Option<AIAgentTrainingMode>) =
    let stepsToSwitchToAI =
        match aiAgentTrainingMode with
        | None -> 0u<step>
        | Some(SendModel options) -> options.aiAgentTrainingOptions.stepsToSwitchToAI
        | Some(SendEachStep options) -> options.aiAgentTrainingOptions.stepsToSwitchToAI

    let stepsToPlay =
        match aiAgentTrainingMode with
        | None -> 0u<step>
        | Some(SendModel options) -> options.aiAgentTrainingOptions.stepsToPlay
        | Some(SendEachStep options) -> options.aiAgentTrainingOptions.stepsToPlay

    let mutable lastCollectedStatistics = Statistics()
    let mutable defaultSearcherSteps = 0u<step>
    let mutable (gameState: Option<GameState>) = None
    let mutable useDefaultSearcher = stepsToSwitchToAI > 0u<step>
    let mutable afterFirstAIPeek = false
    let mutable incorrectPredictedStateId = false

    let defaultSearcher =
        let pickSearcher =
            function
            | BFSMode -> BFSSearcher() :> IForwardSearcher
            | DFSMode -> DFSSearcher() :> IForwardSearcher
            | x -> failwithf $"Unexpected default searcher {x}. DFS and BFS supported for now."

        match aiAgentTrainingMode with
        | None -> BFSSearcher() :> IForwardSearcher
        | Some(SendModel options) -> pickSearcher options.aiAgentTrainingOptions.aiBaseOptions.defaultSearchStrategy
        | Some(SendEachStep options) -> pickSearcher options.aiAgentTrainingOptions.aiBaseOptions.defaultSearchStrategy

    let mutable stepsPlayed = 0u<step>

    let isInAIMode () =
        (not useDefaultSearcher) && afterFirstAIPeek

    let q = ResizeArray<_>()
    let availableStates = HashSet<_>()



    let init states =
        q.AddRange states
        defaultSearcher.Init q
        states |> Seq.iter (availableStates.Add >> ignore)

    let reset () =
        defaultSearcher.Reset()
        defaultSearcherSteps <- 0u<step>
        lastCollectedStatistics <- Statistics()
        gameState <- None
        afterFirstAIPeek <- false
        incorrectPredictedStateId <- false
        useDefaultSearcher <- stepsToSwitchToAI > 0u<step>
        q.Clear()
        availableStates.Clear()

    let update (parent, newStates) =
        if useDefaultSearcher then
            defaultSearcher.Update(parent, newStates)

        newStates |> Seq.iter (availableStates.Add >> ignore)

    let remove state =
        if useDefaultSearcher then
            defaultSearcher.Remove state

        let removed = availableStates.Remove state
        assert removed

        for bb in state._history do
            bb.Key.AssociatedStates.Remove state |> ignore

    let aiMode =
        match aiAgentTrainingMode with
        | Some(SendEachStep _) -> TrainingSendEachStep
        | Some(SendModel _) -> TrainingSendModel
        | None -> Runner

    let pick selector =
        if useDefaultSearcher then
            defaultSearcherSteps <- defaultSearcherSteps + 1u<step>

            if Seq.length availableStates > 0 then
                let gameStateDelta = collectGameStateDelta ()
                gameState <- AISearcher.updateGameState gameStateDelta gameState
                let statistics = computeStatistics gameState.Value
                Application.applicationGraphDelta.Clear()
                lastCollectedStatistics <- statistics
                useDefaultSearcher <- defaultSearcherSteps < stepsToSwitchToAI

            defaultSearcher.Pick()
        elif Seq.length availableStates = 0 then
            None
        elif Seq.length availableStates = 1 then
            Some(Seq.head availableStates)
        else
            let gameStateDelta = collectGameStateDelta ()
            gameState <- AISearcher.updateGameState gameStateDelta gameState
            let statistics = computeStatistics gameState.Value

            if isInAIMode () then
                let reward = computeReward lastCollectedStatistics statistics
                oracle.Feedback(Feedback.MoveReward reward)

            Application.applicationGraphDelta.Clear()

            let toPredict =
                match aiMode with
                | TrainingSendEachStep
                | TrainingSendModel ->
                    if stepsPlayed > 0u<step> then
                        gameStateDelta
                    else
                        gameState.Value
                | Runner -> gameState.Value

            let stateId = oracle.Predict toPredict
            afterFirstAIPeek <- true
            let state = availableStates |> Seq.tryFind (fun s -> s.internalId = stateId)
            lastCollectedStatistics <- statistics
            stepsPlayed <- stepsPlayed + 1u<step>

            match state with
            | Some state -> Some state
            | None ->
                incorrectPredictedStateId <- true
                oracle.Feedback(Feedback.IncorrectPredictedStateId stateId)
                None

    static member updateGameState (delta: GameState) (gameState: Option<GameState>) =
        match gameState with
        | None -> Some delta
        | Some s ->
            let updatedBasicBlocks = delta.GraphVertices |> Array.map (fun b -> b.Id) |> HashSet
            let updatedStates = delta.States |> Array.map (fun s -> s.Id) |> HashSet

            let vertices =
                s.GraphVertices
                |> Array.filter (fun v -> updatedBasicBlocks.Contains v.Id |> not)
                |> ResizeArray<_>

            vertices.AddRange delta.GraphVertices

            let edges =
                s.Map
                |> Array.filter (fun e -> updatedBasicBlocks.Contains e.VertexFrom |> not)
                |> ResizeArray<_>

            edges.AddRange delta.Map
            let activeStates = vertices |> Seq.collect (fun v -> v.States) |> HashSet

            let states =
                let part1 =
                    s.States
                    |> Array.filter (fun s -> activeStates.Contains s.Id && (not <| updatedStates.Contains s.Id))
                    |> ResizeArray<_>

                part1.AddRange delta.States

                part1.ToArray()
                |> Array.map (fun s ->
                    State(
                        s.Id,
                        s.Position,
                        s.PathCondition,
                        s.VisitedAgainVertices,
                        s.VisitedNotCoveredVerticesInZone,
                        s.VisitedNotCoveredVerticesOutOfZone,
                        s.StepWhenMovedLastTime,
                        s.InstructionsVisitedInCurrentBlock,
                        s.History,
                        s.Children |> Array.filter activeStates.Contains
                    ))

            let pathConditionVertices = ResizeArray<PathConditionVertex> s.PathConditionVertices

            pathConditionVertices.AddRange delta.PathConditionVertices

            Some
            <| GameState(vertices.ToArray(), states, pathConditionVertices.ToArray(), edges.ToArray())

    static member convertOutputToJson(output: IDisposableReadOnlyCollection<OrtValue>) =
        seq { 0 .. output.Count - 1 }
        |> Seq.map (fun i -> output[i].GetTensorDataAsSpan<float32>().ToArray())



    new
        (
            pathToONNX: string,
            useGPU: bool,
            optimize: bool,
            aiAgentTrainingModelOptions: Option<AIAgentTrainingModelOptions>
        ) =
        let numOfVertexAttributes = 7
        let numOfStateAttributes = 6
        let numOfPathConditionVertexAttributes = 48
        let numOfHistoryEdgeAttributes = 2


        let createOracleRunner (pathToONNX: string, aiAgentTrainingModelOptions: Option<AIAgentTrainingModelOptions>) =
            let sessionOptions =
                if useGPU then
                    SessionOptions.MakeSessionOptionWithCudaProvider(0)
                else
                    new SessionOptions()

            if optimize then
                sessionOptions.ExecutionMode <- ExecutionMode.ORT_PARALLEL
                sessionOptions.GraphOptimizationLevel <- GraphOptimizationLevel.ORT_ENABLE_ALL
            else
                sessionOptions.GraphOptimizationLevel <- GraphOptimizationLevel.ORT_ENABLE_BASIC

            let session = new InferenceSession(pathToONNX, sessionOptions)

            let runOptions = new RunOptions()
            let feedback (x: Feedback) = ()

            let mutable stepsPlayed = 0
            let mutable currentGameState = None

            let predict (gameStateOrDelta: GameState) =
                let _ =
                    match aiAgentTrainingModelOptions with
                    | Some _ when not (stepsPlayed = 0) ->
                        currentGameState <- AISearcher.updateGameState gameStateOrDelta currentGameState
                    | _ -> currentGameState <- Some gameStateOrDelta

                let gameState = currentGameState.Value
                let stateIds = Dictionary<uint<stateId>, int>()
                let verticesIds = Dictionary<uint<basicBlockGlobalId>, int>()
                let pathConditionVerticesIds = Dictionary<uint<pathConditionVertexId>, int>()

                let networkInput =
                    let res = Dictionary<_, _>()

                    let pathConditionVertices, numOfPcToPcEdges =
                        let mutable numOfPcToPcEdges = 0

                        let shape =
                            [| int64 gameState.PathConditionVertices.Length
                               numOfPathConditionVertexAttributes |]

                        let attributes =
                            Array.zeroCreate (
                                gameState.PathConditionVertices.Length * numOfPathConditionVertexAttributes
                            )

                        for i in 0 .. gameState.PathConditionVertices.Length - 1 do
                            let v = gameState.PathConditionVertices.[i]
                            numOfPcToPcEdges <- numOfPcToPcEdges + v.Children.Length * 2
                            pathConditionVerticesIds.Add(v.Id, i)
                            let j = i * numOfPathConditionVertexAttributes
                            attributes.[j + int v.Type] <- float32 1u

                        OrtValue.CreateTensorValueFromMemory(attributes, shape), numOfPcToPcEdges


                    let gameVertices =
                        let shape = [| int64 gameState.GraphVertices.Length; numOfVertexAttributes |]

                        let attributes =
                            Array.zeroCreate (gameState.GraphVertices.Length * numOfVertexAttributes)

                        for i in 0 .. gameState.GraphVertices.Length - 1 do
                            let v = gameState.GraphVertices.[i]
                            verticesIds.Add(v.Id, i)
                            let j = i * numOfVertexAttributes
                            attributes.[j] <- float32 <| if v.InCoverageZone then 1u else 0u
                            attributes.[j + 1] <- float32 <| v.BasicBlockSize
                            attributes.[j + 2] <- float32 <| if v.CoveredByTest then 1u else 0u
                            attributes.[j + 3] <- float32 <| if v.VisitedByState then 1u else 0u
                            attributes.[j + 4] <- float32 <| if v.TouchedByState then 1u else 0u
                            attributes.[j + 5] <- float32 <| if v.ContainsCall then 1u else 0u
                            attributes.[j + 6] <- float32 <| if v.ContainsThrow then 1u else 0u

                        OrtValue.CreateTensorValueFromMemory(attributes, shape)

                    let states, numOfParentOfEdges, numOfPathConditionEdges, numOfHistoryEdges =
                        let mutable numOfParentOfEdges = 0
                        let mutable numOfPathConditionEdges = 0
                        let mutable numOfHistoryEdges = 0
                        let shape = [| int64 gameState.States.Length; numOfStateAttributes |]
                        let attributes = Array.zeroCreate (gameState.States.Length * numOfStateAttributes)

                        for i in 0 .. gameState.States.Length - 1 do
                            let v = gameState.States.[i]
                            numOfParentOfEdges <- numOfParentOfEdges + v.Children.Length
                            numOfPathConditionEdges <- numOfPathConditionEdges + v.PathCondition.Length 
                            numOfHistoryEdges <- numOfHistoryEdges + v.History.Length
                            stateIds.Add(v.Id, i)
                            let j = i * numOfStateAttributes
                            attributes.[j] <- float32 v.Position
                            attributes.[j + 1] <- float32 v.VisitedAgainVertices
                            attributes.[j + 2] <- float32 v.VisitedNotCoveredVerticesInZone
                            attributes.[j + 3] <- float32 v.VisitedNotCoveredVerticesOutOfZone
                            attributes.[j + 4] <- float32 v.StepWhenMovedLastTime
                            attributes.[j + 5] <- float32 v.InstructionsVisitedInCurrentBlock

                        OrtValue.CreateTensorValueFromMemory(attributes, shape), numOfParentOfEdges, numOfPathConditionEdges, numOfHistoryEdges

                    let pcToPcEdgeIndex =
                        let shapeOfIndex = [| 2L; numOfPcToPcEdges |]
                        let index = Array.zeroCreate (2 * numOfPcToPcEdges)
                        let mutable firstFreePositionOfIndex = 0

                        for v in gameState.PathConditionVertices do
                            for child in v.Children do
                                // from vertex to child
                                index.[firstFreePositionOfIndex] <- int64 pathConditionVerticesIds.[v.Id]

                                index.[firstFreePositionOfIndex + numOfPcToPcEdges] <-
                                    int64 pathConditionVerticesIds.[child]

                                firstFreePositionOfIndex <- firstFreePositionOfIndex + 1
                                // from child to vertex
                                index.[firstFreePositionOfIndex] <- int64 pathConditionVerticesIds.[child]

                                index.[firstFreePositionOfIndex + numOfPcToPcEdges] <-
                                    int64 pathConditionVerticesIds.[v.Id]

                                firstFreePositionOfIndex <- firstFreePositionOfIndex + 1

                        OrtValue.CreateTensorValueFromMemory(index, shapeOfIndex)


                    let vertexToVertexEdgesIndex, vertexToVertexEdgesAttributes =
                        let shapeOfIndex = [| 2L; gameState.Map.Length |]
                        let shapeOfAttributes = [| int64 gameState.Map.Length |]
                        let index = Array.zeroCreate (2 * gameState.Map.Length)
                        let attributes = Array.zeroCreate gameState.Map.Length

                        gameState.Map
                        |> Array.iteri (fun i e ->
                            index[i] <- int64 verticesIds[e.VertexFrom]
                            index[gameState.Map.Length + i] <- int64 verticesIds[e.VertexTo]
                            attributes[i] <- int64 e.Label.Token)

                        OrtValue.CreateTensorValueFromMemory(index, shapeOfIndex),
                        OrtValue.CreateTensorValueFromMemory(attributes, shapeOfAttributes)

                    let historyEdgesIndex_vertexToState, historyEdgesAttributes, parentOfEdges, edgeIndex_pcToState =
                        let shapeOfParentOf = [| 2L; numOfParentOfEdges |]
                        let parentOf = Array.zeroCreate (2 * numOfParentOfEdges)
                        let shapeOfHistory = [| 2L; numOfHistoryEdges |]
                        let historyIndex_vertexToState = Array.zeroCreate (2 * numOfHistoryEdges)
                        let shapeOfPcToState = [| 2L; numOfPathConditionEdges |]
                        let index_pcToState = Array.zeroCreate (2 * numOfPathConditionEdges)

                        let shapeOfHistoryAttributes =
                            [| int64 numOfHistoryEdges; int64 numOfHistoryEdgeAttributes |]

                        let historyAttributes = Array.zeroCreate (2 * numOfHistoryEdges)
                        let mutable firstFreePositionInParentsOf = 0
                        let mutable firstFreePositionInHistoryIndex = 0
                        let mutable firstFreePositionInHistoryAttributes = 0
                        let mutable firstFreePositionInPcToState = 0

                        gameState.States
                        |> Array.iter (fun state ->
                            state.Children
                            |> Array.iteri (fun i children ->
                                let j = firstFreePositionInParentsOf + i
                                parentOf[j] <- int64 stateIds[state.Id]
                                parentOf[numOfParentOfEdges + j] <- int64 stateIds[children])

                            firstFreePositionInParentsOf <- firstFreePositionInParentsOf + state.Children.Length

                            state.PathCondition
                            |> Array.iteri (fun i pcId ->
                                let j = firstFreePositionInPcToState + i
                                index_pcToState[j] <- int64 pathConditionVerticesIds[pcId]
                                index_pcToState[numOfPathConditionEdges + j] <- int64 stateIds[state.Id])
                                
                            firstFreePositionInPcToState <- firstFreePositionInPcToState + state.PathCondition.Length

                            state.History
                            |> Array.iteri (fun i historyElem ->
                                let j = firstFreePositionInHistoryIndex + i
                                historyIndex_vertexToState[j] <- int64 verticesIds[historyElem.GraphVertexId]
                                historyIndex_vertexToState[numOfHistoryEdges + j] <- int64 stateIds[state.Id]

                                let j = firstFreePositionInHistoryAttributes + numOfHistoryEdgeAttributes * i
                                historyAttributes[j] <- int64 historyElem.NumOfVisits
                                historyAttributes[j + 1] <- int64 historyElem.StepWhenVisitedLastTime)

                            firstFreePositionInHistoryIndex <- firstFreePositionInHistoryIndex + state.History.Length

                            firstFreePositionInHistoryAttributes <-
                                firstFreePositionInHistoryAttributes
                                + numOfHistoryEdgeAttributes * state.History.Length)

                        OrtValue.CreateTensorValueFromMemory(historyIndex_vertexToState, shapeOfHistory),
                        OrtValue.CreateTensorValueFromMemory(historyAttributes, shapeOfHistoryAttributes),
                        OrtValue.CreateTensorValueFromMemory(parentOf, shapeOfParentOf),
                        OrtValue.CreateTensorValueFromMemory(index_pcToState, shapeOfPcToState)

                    let statePosition_stateToVertex, statePosition_vertexToState =
                        let data_stateToVertex = Array.zeroCreate (2 * gameState.States.Length)
                        let data_vertexToState = Array.zeroCreate (2 * gameState.States.Length)
                        let shape = [| 2L; gameState.States.Length |]
                        let mutable firstFreePosition = 0

                        gameState.GraphVertices
                        |> Array.iter (fun v ->
                            v.States
                            |> Array.iteri (fun i stateId ->
                                let j = firstFreePosition + i
                                let stateIndex = int64 stateIds[stateId]
                                let vertexIndex = int64 verticesIds[v.Id]
                                data_stateToVertex[j] <- stateIndex
                                data_stateToVertex[stateIds.Count + j] <- vertexIndex

                                data_vertexToState[j] <- vertexIndex
                                data_vertexToState[stateIds.Count + j] <- stateIndex)

                            firstFreePosition <- firstFreePosition + v.States.Length)

                        OrtValue.CreateTensorValueFromMemory(data_stateToVertex, shape),
                        OrtValue.CreateTensorValueFromMemory(data_vertexToState, shape)

                    res.Add("game_vertex", gameVertices)
                    res.Add("state_vertex", states)
                    res.Add("path_condition_vertex", pathConditionVertices)

                    res.Add("gamevertex_to_gamevertex_index", vertexToVertexEdgesIndex)
                    res.Add("gamevertex_to_gamevertex_type", vertexToVertexEdgesAttributes)

                    res.Add("gamevertex_history_statevertex_index", historyEdgesIndex_vertexToState)
                    res.Add("gamevertex_history_statevertex_attrs", historyEdgesAttributes)

                    res.Add("gamevertex_in_statevertex", statePosition_vertexToState)
                    res.Add("statevertex_parentof_statevertex", parentOfEdges)

                    res.Add("pathconditionvertex_to_pathconditionvertex", pcToPcEdgeIndex)
                    res.Add("pathconditionvertex_to_statevertex", edgeIndex_pcToState)

                    res

                let output = session.Run(runOptions, networkInput, session.OutputNames)

                let _ =
                    match aiAgentTrainingModelOptions with
                    | Some aiAgentOptions ->
                        aiAgentOptions.stepSaver (
                            AIGameStep(gameState = gameStateOrDelta, output = AISearcher.convertOutputToJson output)
                        )
                    | None -> ()

                stepsPlayed <- stepsPlayed + 1

                let weighedStates = output[0].GetTensorDataAsSpan<float32>().ToArray()

                let id = weighedStates |> Array.mapi (fun i v -> i, v) |> Array.maxBy snd |> fst
                stateIds |> Seq.find (fun kvp -> kvp.Value = id) |> (fun x -> x.Key)

            Oracle(predict, feedback)

        let aiAgentTrainingOptions =
            match aiAgentTrainingModelOptions with
            | Some aiAgentTrainingModelOptions -> Some(SendModel aiAgentTrainingModelOptions)
            | None -> None

        AISearcher(createOracleRunner (pathToONNX, aiAgentTrainingModelOptions), aiAgentTrainingOptions)

    interface IForwardSearcher with
        override x.Init states = init states
        override x.Pick() = pick (always true)
        override x.Pick selector = pick selector
        override x.Update(parent, newStates) = update (parent, newStates)
        override x.States() = availableStates
        override x.Reset() = reset ()
        override x.Remove cilState = remove cilState
        override x.StatesCount = availableStates.Count
