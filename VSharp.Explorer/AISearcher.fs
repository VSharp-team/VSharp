namespace VSharp.Explorer

open System.Collections.Generic
open VSharp
open VSharp.IL.Serializer
open VSharp.ML.GameServer.Messages

type internal AISearcher(coverageToSwitchToAI: uint, oracle:Oracle, serialize:bool) =
    let mutable lastCollectedStatistics = Statistics()
    let mutable (gameState:Option<GameState>) = None
    let mutable useDefaultSearcher = coverageToSwitchToAI > 0u
    let mutable afterFirstAIPeek = false
    let mutable incorrectPredictedStateId = false
    let defaultSearcher = BFSSearcher() :> IForwardSearcher
    let q = ResizeArray<_>()
    let availableStates = HashSet<_>()
    let updateGameState (delta:GameState) =
            match gameState with
            | None ->
                gameState <- Some delta
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
                    s.States |> Array.filter (fun s -> activeStates.Contains s.Id && (not <| updatedStates.Contains s.Id))
                    |> Array.map (fun s -> State(s.Id
                                                 , s.Position
                                                 , s.PredictedUsefulness
                                                 , s.PathConditionSize
                                                 , s.VisitedAgainVertices
                                                 , s.VisitedNotCoveredVerticesInZone
                                                 , s.VisitedNotCoveredVerticesOutOfZone
                                                 , s.History
                                                 , s.Children |> Array.filter activeStates.Contains )
                    )
                    |> ResizeArray<_>
                states.AddRange delta.States
                
                gameState <- Some <| GameState (vertices.ToArray(), states.ToArray(), edges.ToArray())
                
                        
    let init states =
        q.AddRange states
        defaultSearcher.Init q  
        states |> Seq.iter (availableStates.Add >> ignore)
    let reset () =
        defaultSearcher.Reset()
        lastCollectedStatistics <- Statistics()
        gameState <- None
        afterFirstAIPeek <- false
        incorrectPredictedStateId <- false
        useDefaultSearcher <- coverageToSwitchToAI > 0u
        q.Clear()
        availableStates.Clear()
    let update (parent, newSates) =
        if useDefaultSearcher
        then defaultSearcher.Update (parent,newSates)
        newSates |> Seq.iter (availableStates.Add >> ignore)
    let remove state =
        if useDefaultSearcher
        then defaultSearcher.Remove state
        let removed = availableStates.Remove state
        assert removed       
        for bb in state._history do bb.Key.AssociatedStates.Remove state |> ignore
        
    let pick selector =
        if useDefaultSearcher
        then
            if Seq.length availableStates > 0
            then
                let gameStateDelta,_ = collectGameStateDelta serialize                
                updateGameState gameStateDelta
                let statistics = computeStatistics gameState.Value
                Application.applicationGraphDelta.Clear()
                lastCollectedStatistics <- statistics
                useDefaultSearcher <- (statistics.CoveredVerticesInZone * 100u) / statistics.TotalVisibleVerticesInZone  < coverageToSwitchToAI
            defaultSearcher.Pick()
        elif Seq.length availableStates = 0
        then None
        else            
            let gameStateDelta,_ = collectGameStateDelta serialize
            updateGameState gameStateDelta
            let statistics = computeStatistics gameState.Value
            Application.applicationGraphDelta.Clear()
            lastCollectedStatistics <- statistics
            let stateId, _ =
                let x,y = oracle.Predict gameState.Value
                x * 1u<stateId>, y
            afterFirstAIPeek <- true
            let state = availableStates |> Seq.tryFind (fun s -> s.internalId = stateId)
            match state with
            | Some state ->                
                Some state
            | None ->
                incorrectPredictedStateId <- true
                oracle.Feedback (Feedback.IncorrectPredictedStateId stateId)
                None
    member this.LastCollectedStatistics
        with get () = lastCollectedStatistics
        and set v = lastCollectedStatistics <- v
    member this.LastGameState with set v = gameState <- Some v    
    member this.ProvideOracleFeedback feedback =
        if not incorrectPredictedStateId
        then
            oracle.Feedback feedback
            incorrectPredictedStateId <- false
    member this.InAIMode with get () = (not useDefaultSearcher) && afterFirstAIPeek 
    
    interface IForwardSearcher with
        override x.Init states = init states
        override x.Pick() = pick (always true)
        override x.Pick selector = pick selector
        override x.Update (parent, newStates) = update (parent, newStates)
        override x.States() = availableStates
        override x.Reset() = reset()
        override x.Remove cilState = remove cilState
        override x.StatesCount with get() = availableStates.Count
