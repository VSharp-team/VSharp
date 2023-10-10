namespace VSharp.Interpreter.IL

open System.Collections.Generic
open VSharp.IL.Serializer
open VSharp.ML.GameServer.Messages
open VSharp.Prelude

type internal AISearcher(coverageToSwitchToAI: uint, oracle:Oracle, serialize:bool) =
    let mutable lastCollectedStatistics = Statistics()
    let mutable gameState = None
    let mutable useDefaultSearcher = coverageToSwitchToAI > 0u
    let mutable afterFirstAIPeek = false
    let mutable incorrectPredictedStateId = false
    let defaultSearcher = BFSSearcher() :> IForwardSearcher
    let q = ResizeArray<_>()
    let availableStates = HashSet<_>()
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
                let _,statistics,_ = collectGameState (Seq.head availableStates).currentLoc serialize
                lastCollectedStatistics <- statistics
                useDefaultSearcher <- (statistics.CoveredVerticesInZone * 100u) / statistics.TotalVisibleVerticesInZone  < coverageToSwitchToAI
            defaultSearcher.Pick()
        elif Seq.length availableStates = 0
        then None
        else            
            let gameState,statistics,_ = collectGameState (Seq.head availableStates).currentLoc serialize
            lastCollectedStatistics <- statistics
            let stateId, predictedUsefulness =
                let x,y = oracle.Predict gameState
                x * 1u<stateId>, y
            afterFirstAIPeek <- true
            let state = availableStates |> Seq.tryFind (fun s -> s.id = stateId)
            match state with
            | Some state ->
                state.predictedUsefulness <- predictedUsefulness
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
