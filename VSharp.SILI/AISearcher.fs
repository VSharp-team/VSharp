namespace VSharp.Interpreter.IL

open System.Collections.Generic
open VSharp.IL.Serializer
open VSharp.Interpreter.IL.TypeUtils
open VSharp.Prelude

type internal AISearcher(coverageToSwitchToAI: uint, oracle:Oracle) =
    let mutable lastCollectedStatistics = Statistics()
    let mutable gameState = None
    let mutable useDefaultSearcher = coverageToSwitchToAI > 0u
    let defaultSearcher = BFSSearcher(System.UInt32.MaxValue)
    let q = ResizeArray<_>()
    let availableStates = HashSet<_>()
    let init states =
        defaultSearcher.Init q states 
        states |> Seq.iter (availableStates.Add >> ignore)
    let reset () =
        (defaultSearcher :> SimpleForwardSearcher :> IForwardSearcher).Reset()
        availableStates.Clear()
    let update (parent, newSates) =
        if useDefaultSearcher
        then (defaultSearcher :> SimpleForwardSearcher :> IForwardSearcher).Update (parent,newSates)
        newSates |> Seq.iter (availableStates.Add >> ignore)
    let remove state =
        if useDefaultSearcher
        then (defaultSearcher :> SimpleForwardSearcher :> IForwardSearcher).Remove state
        let removed = availableStates.Remove state
        assert removed
    let pick selector =
        if useDefaultSearcher
        then
            let _,statistics = collectGameState (Seq.head availableStates).currentLoc
            lastCollectedStatistics <- statistics
            useDefaultSearcher <- (statistics.CoveredVerticesInZone / statistics.TotalVisibleVerticesInZone) * 100u < coverageToSwitchToAI
            defaultSearcher.Choose q selector
        else
            let gameState,statistics = collectGameState (Seq.head availableStates).currentLoc
            lastCollectedStatistics <- statistics
            let stateId, predictedUsefulness = oracle.Predict gameState
            let state = availableStates |> Seq.tryFind (fun s -> s.id = stateId)
            match state with
            | Some state ->
                state.predictedUsefulness <- predictedUsefulness
                Some state
            | None -> None //!!!! Fail!!! 
    member this.LastCollectedStatistics
        with get () = lastCollectedStatistics
        and set v = lastCollectedStatistics <- v
    member this.LastGameState with set v = gameState <- Some v    
    member this.Oracle = oracle    
    member this.InAIMode with get () = not useDefaultSearcher
    
    interface IForwardSearcher with
        override x.Init states = init states
        override x.Pick() = pick (always true)
        override x.Pick selector = pick selector
        override x.Update (parent, newStates) = update (parent, newStates)
        override x.States() = availableStates
        override x.Reset() = reset()
        override x.Remove cilState = remove cilState
        override x.StatesCount with get() = availableStates.Count
