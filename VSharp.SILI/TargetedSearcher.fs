namespace VSharp.Interpreter.IL

open System.Collections.Generic

open VSharp
open VSharp.Interpreter.IL
open VSharp.Utils
open CilStateOperations

type TargetedSearcher(maxBound, target, isPaused) =
    inherit WeightedSearcher(maxBound, ShortestDistanceWeighter(target), BidictionaryPriorityQueue())

    override x.Insert states =
        base.Insert states
        for state in states do
            match x.TryGetWeight state with
            | None when not <| isPaused state ->
                removeTarget state target
            | _ -> ()

    override x.Update (parent, newStates) =
        let needsUpdating state =
            let onVertex state =
                let currLoc = tryCurrentLoc state
                match currLoc with
                | Some loc ->
                    let cfg = loc.method.ForceCFG
                    cfg.IsBasicBlockStart loc.offset
                | None -> false

            violatesLevel state maxBound || onVertex state

        if needsUpdating parent then
            base.Update (parent, newStates)
        else
            base.Insert newStates

        for state in Seq.append [parent] newStates do
            match x.TryGetWeight state with
            | None when not <| isPaused state ->
                removeTarget state target

            | _ -> ()

    member x.TargetedInsert states : cilState list =
        x.Insert states
        states |> Seq.fold (fun reachedStates state ->
        match x.TryGetWeight state with
        | Some 0u -> state::reachedStates
        | _ -> reachedStates) []

    member x.TargetedUpdate (parent, newStates) =
        x.Update (parent, newStates)
        Seq.append [parent] newStates |> Seq.fold (fun reachedStates state ->
        match x.TryGetWeight state with
        | Some 0u -> state::reachedStates
        | _ -> reachedStates) []

type ITargetCalculator =
    abstract member CalculateTarget : cilState -> codeLocation option

type StatisticsTargetCalculator(statistics : SILIStatistics) =
    interface ITargetCalculator with
        override x.CalculateTarget state =
            let locStack = state.ipStack |> Seq.choose ipOperations.ip2codeLocation
            let inCoverageZone loc = loc.method.InCoverageZone
            Cps.Seq.foldlk (fun reachingLoc loc k ->
            match reachingLoc with
            | None when inCoverageZone loc ->
                let localHistory = Seq.filter inCoverageZone state.history
                match statistics.PickUnvisitedWithHistoryInCFG(loc, localHistory) with
                | None -> k None
                | Some l -> Some l
            | _ -> k reachingLoc) None locStack id


type GuidedSearcher(maxBound, threshold : uint, baseSearcher : IForwardSearcher, targetCalculator : ITargetCalculator) =
    let targetedSearchers = Dictionary<codeLocation, TargetedSearcher>()
    let getTargets (state : cilState) = state.targets
    let reachedOrUnreachableTargets = HashSet<codeLocation> ()
    let pausedStates = HashSet<cilState>()

    let isPaused cilState = pausedStates.Contains cilState

    let calculateTarget (state : cilState): codeLocation option =
        targetCalculator.CalculateTarget state

    let violatesRecursionLevel s =
        let optCurrLoc = tryCurrentLoc s
        match optCurrLoc with
        | Some currLoc when currLoc.method.CFG.IsSome ->
            let cfg = currLoc.method.ForceCFG
            let onVertex = cfg.IsBasicBlockStart currLoc.offset
            let level = if PersistentDict.contains currLoc s.level then s.level.[currLoc] else 0u
            onVertex && level > threshold
        | _ -> false

    let mkTargetedSearcher target = TargetedSearcher(maxBound, target, isPaused)
    let getTargetedSearcher target =
        Dict.getValueOrUpdate targetedSearchers target (fun () -> mkTargetedSearcher target)

    let mutable index = 1

    let insertInTargetedSearcher state target =
        let targetedSearcher = getTargetedSearcher target
        targetedSearcher.Insert [state]

    let addReturnTarget state =
        let startingLoc = startingLoc state
        let startingMethod = startingLoc.method
        let cfg = startingMethod.ForceCFG

        for retOffset in cfg.Sinks do
            let target = {offset = retOffset.StartOffset; method = startingMethod}

            match state.targets with
            | Some targets ->
                state.targets <- Some <| Set.add target targets
                if not <| Set.contains target targets then
                    insertInTargetedSearcher state target
            | None ->
                state.targets <-Some (Set.add target Set.empty)
                insertInTargetedSearcher state target

    let deleteTargetedSearcher target =
        let targetedSearcher = getTargetedSearcher target
        for state in targetedSearcher.ToSeq () do
            removeTarget state target
        targetedSearchers.Remove target |> ignore

    let updateTargetedSearchers parent (newStates : cilState seq) =
        let addedCilStates = Dictionary<codeLocation, cilState list>()
        let updateParentTargets = getTargets parent

        match updateParentTargets with
        | Some targets ->
            for target in targets do
                addedCilStates.Add(target, [])
        | None -> ()

        for state in newStates do
            option {
                let! sTargets = getTargets state
                sTargets |> Seq.iter (fun target ->
                let targets = Dict.getValueOrUpdate addedCilStates target (fun () -> [])
                addedCilStates.[target] <- state :: targets)
            } |> ignore

        addedCilStates |> Seq.iter (fun kvpair ->
        let targetedSearcher = getTargetedSearcher kvpair.Key
        let reachedStates =
            match updateParentTargets with
            | Some targets when targets.Contains kvpair.Key ->
                targetedSearcher.TargetedUpdate (parent, kvpair.Value)
            | _ -> targetedSearcher.TargetedInsert addedCilStates.[kvpair.Key]

        if not <| List.isEmpty reachedStates then
            reachedOrUnreachableTargets.Add kvpair.Key |> ignore
            for state in reachedStates do
                deleteTargetedSearcher kvpair.Key
                addReturnTarget state)

    let insertInTargetedSearchers states =
        states
     |> Seq.iter (fun state ->
        option {
            let! sTargets = getTargets state
            sTargets
         |> Seq.iter (fun target ->
            insertInTargetedSearcher state target)
        } |> ignore)

    let update parent newStates =
        baseSearcher.Update (parent, newStates)
        updateTargetedSearchers parent newStates

    let pause (state : cilState) : unit =
        pausedStates.Add state |> ignore

    let setTargetOrPause state =
        match calculateTarget state with
        | Some target ->
            addTarget state target
            insertInTargetedSearcher state target
        | None ->
            state.targets <- None
            pause state

    let rec pick' (selector : (cilState -> bool) option) =
        let pick (searcher : IForwardSearcher) =
            if selector.IsSome then
                searcher.Pick(fun s -> not <| isPaused s && selector.Value s)
            else
                searcher.Pick(not << isPaused)
        let pickFromBaseSearcher () =
            match pick baseSearcher with
            | Some state ->
                match state.targets with
                | None when violatesRecursionLevel state ->
                    setTargetOrPause state
                    pick' selector
                | _ -> Some state
            | None -> None
        let pickFromTargetedSearcher () =
            let targetSearcher = targetedSearchers |> Seq.item index

            if targetSearcher.Value.Count = 0u then
                deleteTargetedSearcher targetSearcher.Key
                pick' selector
            else
                let optState = pick targetSearcher.Value
                if Option.isSome optState then optState else pick' selector
        let size = targetedSearchers.Count

        index <- (index + 1) % (size + 1)
        if index <> size
            then pickFromTargetedSearcher ()
            else pickFromBaseSearcher ()

    let pick selector = pick' selector

    let reset () =
        baseSearcher.Reset()
        for searcher in targetedSearchers.Values do (searcher :> IForwardSearcher).Reset()

    let remove cilState =
        baseSearcher.Remove cilState
        for searcher in targetedSearchers.Values do (searcher :> IForwardSearcher).Remove cilState

    interface IForwardSearcher with
        override x.Init states =
            baseSearcher.Init states
            insertInTargetedSearchers states
        override x.Pick() = pick None
        override x.Pick selector = pick (Some selector)
        override x.Update (parent, newStates) = update parent newStates
        override x.States() = baseSearcher.States()
        override x.Reset() = reset ()
        override x.Remove cilState = remove cilState
        override x.StatesCount with get() = baseSearcher.StatesCount + (targetedSearchers.Values |> Seq.sumBy (fun s -> int s.Count))


type ShortestDistanceBasedSearcher(maxBound, statistics : SILIStatistics) =
    inherit WeightedSearcher(maxBound, IntraproceduralShortestDistanceToUncoveredWeighter(statistics), BidictionaryPriorityQueue())

type RandomShortestDistanceBasedSearcher(maxBound, statistics : SILIStatistics) =
    inherit WeightedSearcher(maxBound, AugmentedWeighter(IntraproceduralShortestDistanceToUncoveredWeighter(statistics), (WeightOperations.inverseLogarithmic 7u)), DiscretePDF(mkCilStateHashComparer))
