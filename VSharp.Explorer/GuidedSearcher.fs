namespace VSharp.Explorer

open System.Collections.Generic

open VSharp
open VSharp.Interpreter.IL
open VSharp.Utils
open CilStateOperations

type TargetedSearcher(target) =
    inherit WeightedSearcher(ShortestDistanceWeighter(target), BidictionaryPriorityQueue())

    override x.Insert state =
        let wasInserted = base.Insert state
        if not wasInserted then
            removeTarget state target |> ignore
        wasInserted

    override x.Update (parent, newStates) =
        let needsUpdating state =
            let currLoc = tryCurrentLoc state
            match currLoc with
            | Some loc ->
                let cfg = loc.method.CFG
                cfg.IsBasicBlockStart loc.offset
            | None -> false

        let wasAnyUpdated =
            if needsUpdating parent then
                base.Update (parent, newStates)
            else
                // Distances from parent didn't change, it's in the same block as before,
                // so we don't need to update it's weight
                Seq.fold (fun r s -> x.Insert s || r) false newStates

        for state in Seq.cons parent newStates do
            match x.TryGetWeight state with
            | None ->
                removeTarget state target |> ignore
            | _ -> ()

        wasAnyUpdated

    member x.ReachedTarget (state : cilState) =
        match x.TryGetWeight state with
        | Some 0u -> true
        | _ -> false

type ITargetManager =
    abstract member CalculateTarget : cilState -> codeLocation option
    abstract member IsStuck : cilState -> bool

type RecursionBasedTargetManager(statistics : SVMStatistics, threshold : uint) =
    interface ITargetManager with
        override x.IsStuck state =
            match tryCurrentLoc state with
            | Some currLoc ->
                let cfg = currLoc.method.CFG
                let onVertex = cfg.IsBasicBlockStart currLoc.offset
                let level = if PersistentDict.contains currLoc state.level then state.level.[currLoc] else 0u
                onVertex && level > threshold
            | _ -> false

        override x.CalculateTarget state =
            let locStack = state.ipStack |> Seq.choose IpOperations.ip2codeLocation
            let inCoverageZone loc = loc.method.InCoverageZone
            Cps.Seq.foldlk (fun reachingLoc loc k ->
            match reachingLoc with
            | None when inCoverageZone loc ->
                let localHistory = Seq.filter inCoverageZone state.history
                match statistics.PickUnvisitedWithHistoryInCFG(loc, localHistory) with
                | None -> k None
                | Some l -> Some l
            | _ -> k reachingLoc) None locStack id

type GuidedSearcher(baseSearcher : IForwardSearcher, targetManager : ITargetManager) =
    let targetedSearchers = Dictionary<codeLocation, TargetedSearcher>()

    let mkTargetedSearcher target = TargetedSearcher(target)
    let getTargetedSearcher target =
        Dict.getValueOrUpdate targetedSearchers target (fun () -> mkTargetedSearcher target)

    let mutable index = 1

    let insertInTargetedSearcher state target =
        let targetedSearcher = getTargetedSearcher target
        targetedSearcher.Insert state

    let addReturnTarget state =
        let startingLoc = startingLoc state
        let startingMethod = startingLoc.method
        let cfg = startingMethod.CFG

        for retOffset in cfg.Sinks do
            let target = {offset = retOffset.StartOffset; method = startingMethod}
            if addTarget state target then
                insertInTargetedSearcher state target |> ignore

    let deleteTargetedSearcher target =
        let targetedSearcher = getTargetedSearcher target
        for state in targetedSearcher.ToSeq() do
            removeTarget state target |> ignore
        targetedSearchers.Remove target |> ignore

    let updateTargetedSearchers parent (newStates : cilState seq) =
        let cilStatesByTarget = Dictionary<codeLocation, List<cilState>>()

        for target in parent.targets do
            cilStatesByTarget[target] <- List()

        for state in newStates do
            for target in state.targets do
                let targetStates = Dict.getValueOrUpdate cilStatesByTarget target (fun () -> List())
                targetStates.Add state

        let statesReachedTarget = HashSet<cilState>()

        for KeyValue(target, states) in cilStatesByTarget do
            let targetedSearcher = getTargetedSearcher target

            if parent.targets.Contains target then
                targetedSearcher.Update(parent, newStates) |> ignore
            else
                Seq.iter (targetedSearcher.Insert >> ignore) newStates

            let statesReachedCurrentTarget = Seq.cons parent states |> Seq.filter targetedSearcher.ReachedTarget
            if not <| Seq.isEmpty statesReachedTarget then
                deleteTargetedSearcher target
                Seq.iter (statesReachedTarget.Add >> ignore) statesReachedCurrentTarget

        Seq.iter addReturnTarget statesReachedTarget

    let init states =
        baseSearcher.Init states
        for state in states do
            for target in state.targets do
                insertInTargetedSearcher state target |> ignore

    let pick (selector : (cilState -> bool) option) =
        let pickInternal (searcher : IForwardSearcher) =
            match selector with
            | Some selector -> searcher.Pick selector
            | None -> searcher.Pick()

        let pickFromTargetedSearcher() =
            let currentTargetedSearcher = (Seq.item index targetedSearchers).Value
            match pickInternal currentTargetedSearcher with
            | Some _ as pickedFromCurrent -> pickedFromCurrent
            | None ->
                seq { yield! Seq.cast<IForwardSearcher> targetedSearchers.Values; yield baseSearcher }
                |> Seq.filter (fun s -> s <> currentTargetedSearcher)
                |> Seq.tryPick pickInternal

        let size = targetedSearchers.Count
        index <- (index + 1) % (size + 1)
        if index <> size then
            pickFromTargetedSearcher()
        else
            pickInternal baseSearcher

    let remove cilState =
        baseSearcher.Remove cilState
        for searcher in targetedSearchers.Values do (searcher :> IForwardSearcher).Remove cilState

    let update parent newStates =
        baseSearcher.Update (parent, newStates)
        updateTargetedSearchers parent newStates

        for state in Seq.cons parent newStates do
            if Set.isEmpty state.targets && targetManager.IsStuck state then
                match targetManager.CalculateTarget state with
                | Some target ->
                    addTarget state target |> ignore
                    if not <| insertInTargetedSearcher state target then
                        state.targets <- Set.empty
                        remove state
                | None ->
                    state.targets <- Set.empty
                    remove state

        let targetsWithEmptySearchers = targetedSearchers |> Seq.filter (fun (KeyValue(_, s)) -> s.Count = 0u) |> Seq.toList
        for KeyValue(t, _) in targetsWithEmptySearchers do
            deleteTargetedSearcher t

    let reset() =
        baseSearcher.Reset()
        for searcher in targetedSearchers.Values do (searcher :> IForwardSearcher).Reset()

    interface IForwardSearcher with
        override x.Init states = init states
        override x.Pick() = pick None
        override x.Pick selector = pick (Some selector)
        override x.Update (parent, newStates) = update parent newStates
        override x.States() = baseSearcher.States()
        override x.Reset() = reset()
        override x.Remove cilState = remove cilState
        override x.StatesCount with get() =
            baseSearcher.StatesCount + (targetedSearchers.Values |> Seq.sumBy (fun s -> int s.Count))

type ShortestDistanceBasedSearcher(statistics : SVMStatistics) =
    inherit WeightedSearcher(IntraproceduralShortestDistanceToUncoveredWeighter(statistics), BidictionaryPriorityQueue())

type RandomShortestDistanceBasedSearcher(statistics : SVMStatistics, randomSeed : int option) =
    inherit WeightedSearcher(AugmentedWeighter(IntraproceduralShortestDistanceToUncoveredWeighter(statistics), (WeightOperations.inverseLogarithmic 7u)), DiscretePDF(mkCilStateHashComparer, randomSeed))
