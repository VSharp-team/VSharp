namespace VSharp.Interpreter.IL

open System.Collections.Generic

open VSharp
open VSharp.Utils
open VSharp.Core
open CilStateOperations

type TargetedSearcher(maxBound, target) =
    inherit WeightedSearcher(maxBound, ShortestDistanceWeighter(target), BidictionaryPriorityQueue())

    let isStopped s = isStopped s || violatesLevel s maxBound

    override x.Insert states =
        base.Insert states
        for state in states do
            match x.TryGetWeight state with
            | None when not state.suspended ->
                removeTarget state target
            | _ -> ()

    override x.Update (parent, newStates) =
        let needsUpdating state =
            let onVertex state =
                let currLoc = tryCurrentLoc state
                match currLoc with
                | Some loc -> CFG.isVertex loc.method loc.offset
                | None -> false

            isStopped state  ||  onVertex state

        if needsUpdating parent then
            base.Update (parent, newStates)
        else
            base.Insert newStates

        for state in Seq.append [parent] newStates do
            match x.TryGetWeight state with
            | None when not state.suspended ->
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

type StatisticsTargetCalculator(statistics : SILIStatistics, coverageZone : coverageZone) =
    interface ITargetCalculator with
        override x.CalculateTarget state =
            let startingLoc = startingLoc state
            let locStack = state.ipStack |> Seq.choose (ipOperations.ip2codeLocation)
            let inCoverageZone loc = inCoverageZone coverageZone startingLoc loc
            Cps.Seq.foldlk (fun reachingLoc loc k ->
            match reachingLoc with
            | None when inCoverageZone loc ->
                    let localHistory = Seq.filter inCoverageZone (history state)
                    match statistics.PickUnvisitedWithHistoryInCFG(loc, localHistory) with
                    | None -> k None
                    | Some l -> Some l
            | _ -> k reachingLoc) None locStack id


type GuidedSearcher(maxBound, threshold : uint, baseSearcher : IForwardSearcher, targetCalculator : ITargetCalculator, coverageZone) =
    let targetedSearchers = Dictionary<codeLocation, TargetedSearcher>()
    let getTargets (state : cilState) = state.targets
    let reachedOrUnreachableTargets = HashSet<codeLocation> ()

    let calculateTarget (state : cilState): codeLocation option =
        targetCalculator.CalculateTarget state

    let suspend (state : cilState) : unit =
        state.suspended <- true
    let resume (state : cilState) : unit =
        state.suspended <- false
    let violatesRecursionLevel s =
        let startingLoc = startingLoc s
        let optCurrLoc = tryCurrentLoc s
        match optCurrLoc with
        | Some currLoc ->
            let onVertex = CFG.isVertex currLoc.method currLoc.offset
            let level = if PersistentDict.contains currLoc s.level then s.level.[currLoc] else 0u
            onVertex && level > threshold
        | _ -> false

    let mkTargetedSearcher target = TargetedSearcher(maxBound, target)
    let getTargetedSearcher target =
        Dict.getValueOrUpdate targetedSearchers target (fun () -> mkTargetedSearcher target)

    let mutable index = 1

    let insertInTargetedSearcher state target =
        let targetedSearcher = getTargetedSearcher target
        targetedSearcher.Insert [state] |> ignore

    let addReturnTarget state =
        let startingLoc = startingLoc state
        let startingMethod = startingLoc.method
        let cfg = CFG.findCfg startingMethod

        for retOffset in cfg.retOffsets do
            let target = {offset = retOffset; method = startingMethod}

            match state.targets with
            | Some targets ->
                state.targets <- Some <| Set.add target targets
                if not <| Set.contains target targets then
                    insertInTargetedSearcher state target
            | None ->
                state.targets <-Some (Set.add target Set.empty)
                insertInTargetedSearcher state target

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
                addReturnTarget state)

    let deleteTargetedSearcher target =
        let targetedSearcher = getTargetedSearcher target
        for state in targetedSearcher.ToSeq () do
            removeTarget state target
        targetedSearchers.Remove target |> ignore

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

    let setTargetOrSuspend state =
        match calculateTarget state with
        | Some target ->
            addTarget state target
            insertInTargetedSearcher state target
        | None ->
            state.targets <- None
            suspend state

    let rec pick' k =
        let pickFromBaseSearcher k =
            match baseSearcher.Pick() with
            | Some state ->
                match state.targets with
                | None when violatesRecursionLevel state ->
                    setTargetOrSuspend state
                    pick' k
                | _ -> k <| Some state
            | None -> k None
        let pickFromTargetedSearcher k =
            let targetSearcher = targetedSearchers |> Seq.item index
            let optState = targetSearcher.Value.Pick()

            if Option.isNone optState then
                reachedOrUnreachableTargets.Add targetSearcher.Key |> ignore
            if reachedOrUnreachableTargets.Contains targetSearcher.Key then
                deleteTargetedSearcher targetSearcher.Key
            if Option.isSome optState then k <| optState else pick' k
        let size = targetedSearchers.Count

        index <- (index + 1) % (size + 1)
        if index <> size
            then pickFromTargetedSearcher k
            else pickFromBaseSearcher k

    let pick () = pick' id

    interface IForwardSearcher with
        override x.Init states =
            baseSearcher.Init states
            insertInTargetedSearchers states
        override x.Pick() = pick ()
        override x.Update (parent, newStates) = update parent newStates
