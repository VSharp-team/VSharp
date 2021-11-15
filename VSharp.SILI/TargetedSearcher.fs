namespace VSharp.Interpreter.IL

open System
open System.Collections.Generic

open VSharp
open VSharp.Core
open CilStateOperations

type TargetedSearcher(maxBound, target) =
    inherit WeightedSearcher(maxBound, ShortestDistancetWeighter(target))

    let isStopped s = isStopped s || violatesLevel s maxBound

    override x.Insert states =
        base.Insert states
        for state in states do
            match x.GetWeight state with
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
            match x.GetWeight state with
            | None when not state.suspended ->
                removeTarget state target
            | _ -> ()

    member x.TargetedInsert states : cilState list =
        x.Insert states
        states |> Seq.fold (fun reachedStates state ->
        match x.GetWeight state with
        | Some 0u -> state::reachedStates
        | _ -> reachedStates) []

    member x.TargetedUpdate (parent, newStates) =
        x.Update (parent, newStates)
        Seq.append [parent] newStates |> Seq.fold (fun reachedStates state ->
        match x.GetWeight state with
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


type GuidedSearcher(maxBound, recursionBound : uint, baseSearcher : IForwardSearcher, targetCalculator : ITargetCalculator, coverageZone) =
    let targetedSearchers = Dictionary<codeLocation, TargetedSearcher>()
    let getTargets (state : cilState) = state.targets

    let calculateTarget (state : cilState): codeLocation option =
        targetCalculator.CalculateTarget state

    let suspend (state : cilState) : unit =
        state.suspended <- true
    let resume (state : cilState) : unit =
        state.suspended <- false
    let violatesRecursionLevel s =
        let startingLoc = startingLoc s
        let inCoverageZone loc = inCoverageZone coverageZone startingLoc loc
        let optCurrLoc = tryCurrentLoc s
        match optCurrLoc with
        | Some currLoc ->
            let onVertex = CFG.isVertex currLoc.method currLoc.offset
            let level = if PersistentDict.contains currLoc s.level then s.level.[currLoc] else 0u
            onVertex && level > recursionBound
        | _ -> false

    let mkTargetedSearcher target = TargetedSearcher(maxBound, target)
    let getTargetedSearcher target =
        Dict.getValueOrUpdate targetedSearchers target (fun () -> mkTargetedSearcher target)

    let mutable index = 1

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
        let reachedStates  =
            match updateParentTargets with
            | Some targets when targets.Contains kvpair.Key ->
            targetedSearcher.TargetedUpdate (parent, kvpair.Value)
            | _ -> targetedSearcher.TargetedInsert addedCilStates.[kvpair.Key]
        if not <| List.isEmpty reachedStates then
            for state in targetedSearcher.ToSeq () do
                removeTarget state kvpair.Key 
            targetedSearchers.Remove kvpair.Key |> ignore)

    let insertInTargetedSearchers states =
        states
     |> Seq.iter (fun state ->
        option {
            let! sTargets = getTargets state
            sTargets
         |> Seq.iter (fun target ->
            let targetedSearcher = getTargetedSearcher target
            targetedSearcher.Insert [state] |> ignore)
        } |> ignore)

    let update parent newStates =
        baseSearcher.Update (parent, newStates)
        updateTargetedSearchers parent newStates

    let setTargetOrSuspend state =
        match calculateTarget state with
        | Some target ->
            addTarget state target
            insertInTargetedSearchers [state]
        | None ->
            state.targets <- None
            suspend state

    let rec pick () =
        let size = targetedSearchers.Count
        index <- (index + 1) % (size + 1)
        let pickFromBaseSearcher () =
            option {
                let! state = baseSearcher.Pick()
                match state.targets with
                | None when violatesRecursionLevel state ->
                    setTargetOrSuspend state
                    return! pick ()
                | _ -> return state
            }
        let pickFromTargetedSearcher () =
            let targetSearcher = targetedSearchers |> Seq.item index
            let state = targetSearcher.Value.Pick()
            if Option.isSome state then state
            else
                targetedSearchers.Remove targetSearcher.Key |> ignore
                pick ()

        if index <> size
            then pickFromTargetedSearcher ()
            else pickFromBaseSearcher ()

    interface IForwardSearcher with
        override x.Init states =
            baseSearcher.Init states
            insertInTargetedSearchers states
        override x.Pick() = pick ()
        override x.Update (parent, newStates) = update parent newStates
