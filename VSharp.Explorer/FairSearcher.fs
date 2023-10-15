namespace VSharp.Explorer

open System
open System.Collections.Generic
open System.Diagnostics

open VSharp
open VSharp.Interpreter.IL

/// <summary>
/// Enumerates initial values as follows: when pick() yielded a value for the first time, it will continue to yield it until timeout, then
/// the next value is yielded. When all values were yielded, a new round begins.
/// <param name="initialValues">Values to yield in the first round.</param>
/// <param name="getElementTimeoutMs">Checks for current element timeout.</param>
/// <param name="shouldStop">If true, pick won't yield a value.</param>
/// <param name="onRound">Called when all elements were yielded, returns the list of elements for the next round.</param>
/// <param name="onTimeout">Called on timeout, current element and elapsed time are passed.</param>
/// </summary>
type private FairEnumerator<'a>(initialValues : 'a list, getElementTimeoutMs : 'a -> uint, shouldStop : unit -> bool, onRound : 'a list -> 'a list, onTimeout : 'a -> uint -> unit) =

    let toExplore = Queue<'a>(initialValues)
    let remains = HashSet<'a>()
    let stopwatch = Stopwatch()

    let stopCurrent() =
        if toExplore.Count > 0 then
            let elapsed = stopwatch.ElapsedMilliseconds
            stopwatch.Reset()
            let dequeued = toExplore.Dequeue()
            onTimeout dequeued (uint elapsed)
            remains.Add dequeued |> ignore
            uint elapsed
        else 0u

    let rec pick() =
        let toExploreIsEmpty = toExplore.Count = 0
        if toExploreIsEmpty && remains.Count = 0 || shouldStop() then
            None
        elif toExploreIsEmpty then
            remains |> List.ofSeq |> onRound |> List.iter toExplore.Enqueue
            remains.Clear()
            pick()
        else
            let peeked = toExplore.Peek()
            if stopwatch.ElapsedMilliseconds <= int64 (getElementTimeoutMs peeked) then
                if not stopwatch.IsRunning then stopwatch.Start()
                Some peeked
            else
                stopCurrent() |> ignore
                pick()

    member x.Count = toExplore.Count + remains.Count

    member x.Pick() = pick()

    member x.DropCurrent() =
        if toExplore.Count > 0 then
            let elapsed = stopwatch.ElapsedMilliseconds
            stopwatch.Reset()
            toExplore.Dequeue() |> ignore
            uint elapsed
        else 0u

    member x.StopCurrent() = stopCurrent()

type internal FairSearcher(baseSearcherFactory : unit -> IForwardSearcher, timeoutMillis : uint, statistics : SVMStatistics) =

    let baseSearcher = baseSearcherFactory()

    let methodEnumerators = Dictionary<Type, FairEnumerator<Method>>()
    let mutable typeEnumerator = FairEnumerator([], (fun _ -> 0u), always true, id, fun _ _ -> ())

    let mutable totalTime = timeoutMillis
    let mutable elapsedTime = 0u

    let shouldStopType() = totalTime = 0u

    let getTypeTimeout _ = totalTime / uint typeEnumerator.Count

    // (method count * 10)ms is hardcoded time minimum to avoid too small timeouts
    // and too frequent switching between methods
    let shouldStopMethod typ = getTypeTimeout() <= uint methodEnumerators[typ].Count * 10u

    let getMethodTimeout typ = getTypeTimeout() / uint methodEnumerators[typ].Count

    let onTypeTimeout typ _ = elapsedTime <- elapsedTime + methodEnumerators[typ].StopCurrent()

    let onMethodTimeout _ elapsed = elapsedTime <- elapsedTime + elapsed

    let onTypeRound types =
        totalTime <- if elapsedTime > totalTime then 0u else totalTime - elapsedTime
        elapsedTime <- 0u
        types

    let onMethodRound methods =
        List.sortBy (fun (m : Method) -> statistics.GetCurrentCoverage m) methods

    let rec getCallsCount (m : Method) =
        try
            m.CFG.Calls.Count
        with :? InsufficientInformationException as e ->
            Logger.warning $"Fair searcher: IIE ({e.Message}) on getting calls count in {m.FullName}"
            Int32.MaxValue

    let init initialStates =
        baseSearcher.Init initialStates
        // Some states may be filtered out
        let initialStates = baseSearcher.States()
        let groupedByType = initialStates |> Seq.map CilStateOperations.entryMethodOf |> Seq.distinct |> Seq.groupBy (fun m -> m.DeclaringType)
        typeEnumerator <- FairEnumerator(groupedByType |> Seq.map fst |> Seq.toList, getTypeTimeout, shouldStopType, onTypeRound, onTypeTimeout)
        for typ, methods in groupedByType do
            methodEnumerators[typ] <- FairEnumerator(
                // Heuristics to explore the methods without calls first
                methods |> Seq.sortBy getCallsCount |> Seq.toList,
                (fun _ -> getMethodTimeout typ),
                (fun _ -> shouldStopMethod typ),
                onMethodRound,
                onMethodTimeout
            )

    let update (parent, newStates) = baseSearcher.Update(parent, newStates)

    let rec pick (selector : cilState -> bool) =
        match typeEnumerator.Pick() with
        | None -> None
        | Some typ ->
            match methodEnumerators[typ].Pick() with
            | None ->
                typeEnumerator.DropCurrent() |> ignore
                pick selector
            | Some method ->
                match baseSearcher.Pick (fun s -> CilStateOperations.entryMethodOf s = method && selector s) with
                | None ->
                    elapsedTime <- elapsedTime + methodEnumerators[typ].DropCurrent()
                    pick selector
                | Some _ as stateOpt -> stateOpt

    let states() = baseSearcher.States()

    let reset() =
        baseSearcher.Reset()
        methodEnumerators.Clear()
        typeEnumerator <- FairEnumerator([], (fun _ -> 0u), always true, id, fun _ _ -> ())

    let remove cilState = baseSearcher.Remove cilState

    member x.BaseSearcher with get() = baseSearcher

    interface IForwardSearcher with
        override x.Init states = init states
        override x.Pick() = pick (always true)
        override x.Pick selector = pick selector
        override x.Update (parent, newStates) = update (parent, newStates)
        override x.States() = states()
        override x.Reset() = reset()
        override x.Remove cilState = remove cilState
        override x.StatesCount with get() = baseSearcher.StatesCount
