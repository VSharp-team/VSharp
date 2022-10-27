namespace VSharp.Interpreter.IL

open System
open System.Collections.Generic
open System.Diagnostics
open VSharp

type private FairEnumerator<'a>(initialValues : 'a list, getElementTimeout : 'a -> uint, shouldStop : unit -> bool, onRound : 'a list -> 'a list, onTimeout : 'a -> uint -> unit) =

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
        if toExplore.Count = 0 && remains.Count = 0 || shouldStop() then
            None
        elif toExplore.Count = 0 then
            remains |> List.ofSeq |> onRound |> List.iter toExplore.Enqueue
            remains.Clear()
            pick()
        else
            let peeked = toExplore.Peek()
            if stopwatch.ElapsedMilliseconds <= int64 (getElementTimeout peeked) then
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

type internal FairSearcher(baseSearcherFactory : unit -> IForwardSearcher, timeoutMillis : uint, statistics : SILIStatistics) =

    let baseSearcher = baseSearcherFactory()

    let methodEnumerators = Dictionary<Type, FairEnumerator<Method>>()
    let mutable typeEnumerator = FairEnumerator([], (fun _ -> 0u), always true, id, fun _ _ -> ())

    let mutable totalTime = timeoutMillis
    let mutable elapsedTime = 0u

    let shouldStopType() = totalTime = 0u

    let getTypeTimeout _ = totalTime / uint typeEnumerator.Count

    let shouldStopMethod typ = getTypeTimeout() <= uint methodEnumerators.[typ].Count * 10u

    let getMethodTimeout typ = getTypeTimeout() / uint methodEnumerators.[typ].Count

    let onTypeTimeout typ _ = elapsedTime <- elapsedTime + methodEnumerators.[typ].StopCurrent()

    let onMethodTimeout _ elapsed = elapsedTime <- elapsedTime + elapsed

    let onTypeRound types =
        totalTime <- if elapsedTime > totalTime then 0u else totalTime - elapsedTime
        elapsedTime <- 0u
        types

    let onMethodRound methods = List.sortBy (fun (m : Method) -> statistics.GetApproximateCoverage m) methods

    let init initialStates =
        baseSearcher.Init initialStates
        let groupedByType = initialStates |> Seq.map CilStateOperations.entryMethodOf |> Seq.distinct |> Seq.groupBy (fun m -> m.DeclaringType)
        typeEnumerator <- FairEnumerator(groupedByType |> Seq.map fst |> Seq.toList, getTypeTimeout, shouldStopType, onTypeRound, onTypeTimeout)
        for typ, methods in groupedByType do
            methodEnumerators.[typ] <- FairEnumerator(
                methods |> Seq.sortBy (fun m -> m.CFG.Calls.Count) |> Seq.toList,
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
            match methodEnumerators.[typ].Pick() with
            | None ->
                typeEnumerator.DropCurrent() |> ignore
                pick selector
            | Some method ->
                match baseSearcher.Pick (fun s -> CilStateOperations.entryMethodOf s = method && selector s) with
                | None ->
                    elapsedTime <- elapsedTime + methodEnumerators.[typ].DropCurrent()
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
