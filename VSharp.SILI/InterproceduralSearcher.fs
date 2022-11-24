namespace VSharp.Interpreter.IL

open System
open System.Collections.Generic
open Microsoft.FSharp.Core
open VSharp
open VSharp.GraphUtils
open VSharp.Interpreter.IL
open VSharp.Utils
open CilStateOperations

[<StructuralEquality; CustomComparison>]
type InterprocDistance =
    | Approximate of uint
    | Precise of uint
    | Infinity

    member x.IsPrecise_ =
        match x with
        | Precise _ -> true
        | _ -> false

    member x.IsApproximate_ =
        match x with
        | Approximate _ -> true
        | _ -> false

    member x.Unwrap() =
        match x with
        | Approximate d
        | Precise d -> d
        | Infinity -> UInt32.MaxValue

    static member (+) (one, another) =
        match one, another with
        | Infinity, _ | _, Infinity -> Infinity
        | Precise d1, Precise d2 -> Precise(d1 + d2)
        | Approximate d1, Precise d2
        | Precise d1, Approximate d2
        | Approximate d1, Approximate d2 -> Approximate(d1 + d2)

    interface IComparable with
        member this.CompareTo other =
            match other with
            | :? InterprocDistance as other -> (this :> IComparable<_>).CompareTo other
            | _ -> invalidArg (nameof(other)) "(InterprocDistance) Types of compared objects don't match"

    interface IComparable<InterprocDistance> with
        member this.CompareTo other =
            match this, other with
            | Infinity, Infinity -> 0
            | Infinity, _ -> 1
            | _, Infinity -> -1
            | Approximate d1, Precise d2 -> 1
            | Precise d1, Approximate d2 -> -1
            | Approximate d1, Approximate d2
            | Precise d1, Precise d2 -> d1.CompareTo d2

type private callTree = {
        parent : callTree option
        children : HashSet<callTree>
        callLocation : codeLocation option
        mutable minDistanceToUncoveredOnReturn : InterprocDistance
    }

type private distanceCache = Dictionary<offset, Dictionary<offset, InterprocDistance>>

type InterproceduralSearcher(maxBound, statistics : SILIStatistics) =

    let mutable priorityQueue : IPriorityCollection<cilState, InterprocDistance> = BidictionaryPriorityQueue()

    let callTreeRoot = {
        parent = None
        children = HashSet()
        callLocation = None
        minDistanceToUncoveredOnReturn = Precise 0u
    }

    let callTrees = Dictionary<cilState, callTree>()
    let callWeights = Dictionary<Method, InterprocDistance * HashSet<Method>>()
    let dijkstraWeightCaches = Dictionary<Method, distanceCache>()
    let returnsCache = Dictionary<Method, HashSet<offset>>()

    let ignoreState s = violatesLevel s maxBound

    let getReturns method =
        Dict.getValueOrUpdate returnsCache method (fun _ ->  HashSet method.CFG.Sinks)

    let getCallWeight method =
        if callWeights.ContainsKey method then
            let weight, _ = callWeights.[method]
            weight
        else
            Approximate 1u

    let getCallInfo codeLocation =
        if codeLocation.method.CFG.Calls.ContainsKey codeLocation.offset then
            Some codeLocation.method.CFG.Calls.[codeLocation.offset]
        else
            None

    let stepWeight toLocation =
        let callInfo = getCallInfo toLocation
        if callInfo.IsSome then (getCallWeight callInfo.Value.Callee) + Precise 1u else Precise 1u

    let dijkstra offsetFrom (edges : graph<offset>) (cache : distanceCache) method =
        let dist = Dictionary<offset, InterprocDistance>()
        dist.Add (offsetFrom, Precise 0u)
        let queue = Queue<_>()
        queue.Enqueue offsetFrom
        while not <| Seq.isEmpty queue do
            let parent = queue.Dequeue()
            if cache.ContainsKey parent then
                for parentWeight in cache.[parent] do
                    if not <| dist.ContainsKey parentWeight.Key then
                        dist.Add (parentWeight.Key, dist.[parent] + parentWeight.Value)
            else
                for child in edges.[parent] do
                    let codeLocation = { offset = child; method = method }
                    if dist.ContainsKey child && dist.[parent] + stepWeight codeLocation < dist.[child] then
                        dist.Remove child |> ignore
                    if not <| dist.ContainsKey child then
                        dist.Add (child, dist.[parent] + stepWeight codeLocation)
                        queue.Enqueue child
        dist

    let localDistancesFrom location =
        let cfg = location.method.CFG
        let basicBlock = cfg.ResolveBasicBlock location.offset
        let distanceCache = Dict.getValueOrUpdate dijkstraWeightCaches location.method distanceCache

        Dict.getValueOrUpdate distanceCache basicBlock (fun () ->
            dijkstra basicBlock cfg.Edges distanceCache location.method)

    let getMinDistanceToReturn codeLocation =
        let localDistances = localDistancesFrom codeLocation
        let returns = getReturns codeLocation.method
        let distancesToReturn = returns |> Seq.filter localDistances.ContainsKey |> Seq.map (fun o -> localDistances.[o])
        if Seq.isEmpty distancesToReturn then Precise 0u else Seq.min distancesToReturn

    let getMinDistanceToUncoveredAndToReturn codeLocation =
        let localDistances = localDistancesFrom codeLocation
        let returns = getReturns codeLocation.method
        let updateMinDistances (currentMinToUncovered, currentMinToReturn) (kvp : KeyValuePair<offset, InterprocDistance>) =
            // TODO: check all instructions of basic block for coverage
            let loc = { offset = kvp.Key; method = codeLocation.method }
            let distance = kvp.Value
            let newMinToUncovered =
                if distance < currentMinToUncovered && distance.Unwrap() <> 0u && not <| CodeLocation.isBasicBlockCoveredByTest loc then distance
                else currentMinToUncovered
            let newMinToReturn =
                if distance < currentMinToReturn && (returns.Contains kvp.Key) then distance
                else currentMinToReturn
            newMinToUncovered, newMinToReturn
        let minToUncovered, minToReturn = Seq.fold updateMinDistances (Infinity, Infinity) localDistances
        if minToReturn = Infinity then minToUncovered, Precise 0u else minToUncovered, minToReturn

    let weightLocation location minDistanceOnReturn =
        if not <| location.method.InCoverageZone then
            let minDistanceToReturn = getMinDistanceToReturn location
            minDistanceToReturn + minDistanceOnReturn
        else
            let minDistanceToLocalUncovered, minDistanceToReturn = getMinDistanceToUncoveredAndToReturn location
            min minDistanceToLocalUncovered (minDistanceToReturn + minDistanceOnReturn)

    let weight state =
        match ipOperations.ip2codeLocation state.ipStack.Head with
        | Some location ->
            let tree = callTrees.[state]
            //assert tree.callLocation.IsSome
            weightLocation location tree.minDistanceToUncoveredOnReturn
        | None -> Precise 0u

    // TODO: use CPS?
    let rec updateCallTree (queue : Queue<callTree>) =
        if queue.Count = 0 then
            ()
        else
            let dequeued = queue.Dequeue()
            match dequeued.callLocation with
            | Some location ->
                let newMinDistanceOnReturn = weightLocation location (if dequeued.parent.IsSome then dequeued.parent.Value.minDistanceToUncoveredOnReturn else Precise 0u)
                if newMinDistanceOnReturn <> dequeued.minDistanceToUncoveredOnReturn && newMinDistanceOnReturn.IsApproximate_ && dequeued.minDistanceToUncoveredOnReturn.IsPrecise_ then
                    Logger.info $"Distance is updated: {dequeued.minDistanceToUncoveredOnReturn} -> {newMinDistanceOnReturn}"
                dequeued.minDistanceToUncoveredOnReturn <- newMinDistanceOnReturn
            | None -> ()
            dequeued.children |> Seq.iter queue.Enqueue
            updateCallTree queue

    let calculateCallWeight (method : Method) =
        let startBlock = { method = method; offset = 0<offsets> }
        getMinDistanceToReturn startBlock + stepWeight startBlock

    let recalculateWeights() =
        let queue = Queue()
        queue.Enqueue callTreeRoot
        updateCallTree queue

        let states = priorityQueue.ToSeq
        priorityQueue <- BidictionaryPriorityQueue()
        for state in states do
            let weight = weight state
            priorityQueue.Insert state weight

    // TODO: use CPS?
    let rec updateCallWeights (preciseWeightMethods : Queue<Method>) =
        if preciseWeightMethods.Count = 0 then
            ()
        else
            let preciseWeightMethod = preciseWeightMethods.Dequeue()
            Logger.info $"Weight for {preciseWeightMethod.Name} is precise, recalculate"
            let weightsToRecalculate =
                callWeights
                |> Seq.map (|KeyValue|)
                |> Seq.filter (fun (_, v) -> (snd v).Contains preciseWeightMethod)

            for method, _ in weightsToRecalculate do
                if dijkstraWeightCaches.ContainsKey method then
                    dijkstraWeightCaches.[method].Clear()

                let newWeight = calculateCallWeight method
                callWeights.[method] <- (newWeight, snd callWeights.[method])

                if newWeight.IsPrecise_ then
                    preciseWeightMethods.Enqueue method

            updateCallWeights preciseWeightMethods

    let addCallWeight method =
        let callWeight = calculateCallWeight method
        let callees = method.CFG.Calls.Values |> Seq.map (fun ci -> ci.Callee) |> HashSet
        callWeights.[method] <- (callWeight, callees)

        if callWeight.IsPrecise_ then
            let queue = Queue()
            queue.Enqueue(method)
            updateCallWeights queue
            recalculateWeights()

    let pushToTree state fromLoc =
        let callTree = callTrees.[state]
        let newCallTreeNode = {
            parent = Some callTree
            children = HashSet()
            callLocation = fromLoc
            minDistanceToUncoveredOnReturn =
                if fromLoc.IsSome then weightLocation fromLoc.Value callTree.minDistanceToUncoveredOnReturn else Precise 0u
        }
        callTree.children.Add newCallTreeNode |> ignore
        callTrees.[state] <- newCallTreeNode

    let storageDump() =
        Logger.info "Storage:"
        priorityQueue.ToSeqWithPriority
        |> Seq.sortByDescending snd
        |> Seq.iter (fun (state, weight) ->
            Logger.info $"{weight} {state.currentLoc.method} {state.currentLoc.offset}")

    let init states =
        for state in states |> Seq.filter (not << ignoreState) do
            callTrees.[state] <- callTreeRoot
            pushToTree state None
            let weight = weight state
            priorityQueue.Insert state weight

    let pick() =
        //storageDump()
        priorityQueue.Choose()

    let pickWithSelector selector = priorityQueue.Choose selector

    let popFromTree state =
        let callTree = callTrees.[state]
        assert callTree.parent.IsSome
        callTrees.[state] <- callTree.parent.Value

    let update parent newStates =
        if priorityQueue.Contains parent then
            assert(callTrees.ContainsKey parent)
            if ignoreState parent && priorityQueue.Contains parent then
                priorityQueue.Remove parent
            else
                for event in parent.lastCallStackEvents do
                    match event with
                    | Push(fromLoc, _) -> pushToTree parent (Some fromLoc)
                    | Pop -> popFromTree parent

                let newParentWeight = weight parent
                priorityQueue.Update parent newParentWeight

                let tryAddCallWeight state =
                    match ipOperations.ip2codeLocation state.ipStack.Head with
                    | Some location ->
                        if not <| callWeights.ContainsKey location.method then
                            addCallWeight location.method
                    | None -> ()

                tryAddCallWeight parent

                let parentCallTree = callTrees.[parent]

                for state in newStates |> Seq.filter (not << ignoreState) do
                    assert(not <| priorityQueue.Contains state)
                    callTrees.[state] <- parentCallTree
                    let weight = weight state
                    priorityQueue.Insert state weight
                    tryAddCallWeight state

    let reset() =
        priorityQueue.Clear()
        callTrees.Clear()
        callWeights.Clear()
        dijkstraWeightCaches.Clear()
        returnsCache.Clear()

    let remove state =
        if priorityQueue.Contains state then
            priorityQueue.Remove state
            callTrees.Remove state |> ignore

    member x.RecalculateWeights() = recalculateWeights()

    interface IForwardSearcher with
        override x.Init states = init states
        override x.Pick() = pick()
        override x.Pick(selector) = pickWithSelector selector
        override x.Update (parent, newStates) = update parent newStates
        override x.States() = priorityQueue.ToSeq
        override x.Refresh() = x.RecalculateWeights()
        override x.Remove(state) = remove state
        override x.Reset() = reset()
        override x.StatesCount with get() = int priorityQueue.Count
