namespace VSharp.Interpreter.IL

open System
open System.Collections.Generic
open System.Reflection
open System.Reflection.Emit
open System.Text
open FSharpx.Collections
open FSharpx.Collections
open FSharpx.Collections
open InstructionsSet
open CilStateOperations
open VSharp
open VSharp.Core
open ipOperations
open Instruction

 [<CustomComparison; CustomEquality>]
 type reachabilityEvaluation =
    | Unknown
    | Reachable of int * int  // cost to go to target and cost to go to exit
    with
    override x.Equals y =
        match y with
        | :? reachabilityEvaluation as y ->
            match x, y with
            | Unknown, Unknown -> true
            | Reachable(p1, p2), Reachable(p3, p4) -> p1 = p2 && p3 = p4
            | _ -> false
        | _ -> false
    override x.GetHashCode() = x.ToString().GetHashCode()
    interface IComparable with
        override x.CompareTo y =
            match y with
            | :? reachabilityEvaluation as r ->
                match x, r with
                | Unknown, Unknown -> 0
                | Unknown, Reachable _ -> 1
                | Reachable _, Unknown _ -> -1
                | Reachable(p1, p2), Reachable(p3, p4) when p1 < p3 || p1 = p3 && p2 < p4 -> -1
                | Reachable(p1, p2), Reachable(p3, p4) when p1 = p3 && p2 = p4 -> 0
                | _ -> 1
            | _ -> -1

type cilstatesComparer(target : codeLocation, reachableLocations : Dictionary<codeLocation, codeLocation list>, reachableMethods : Dictionary<codeLocation, MethodBase list>,
                       methodsReachabilityTransitiveClosure : Dictionary<MethodBase, MethodBase list>) =
    let COST_OF_MANY_CALLS = 3000
    let COST_OF_CALL = 500
    let COST_OF_EXIT = 100
    let REGULAR_COST = 1
    let COST_TO_GO_TO_ENDFINALLY = 50
    let UNKNOWN_CONSTANT = Int32.MaxValue
//    let

    let ip2codeLocation (ip : ip) =
        match offsetOf ip, methodOf ip with
        | None, _ -> None
        | Some offset, m ->
            let loc = {offset = offset; method = m}
            Some loc
    let min(x, y) = if x < y then x else y

    let loc2Locs (loc : codeLocation) =
        match reachableLocations.ContainsKey loc with
        | true -> reachableLocations.[loc]
        | _ -> []

    let loc2Methods (loc : codeLocation) =
        match reachableMethods.ContainsKey loc with
        | true -> reachableMethods.[loc]
        | _ -> []

    let method2Methods (m : MethodBase) =
        match methodsReachabilityTransitiveClosure.ContainsKey m with
        | true -> methodsReachabilityTransitiveClosure.[m]
        | _ -> []

    let canReachMetrics(ipStack : ip list, _ : ip list) : reachabilityEvaluation =
        let helper target acc ip =
            let currentReachableCost, currentCostToExit =
                match acc with
                | Reachable(x, y) -> x, y
                | Unknown -> UNKNOWN_CONSTANT, 0
            let findNewCost price =
                Reachable(min(currentReachableCost, currentCostToExit + price), currentCostToExit + COST_OF_EXIT)
            match ip2codeLocation ip with
            | Some loc when not <| reachableLocations.ContainsKey(loc) -> findNewCost COST_TO_GO_TO_ENDFINALLY
            | Some loc when List.contains target (loc2Locs loc) -> findNewCost REGULAR_COST
            | Some loc when List.contains target.method (loc2Methods loc) -> findNewCost COST_OF_CALL
            | Some loc when Seq.exists (fun m -> List.contains target.method (method2Methods m)) (loc2Methods loc) ->
                findNewCost COST_OF_MANY_CALLS
            | _  -> Reachable(currentReachableCost, currentCostToExit + COST_OF_EXIT)
        List.fold (helper target) Unknown ipStack

//    member x.Append (s : cilState) =
//        if x.CanReach (s.ipStack, []) then
    member x.CanReach(ipStack, blocked) =
        match canReachMetrics(ipStack, blocked) with
        | Unknown -> __unreachable__()
        | Reachable(x, _) -> x <> UNKNOWN_CONSTANT

    interface IComparer<cilState> with
        override x.Compare (a : cilState, b : cilState) =
//            if a.stepsNumber > 1000u then 1 else
            let aCanReach = canReachMetrics(a.ipStack, [])
            let bCanReach = canReachMetrics(b.ipStack, [])
            match aCanReach, bCanReach with
            | Reachable(x, _), Reachable(y, _) when x < y -> -1
            | Reachable(x, _), Reachable(y, _) when x = y -> 0
//                if a.stepsNumber < b.stepsNumber then -1
//                elif a.stepsNumber = b.stepsNumber then 0
//                else 1
            | Reachable(x, _), Reachable(y, _)  -> 1
            | _ -> __unreachable__()

type OneTargetedSearcher(maxBound, target : codeLocation, reachableLocations, reachableMethods, methodsReachabilityTransitiveClosure ) =
    let comparer = cilstatesComparer(target, reachableLocations, reachableMethods, methodsReachabilityTransitiveClosure)
    let priorityQueue  = C5.IntervalHeap<cilState>(comparer)
    let mutable stepsNumber = 0u

    member x.GetNext() =
        match priorityQueue.IsEmpty with
        | true -> None
        | _ ->
            let s = priorityQueue.FindMin()
            priorityQueue.DeleteMin() |> ignore
            Some s

    interface INewSearcher with
        override x.CanReach(ipStack : ip list, target : ip, blocked : ip list) = comparer.CanReach(ipStack, blocked)
        override x.MaxBound = maxBound
        override x.AppendNewStates states =
            List.iter (fun s -> if comparer.CanReach(s.ipStack, []) then priorityQueue.Add s |> ignore) states
        override x.Reset() =
            Logger.warning "steps number done by %O = %d" (x.GetType()) stepsNumber
            stepsNumber <- 0u
        override x.Init(_,_) = ()
        override x.ChooseAction(_,_,_,_) =
            __notImplemented__()

type TargetedSearcher(maxBound) =
    let reachableLocations = Dictionary<codeLocation, codeLocation list>()
    let reachableMethods = Dictionary<codeLocation, MethodBase list>()
    let methodsReachability = Dictionary<MethodBase, MethodBase list>()
    let methodsReachabilityTransitiveClosure = Dictionary<MethodBase, MethodBase list>()

    let mutable entryMethod : MethodBase = null
    let mutable stepsNumber = 0u
    let searchers = List<OneTargetedSearcher>()
    let loc2Searcher = Dictionary<codeLocation, OneTargetedSearcher>()
    let appendReachableInfo (cfg : cfg) (reachableLocsForSCC : HashSet<codeLocation>) (reachableMethodsForSCC : HashSet<MethodBase>) (current : offset) =
        let currentLoc = {offset = current; method = cfg.methodBase}
        reachableLocsForSCC.Add(currentLoc) |> ignore
        if cfg.offsetsDemandingCall.ContainsKey current then
           let _, calledMethod = cfg.offsetsDemandingCall.[current]
           if calledMethod.DeclaringType.Assembly = entryMethod.DeclaringType.Assembly then
            reachableMethodsForSCC.Add(calledMethod) |> ignore

        let helper (target : offset) =
            let loc = {offset = target; method = cfg.methodBase}
            if not <| reachableLocations.ContainsKey loc then ()
            List.iter (reachableLocsForSCC.Add >> ignore) reachableLocations.[loc]
            List.iter (reachableMethodsForSCC.Add >> ignore) reachableMethods.[loc]
        let targets = cfg.graph.[current]
        Seq.iter helper targets

    let commitReachableInfo (cfg : cfg) (reachableLocsForSCC : HashSet<codeLocation>) (reachableMethodsForSCC : HashSet<MethodBase>) (current : offset) =
        let currentLoc = {offset = current; method = cfg.methodBase}
        reachableLocations.[currentLoc] <-  List.ofSeq (reachableLocsForSCC)
        reachableMethods.[currentLoc] <- List.ofSeq (reachableMethodsForSCC)

    let initReachableInfo (cfg : cfg) (current : offset) =
        let currentLoc = {offset = current; method = cfg.methodBase}
        reachableLocations.Add(currentLoc, [])
        reachableMethods.Add(currentLoc, [])
    let buildReachabilityInfo (currentMethod : MethodBase) : MethodBase list =
        let cfg = CFG.build currentMethod
        let rec dfsSCC (usedSCC : int list) (v : offset) : int list =
            let currentSCC = cfg.sccOut.[v]
            if List.contains currentSCC usedSCC then usedSCC
            else
                let usedSCC = currentSCC :: usedSCC
                let currentSCCOffsets = Seq.filter (fun offset -> currentSCC = cfg.sccOut.[offset]) cfg.sortedOffsets
                let newUsed = Seq.fold (fun acc u1 -> Seq.fold (fun acc u2 -> dfsSCC acc u2) acc cfg.graph.[u1]) usedSCC currentSCCOffsets
                let reachableLocsForSCC = HashSet<codeLocation>()
                let reachableMethodsForSCC = HashSet<MethodBase>()
                Seq.iter (initReachableInfo cfg) currentSCCOffsets
                Seq.iter (appendReachableInfo cfg reachableLocsForSCC reachableMethodsForSCC) currentSCCOffsets
                Seq.iter (commitReachableInfo cfg reachableLocsForSCC reachableMethodsForSCC) currentSCCOffsets
                newUsed
        let _ = dfsSCC [] 0 //TODO: what about EHC?
        reachableMethods.[{offset = 0; method = currentMethod}]

    let addCall (current : MethodBase) (calledMethods : MethodBase list) =
        let add m (ms : MethodBase list) (d : Dictionary<_, MethodBase list >) =
            if d.ContainsKey m then d.[m] <- ms @ d.[m]
            else d.Add(m, ms)

        add current calledMethods methodsReachability

    let buildReachability () =
        let rec exit processedMethods = function
                | [] -> ()
                | m :: q' -> findFixPoint (processedMethods, q') m
        and findFixPoint (processedMethods : MethodBase list, methodsQueue : MethodBase list) (current : MethodBase) =
            if List.contains current processedMethods then exit processedMethods methodsQueue
            else
                let processedMethods = current :: processedMethods
                let calledMethods = buildReachabilityInfo current
                addCall current calledMethods

                exit processedMethods (methodsQueue @ calledMethods)
        findFixPoint ([],[]) entryMethod

    let makeTransitiveClosure () =
        let findReachableMethodsForMethod (current : MethodBase) =
            Logger.info "Iterating for %O" current
            methodsReachabilityTransitiveClosure.Add(current, [])
            let used = HashSet<MethodBase>()
            let rec dfs (v : MethodBase) =
                if used.Contains v then ()
                else
                    used.Add(v) |> ignore
                    methodsReachabilityTransitiveClosure.[current] <- v :: methodsReachability.[current]
                    List.iter dfs (methodsReachability.[v])
            List.iter dfs methodsReachability.[current]
        Seq.iter findReachableMethodsForMethod (methodsReachability.Keys)
    let print () =
        Logger.info "Calculated CFG Reachability\n"
        methodsReachabilityTransitiveClosure |> Seq.iter (fun kvp ->
            let value = List.fold (fun (sb : StringBuilder) m -> sb.AppendFormat("{0}; ", m.ToString())) (StringBuilder()) kvp.Value
            Logger.info "key = %O; Value = %s" kvp.Key (value.ToString()))

    interface INewSearcher with
        override x.CanReach(ipStack : ip list, target : ip, blocked : ip list) =
            Seq.fold (fun acc (s : OneTargetedSearcher) -> acc || (s :> INewSearcher).CanReach(ipStack, target, blocked)) false searchers
        override x.MaxBound = maxBound
        override x.AppendNewStates states =
            let appendStateToSearcher (s : OneTargetedSearcher) =
                let s = s :> INewSearcher
                s.AppendNewStates states
            Seq.iter appendStateToSearcher searchers

        override x.Init(mainM, locs) =
            let createSearcher loc =
                let s = OneTargetedSearcher(maxBound, loc, reachableLocations, reachableMethods, methodsReachabilityTransitiveClosure)
                searchers.Add(s)
                loc2Searcher.Add(loc, s)
            (x :> INewSearcher).Reset()
            entryMethod <- mainM
            buildReachability ()
            makeTransitiveClosure ()
            Seq.iter createSearcher locs
        override x.Reset() =
//            Logger.warning "steps number done by TS = %d" stepsNumber
//            stepsNumber <- 0u
            Logger.warning "steps number done by %O = %d" (x.GetType()) stepsNumber
            stepsNumber <- 0u
            searchers.Clear()
            reachableLocations.Clear()
            reachableMethods.Clear()
            methodsReachability.Clear()
            methodsReachabilityTransitiveClosure.Clear()
            loc2Searcher.Clear()
            entryMethod <- null
        override x.ChooseAction(qf,qb,pobs,main) =
            let tryFindAction acc (searcher : OneTargetedSearcher) =
                match acc with
                | Some _ -> acc
                | _ -> searcher.GetNext()
//            let tryFindState () =
//                    match priorityQueue.IsEmpty with
//                    | true -> None
//                    | _ ->
//                        let s = priorityQueue.FindMin()
//
//                        priorityQueue.DeleteMin() |> ignore
//                        Some s

            match qf, qb, pobs with
            | [], _, _ -> Start(Instruction(0, main))
            | _, b ::_, _ -> GoBackward(b)
            | _, _, [] -> Stop
            | _, _, p :: _ ->
                let s = Seq.fold tryFindAction None searchers
                match s with
                | None -> Stop
                | Some s ->
                    stepsNumber <- stepsNumber + 1u
                    GoForward s
//                let s = tryFindState ()
//                match s with
//                | None -> Stop
//                | Some s ->
//                    stepsNumber <- stepsNumber + 1u
//                    GoForward s




