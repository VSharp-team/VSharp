namespace VSharp.Interpreter.IL

open System.Reflection
open System.Collections.Generic
open FSharpx.Collections
open VSharp
open VSharp.Core
open VSharp.Utils

type BidirectionalSearcher(forward : IForwardSearcher, backward : IBackwardSearcher, targeted : ITargetedSearcher) =

//    let starts = Queue<MethodBase>()
//    let addedStarts = HashSet<MethodBase>()
    let mutable mainMethod = null
//    let mutable inverseReachability : Dictionary<MethodBase, HashSet<MethodBase>> = null

//    let rememberStart (m : MethodBase) =
//        if addedStarts.Contains(m) then ()
//        else
//            addedStarts.Add(m) |> ignore
//            starts.Enqueue(m)
//    let startFrom (m : MethodBase) =
//        assert(addedStarts.Contains(m))
////        Logger.warning "Starting for method = %s" (Reflection.getFullMethodName m)
//        Start(Instruction(0x00, m))
//    let getInverse (ip : ip) =
//        let m = CilStateOperations.methodOf ip
//        if inverseReachability.ContainsKey(m) then inverseReachability.[m]
//        else HashSet<_>()



    interface IBidirectionalSearcher with
//            let _, inverseReachability' = CFG.buildMethodsReachabilityForAssembly m
//            inverseReachability <- inverseReachability'
//            Seq.iter (fun p -> rememberStart p.loc.method) mainPobs
//        override x.PriorityQueue _ = StackFrontQueue() :> IPriorityQueue<cilState>


        override x.Statuses() = backward.Statuses()
        override x.Answer pob pobStatus = backward.Answer pob pobStatus
        override x.UpdatePobs parent child =
            backward.Update parent child
        override x.UpdateStates parent children =
            let loc = ipOperations.ip2codeLocation parent.startingIP
            match loc with
            | Some loc when loc.method = mainMethod && loc.offset = 0x0 ->
                forward.Update parent children
                backward.AddBranch parent |> ignore
                Seq.iter (backward.AddBranch >> ignore) children
            | _ ->
                let reached = targeted.Update parent children
                Seq.iter (backward.AddBranch >> ignore) reached

        override x.Pick () =
            match backward.Pick() with
            | Propagate(s,p) -> GoBack (s,p)
            | InitTarget(from, pobs) ->
                let tos = Seq.map (fun (pob : pob) -> Instruction(pob.loc.offset, pob.loc.method)) pobs
                targeted.SetTargets from tos
                GoFront(targeted.Pick())
            | NoAction ->
                if targeted.ShouldWork() then
                    GoFront(targeted.Pick())
                elif forward.ShouldWork() then
                    let state = forward.Pick()
                    backward.RemoveBranch state
                    GoFront state
                else Stop
    interface IResettableSearcher with
        override x.Init m mainPobs =
            mainMethod <- m
            backward.Init m mainPobs
//            let start : cilState = CilStateOperations.makeInitialState m (ExplorerBase.FormInitialStateWithoutStatics m)
            forward.Init m mainPobs
        override x.ShouldWork() =
            // TODO: add has progress
            backward.ShouldWork()
        override x.Reset () =
            mainMethod <- null
            forward.Reset()
            backward.Reset()
            targeted.Reset()


type BackwardSearcher() =
    let mainPobs = List<pob>()
    let currentPobs = List<pob>()
    let qBack = List<cilState * pob>()
    let alreadyAddedInQBack = HashSet<cilState * pob>()
    let ancestorOf = Dictionary<pob, List<pob>>()
    let answeredPobs = Dictionary<pob, pobStatus>()
    let loc2pob = Dictionary<codeLocation, List<pob>>()
    let mutable mainMethod : MethodBase = null

    let doAddPob (pob : pob) =
        currentPobs.Add(pob)
        let mutable pobsRef = ref null
        if not <| loc2pob.TryGetValue(pob.loc, pobsRef) then
            pobsRef <- ref (List<pob>())
            loc2pob.Add(pob.loc, !pobsRef)
        (!pobsRef).Add pob

    let addPob parent child =
        if not <| ancestorOf.ContainsKey(child) then
            ancestorOf.Add(child, List<_>())
        ancestorOf.[child].Add(parent)
        doAddPob child

    let updateQBack s : pob list =
        match ipOperations.ip2codeLocation (CilStateOperations.currentIp s) with
        | None -> []
        | Some loc ->
            let pobsList = ref null
            if loc2pob.TryGetValue(loc, pobsList) then
                !pobsList |> Seq.iter (fun p ->
                    if not <| alreadyAddedInQBack.Contains(s, p) then
                        alreadyAddedInQBack.Add(s, p) |> ignore
                        qBack.Add(s, p))
                !pobsList |> List.ofSeq
            else []

    let rec answerYes (s' : cilState) (p' : pob) =
        if (not <| loc2pob.ContainsKey(p'.loc)) then
            assert(mainPobs.Contains p')
        else
            let list = loc2pob.[p'.loc]
            list.Remove(p') |> ignore
            if list.Count = 0 then loc2pob.Remove(p'.loc) |> ignore
        currentPobs.Remove(p') |> ignore
        qBack.RemoveAll(fun ((_, p) as pair) -> if p = p' then alreadyAddedInQBack.Remove(pair) |> ignore; true else false) |> ignore
        if Seq.contains p' mainPobs then
            mainPobs.Remove(p') |> ignore
        if not(answeredPobs.ContainsKey p') then answeredPobs.Add(p', Witnessed s')
        else answeredPobs.[p'] <- Witnessed s'
        if ancestorOf.ContainsKey p' then
//            assert(ancestorOf.[p'] <> null)
            Seq.iter (fun (ancestor : pob) ->
                assert(p' <> ancestor)
                if currentPobs.Contains(ancestor) || mainPobs.Contains(ancestor) then
                    answerYes s' ancestor) ancestorOf.[p']

    let clear() =
        mainMethod <- null
        mainPobs.Clear()
        currentPobs.Clear()
        qBack.Clear()
        alreadyAddedInQBack.Clear()
        ancestorOf.Clear()
        answeredPobs.Clear()

    interface IResettableSearcher with

        override x.Reset() = clear()
        override x.Init m pobs =
            clear()
            mainMethod <- m
            Seq.iter mainPobs.Add pobs
            Seq.iter (fun p -> answeredPobs.Add(p, pobStatus.Unknown)) pobs
            Seq.iter doAddPob pobs
        override x.ShouldWork() =
            mainPobs.Count > 0
    interface IBackwardSearcher with
        override x.Update parent child = addPob parent child

        override x.Answer pob status =
            match status with
            | Witnessed s' -> answerYes s' pob
            | _ -> __notImplemented__()
        override x.Statuses () = Seq.map (fun (kvp : KeyValuePair<pob, pobStatus>) -> kvp.Key, kvp.Value) answeredPobs

        override x.Pick() =
            if qBack.Count > 0 then
                let ps = qBack.[0]
                qBack.RemoveAt(0)
                Propagate ps
            else NoAction

        override x.AddBranch cilState =
            updateQBack cilState

        override x.RemoveBranch cilState =
            let count = qBack.RemoveAll(fun ((cilState', _) as pair) -> if cilState = cilState' then alreadyAddedInQBack.Remove(pair) |> ignore; true else false)
            if count > 0 then
                internalfail "olololo!"

module TargetedSearcher =
    open CilStateOperations

    type DummyTargetedSearcher() =
        let forPropagation = Dictionary<ip, List<cilState>>()
        let finished = Dictionary<ip, List<cilState>>()
        let reached = Dictionary<ip, List<cilState>>()
        let targets = Dictionary<ip, List<ip>>()

        let addReached from s =
            let mutable l = ref null
            if not (reached.TryGetValue(from, l)) then
                l <- ref (List<cilState>())
                reached.Add(from, !l)
            (!l).Add s
            Seq.singleton s
        let add (s : cilState) : cilState seq =
            let from = s.startingIP
            assert(targets.ContainsKey from)
            let current = currentIp s

            if isIIEState s || isError s || not(isExecutable(s)) then
                finished.[from].Add(s); Seq.empty
            elif targets.[from].Contains(current) then addReached from s
            else
                let list = forPropagation.[from]
                if not <| list.Contains s then list.Add(s)
                Seq.empty

        interface IResettableSearcher with
            override x.Init _ _ = ()
            override x.Reset() =
                forPropagation.Clear()
                finished.Clear()
                reached.Clear()
                targets.Clear()
            override x.ShouldWork () =
                Seq.fold (fun acc (kvp : KeyValuePair<ip, List<cilState>>) -> acc || kvp.Value.Count > 0) false forPropagation
        interface ITargetedSearcher with
            override x.SetTargets from tos =
                if not (targets.ContainsKey from) then targets.Add(from, List<ip>())
                targets.[from].AddRange(tos)

            override x.Update parent children =
                let from = parent.startingIP
                assert(targets.ContainsKey from)
//                forPropagation.[from].Remove(parent) |> ignore
                Seq.map add (Seq.cons parent children) |> Seq.concat

            override x.Pick () =
                let ip = targets.Keys |> Seq.filter (fun ip -> forPropagation.[ip].Count > 0) |> Seq.head
                forPropagation.[ip].[0]

