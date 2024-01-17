namespace VSharp.Explorer

open System.Collections.Generic
open FSharpx.Collections

open VSharp
open VSharp.Interpreter.IL
open CilState

type BidirectionalSearcher(forward : IForwardSearcher, backward : IBackwardSearcher, targeted : ITargetedSearcher) =

    interface IBidirectionalSearcher with

        override x.Init cilStates mainPobs =
            backward.Init mainPobs
            forward.Init cilStates

        override x.Statuses() = backward.Statuses()
        override x.Answer pob pobStatus = backward.Answer pob pobStatus
        override x.UpdatePobs parent child =
            backward.Update parent child
        override x.UpdateStates parent children =
            if not parent.IsIsolated then
                forward.Update (parent, children)
                backward.AddBranch parent |> ignore
                Seq.iter (backward.AddBranch >> ignore) children
            else
                let reached = targeted.Update parent children
                Seq.iter (backward.AddBranch >> ignore) reached
        override x.States() = forward.States()

        override x.Pick () =
            match forward.Pick() with
            | Some s -> GoFront s
            | None -> Stop

        override x.Reset () =
            forward.Reset()
            backward.Reset()
            targeted.Reset()

        override x.Remove cilState =
            forward.Remove cilState
            backward.Remove cilState
            targeted.Remove cilState

        override x.StatesCount with get() =
            forward.StatesCount + backward.StatesCount + targeted.StatesCount

type OnlyForwardSearcher(searcher : IForwardSearcher) =
    interface IBidirectionalSearcher with
        override x.Init cilStates _ = searcher.Init cilStates
        override x.Statuses() = []
        override x.Answer _ _ = ()
        override x.UpdatePobs _ _ = ()
        override x.UpdateStates parent children = searcher.Update(parent, children)
        override x.Pick () =
            match searcher.Pick() with
            | Some s -> GoFront s
            | None -> Stop
        override x.States() = searcher.States()
        override x.Reset() = searcher.Reset()
        override x.Remove cilState = searcher.Remove cilState
        override x.StatesCount with get() = searcher.StatesCount

// TODO: check pob duplicates
type BackwardSearcher() =
    let mainPobs = List<pob>()
    let currentPobs = List<pob>()
    let qBack = List<cilState * pob>()
    let alreadyAddedInQBack = HashSet<cilState * pob>()
    let ancestorOf = Dictionary<pob, List<pob>>()
    let answeredPobs = Dictionary<pob, pobStatus>()
    let loc2pob = Dictionary<codeLocation, List<pob>>()

    let doAddPob (pob : pob) =
        currentPobs.Add(pob)
        let mutable pobsRef = ref null
        if not <| loc2pob.TryGetValue(pob.loc, pobsRef) then
            pobsRef <- ref (List<pob>())
            loc2pob.Add(pob.loc, pobsRef.Value)
        pobsRef.Value.Add pob

    let addPob parent child =
        if not <| ancestorOf.ContainsKey(child) then
            ancestorOf.Add(child, List<_>())
        ancestorOf[child].Add(parent)
        doAddPob child

    let updateQBack (s : cilState) : pob list =
        match s.CurrentIp.ToCodeLocation() with
        | None -> []
        | Some loc ->
            let pobsList = ref null
            if loc2pob.TryGetValue(loc, pobsList) then
                pobsList.Value |> Seq.iter (fun p ->
                    if not <| alreadyAddedInQBack.Contains(s, p) then
                        alreadyAddedInQBack.Add(s, p) |> ignore
                        qBack.Add(s, p))
                pobsList.Value |> List.ofSeq
            else []

    let rec answerYes (s' : cilState) (p' : pob) =
        if (not <| loc2pob.ContainsKey(p'.loc)) then
            assert(mainPobs.Contains p')
        else
            let list = loc2pob[p'.loc]
            list.Remove(p') |> ignore
            if list.Count = 0 then loc2pob.Remove(p'.loc) |> ignore
        currentPobs.Remove(p') |> ignore
        qBack.RemoveAll(fun (_, p as pair) -> if p = p' then alreadyAddedInQBack.Remove(pair) |> ignore; true else false) |> ignore
        if Seq.contains p' mainPobs then
            mainPobs.Remove(p') |> ignore
        if not(answeredPobs.ContainsKey p') then answeredPobs.Add(p', Witnessed s')
        else answeredPobs[p'] <- Witnessed s'
        Application.removeGoal p'.loc
        if ancestorOf.ContainsKey p' then
            Seq.iter (fun (ancestor : pob) ->
                assert(p' <> ancestor)
                if currentPobs.Contains(ancestor) || mainPobs.Contains(ancestor) then
                    answerYes s' ancestor) ancestorOf[p']

    let clear() =
        mainPobs.Clear()
        currentPobs.Clear()
        qBack.Clear()
        alreadyAddedInQBack.Clear()
        ancestorOf.Clear()
        answeredPobs.Clear()

    interface IBackwardSearcher with
        override x.Init pobs =
            clear()
            Seq.iter mainPobs.Add pobs
            Seq.iter (fun p -> answeredPobs.Add(p, pobStatus.Unknown)) pobs
            Seq.iter doAddPob pobs

        override x.Update parent child = addPob parent child

        override x.Answer pob status =
            match status with
            | Witnessed s' -> answerYes s' pob
            | _ -> __notImplemented__()
        override x.Statuses () = Seq.map (fun (kvp : KeyValuePair<pob, pobStatus>) -> kvp.Key, kvp.Value) answeredPobs

        override x.Pick() =
            if qBack.Count > 0 then
                let ps = qBack[0]
                qBack.RemoveAt(0)
                Propagate ps
            else NoAction

        override x.AddBranch cilState =
            updateQBack cilState

        override x.Remove cilState =
            let removePredicate (cilState', _ as pair) =
                if cilState = cilState' then alreadyAddedInQBack.Remove(pair) |> ignore; true
                else false
            let count = qBack.RemoveAll(removePredicate)
            if count > 0 then
                internalfail "BackwardSearcher.Remove: count > 0"

        override x.Reset() = clear()

        override x.StatesCount with get() = qBack.Count

module DummyTargetedSearcher =

    type DummyTargetedSearcher() =
        let forPropagation = Dictionary<instructionPointer, List<cilState>>()
        let finished = Dictionary<instructionPointer, List<cilState>>()
        let reached = Dictionary<instructionPointer, List<cilState>>()
        let targets = Dictionary<instructionPointer, List<instructionPointer>>()

        let addReached from s =
            let mutable l = ref null
            if not (reached.TryGetValue(from, l)) then
                l <- ref (List<cilState>())
                reached.Add(from, l.Value)
            l.Value.Add s
            Seq.singleton s
        let add (s : cilState) : cilState seq =
            let from = s.startingIP
            assert(targets.ContainsKey from)
            let current = s.CurrentIp

            if s.IsIIEState || s.IsUnhandledExceptionOrError || not s.IsExecutable then
                finished[from].Add(s); Seq.empty
            elif targets[from].Contains(current) then addReached from s
            else
                let list = forPropagation[from]
                if not <| list.Contains s then list.Add(s)
                Seq.empty

        interface ITargetedSearcher with
            override x.SetTargets from tos =
                if not (targets.ContainsKey from) then targets.Add(from, List<instructionPointer>())
                targets[from].AddRange(tos)

            override x.Update parent children =
                let from = parent.startingIP
                assert(targets.ContainsKey from)
                Seq.map add (Seq.cons parent children) |> Seq.concat

            override x.Pick () =
                targets.Keys |> Seq.tryPick (fun ip -> forPropagation[ip] |> Seq.tryHead)

            override x.Reset () =
                forPropagation.Clear()
                finished.Clear()
                reached.Clear()
                targets.Clear()

            override x.Remove cilState =
                forPropagation.Values |> Seq.iter (fun s -> s.Remove cilState |> ignore)
                finished.Values |> Seq.iter (fun s -> s.Remove cilState |> ignore)
                reached.Values |> Seq.iter (fun s -> s.Remove cilState |> ignore)

            override x.StatesCount with get() = forPropagation.Values.Count
