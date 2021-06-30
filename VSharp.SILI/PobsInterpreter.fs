namespace VSharp.Interpreter.IL

open System
open System.Collections.Generic
open System.Reflection
open System.Reflection.Emit
open System.Text
open C5
open FSharpx.Collections
open System.Reflection

open VSharp
open VSharp.Core
open CilStateOperations

type public PobsInterpreter(maxBound, searcher : INewSearcher) =
    inherit ExplorerBase()
    let mutable curLvl = maxBound
    let infty = System.Int32.MaxValue
    let qFront = FrontQueue(maxBound)
    let qBack = List<pob * cilState>()
    let mainPobs = List<pob>()
    let currentPobs = List<pob>()
    let loc2pob = Dictionary<ip, List<pob>>()
    let answeredPobs = Dictionary<pob, pobStatus>()
    let childOf = Dictionary<pob, pob>()

    let doAddPob (pob : pob) =
        currentPobs.Add(pob)
        let mutable pobsRef = ref null
        if not <| loc2pob.TryGetValue(pob.loc, pobsRef) then
            pobsRef <- ref (List<pob>())
            loc2pob.Add(pob.loc, !pobsRef)
        (!pobsRef).Add pob

    let addPob(oldPob : pob, newPob : pob) =
        assert(childOf.ContainsKey(newPob) |> not)
        childOf.Add(newPob, oldPob)
        doAddPob newPob

    let rec answerYes (s' : cilState, p' : pob) =
        if (not <| loc2pob.ContainsKey(p'.loc)) then
            assert(mainPobs.Contains p')
        else
            let list = loc2pob.[p'.loc]
            list.Remove(p') |> ignore
            if list.Count = 0 then loc2pob.Remove(p'.loc) |> ignore
        currentPobs.Remove(p') |> ignore
        qBack.RemoveAll(fun (p, _) -> p = p') |> ignore
        if Seq.contains p' mainPobs then
            mainPobs.Remove(p') |> ignore
        if not(answeredPobs.ContainsKey p') then answeredPobs.Add(p', Witnessed s')
        else answeredPobs.[p'] <- Witnessed s'
        if childOf.ContainsKey p' then
            if p' = childOf.[p'] then internalfailf "Pob has itself as parent"
            answerYes(s', childOf.[p'])

    member x.MakeCilStateForIp (ip : ip) =
        let m = methodOf ip
        let cilStates = x.FormInitialState m
        cilStates |> List.map (fun s -> {s with startingIP = ip} |> withIpStack [ip])

    //NOTE: Must be called for ip with empty evaluation stack!
    member x.Start (ip : ip) =
        let starts = x.MakeCilStateForIp ip
        qFront.AddGoodStates(starts)
    member x.Forward (s : cilState) =
        let removed = qFront.RemoveAll(fun s' -> LanguagePrimitives.PhysicalEquality s s')
        in
//            if removed <= 0 then ()
            assert(removed > 0)
        let goodStates, iieStates, errors = ILInterpreter(x).ExecuteOnlyOneInstruction s
        qFront.AddGoodStates(goodStates)
        qFront.AddIIEStates(iieStates)
        qFront.AddErroredStates(errors)

        goodStates |> List.iter (fun (s' : cilState) ->
            let pobsList = ref null
            if loc2pob.TryGetValue(currentIp s', pobsList) then
                !pobsList |> Seq.iter (fun p -> qBack.Add(p, s'))
        )

    member x.OverApproximate(_ : ip, _ : int) : term = Terms.True

    member x.Backward (p' : pob, s' : cilState, EP : ip) =
        let removed = qBack.Remove(p',s') in assert(removed)
        assert(currentIp s' = p'.loc)
        let lvl = p'.lvl - int(levelToInt s'.level)
        let fml = Memory.WLP s'.state p'.fml
        match Memory.IsSAT fml with
        | true when s'.startingIP = EP -> answerYes(s', p')
        | true ->
            let p = {loc = s'.startingIP; lvl = lvl; fml = fml}
            addPob(p', p)
        | false ->
            Logger.info "UNSAT for pob = %O and s'.PC = %O" p' s'.state.pc
    member x.BidirectionalSymbolicExecution (main : MethodBase) (EP : ip) mainPobsList =
        Seq.iter mainPobs.Add mainPobsList
        let createPobs () = mainPobs |> Seq.iter (fun (mp : pob) ->
                let p = {mp with lvl = curLvl}
                childOf.Add(p, mp)
                doAddPob p)
        while mainPobs.Count > 0 && curLvl <= maxBound do
            createPobs()
            while currentPobs.Count > 0 do
                match searcher.ChooseAction(qFront, List.ofSeq qBack, List.ofSeq currentPobs, main) with
                | Stop -> currentPobs.Clear()
                | Start loc -> x.Start(loc)
                | GoForward s ->
                    Logger.info "GoForward: ip = %O" (currentIp s)
                    x.Forward(s)
                | GoBackward(p', s') -> x.Backward(p',s', EP)
            curLvl <- curLvl + 1
    member x.ClearStructures () =
        curLvl <- maxBound
        mainPobs.Clear()
        qFront.Clear()
        qBack.Clear()
        currentPobs.Clear()
        answeredPobs.Clear()
        childOf.Clear()
//        searcher.Reset()
    override x.Invoke _ _ _ = __notImplemented__()
    override x.AnswerPobs entryMethod codeLocations k =
        let printLocInfo (loc : codeLocation) =
            Logger.info "Got loc {%O, %O}" loc.offset loc.method
        Seq.iter printLocInfo codeLocations

        x.ClearStructures()
        let mainPobs =
            codeLocations
            |> Seq.filter (fun loc -> loc.method.GetMethodBody().GetILAsByteArray().Length > loc.offset)
            |> Seq.map (fun (loc : codeLocation) -> {loc = Instruction(loc.offset, loc.method); lvl = infty; fml = Terms.True})
        Seq.iter (fun p -> answeredPobs.Add(p, Unknown)) mainPobs
        let EP = Instruction(0, entryMethod)
        searcher.Init(entryMethod, codeLocations)
        x.BidirectionalSymbolicExecution entryMethod EP mainPobs
        searcher.Reset()
//        let showResultFor (mp : pob) =
//            match answeredPobs.[mp] with
//            | Unreachable -> Logger.info "NO: MainPob = %O" mp
//            | Witnessed s -> Logger.info "YES: MainPob = %O, witness = %O" mp s
//            | Unknown -> Logger.info "Unknown: MainPob = %O" mp
//        List.iter showResultFor mainPobs
        let addLocationStatus (acc : Dictionary<codeLocation, string>) (loc : codeLocation) =
            let pob = {loc = Instruction(loc.offset, loc.method); lvl = infty; fml = Terms.True}
            let result =
                match answeredPobs.[pob] with
                | Witnessed _ -> "Witnessed"
                | status -> status.ToString()
            acc.Add(loc, result)
            acc
        let result = codeLocations |> Seq.fold addLocationStatus (Dictionary<codeLocation, string>())
        k result

