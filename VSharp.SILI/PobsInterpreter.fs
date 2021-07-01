namespace VSharp.Interpreter.IL

open System
open System.Collections.Generic
open System.Reflection
open FSharpx.Collections
open System.Reflection

open VSharp
open VSharp.Core
open VSharp.Utils
open ipOperations
open CilStateOperations

type public PobsInterpreter(maxBound, searcher : INewSearcher) =
    inherit ExplorerBase()
    let mutable curLvl = maxBound
    let infty = System.UInt32.MaxValue
    let qFront = FrontQueue(maxBound)
    let qBack = List<pob * cilState>()
    let mainPobs = List<pob>()
    let currentPobs = List<pob>()
    let loc2pob = Dictionary<ip, List<pob>>()
    let answeredPobs = Dictionary<pob, pobStatus>()
    let ancestorOf = Dictionary<pob, pob>()

    let doAddPob (pob : pob) =
        currentPobs.Add(pob)
        let mutable pobsRef = ref null
        if not <| loc2pob.TryGetValue(pob.loc, pobsRef) then
            pobsRef <- ref (List<pob>())
            loc2pob.Add(pob.loc, !pobsRef)
        (!pobsRef).Add pob

    let addPob(ancestor : pob, child : pob) =
        assert(ancestorOf.ContainsKey(child) |> not)
        ancestorOf.Add(child, ancestor)
        doAddPob child
    let existAnyMainPob() = mainPobs.Count > 0

    let createPobs () =
        Seq.iter (fun (mp : pob) ->
            let p = {mp with lvl = curLvl}
            ancestorOf.Add(p, mp)
            doAddPob p) mainPobs

    let updateQBack ip p =
        // TODO: choose cilState for GoForward and update qBack for this cilState #md do
        qFront.StatesForPropagation() |> Seq.iter (fun s -> if currentIp s = ip then qBack.Add(p, s))

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
        if ancestorOf.ContainsKey p' then
//            assert(ancestorOf.[p'] <> null)
            let ancestor = ancestorOf.[p']
            if p' = ancestor then internalfailf "Pob has itself as parent"
            if currentPobs.Contains(ancestor) || mainPobs.Contains(ancestor) then
                answerYes(s', ancestorOf.[p'])

    member x.MakeCilStateForIp (ip : ip) =
        let m = methodOf ip
        let cilStates = x.FormInitialState m
        cilStates |> List.map (fun s -> {s with startingIP = ip} |> withIpStack [ip])

    //NOTE: Must be called for ip with empty evaluation stack!
    member x.Start (ip : ip) =
        try
            let starts = x.MakeCilStateForIp ip
            qFront.AddGoodStates(starts)
        with
        | :? InsufficientInformationException -> Logger.info "Could not START from %O" ip
    member x.Forward (s : cilState) =
        let removed = qFront.Remove(s)
        assert(removed)
        let goodStates, iieStates, errors = ILInterpreter(x).ExecuteOnlyOneInstruction s
        qFront.AddGoodStates(goodStates)
        qFront.AddIIEStates(iieStates)
        qFront.AddErroredStates(errors)

        goodStates |> List.iter (fun (s' : cilState) ->
            let pobsList = ref null
            if loc2pob.TryGetValue(currentIp s', pobsList) then
                !pobsList |> Seq.iter (fun p -> qBack.Add(p, s'))
        )
    member x.Backward (p' : pob, s' : cilState, EP : ip) =
        let removed = qBack.Remove(p',s') in assert(removed)
        assert(currentIp s' = p'.loc)
        let sLvl = levelToUnsignedInt s'.level
        if p'.lvl >= sLvl then
            let lvl = p'.lvl - sLvl
            let pc = Memory.WLP s'.state p'.pc
            match Memory.IsSAT pc with
            | true when s'.startingIP = EP -> answerYes(s', p')
            | true ->
                let p = {loc = s'.startingIP; lvl = lvl; pc = pc}
                Logger.warning "Backward:\nWas: %O\nNow: %O\n\n" p' p
                addPob(p', p)
                updateQBack s'.startingIP p
            | false ->
                Logger.info "UNSAT for pob = %O and s'.PC = %O" p' s'.state.pc
    member x.BidirectionalSymbolicExecution (main : MethodBase) (EP : ip) mainPobsList =
        Seq.iter mainPobs.Add mainPobsList
        let createPobs () = mainPobs |> Seq.iter (fun (mp : pob) ->
                let p = {mp with lvl = curLvl}
                ancestorOf.Add(p, mp)
                doAddPob p)
        while existAnyMainPob() && curLvl <= maxBound do
            createPobs()
            while currentPobs.Count > 0 && existAnyMainPob() do
                match searcher.ChooseAction(qFront, qBack, currentPobs) with
                | Stop -> currentPobs.Clear()
                | Start loc -> x.Start(loc)
                | GoForward s ->
                    Logger.info "GoForward: ip = %O" (currentIp s)
                    x.Forward(s)
                | GoBackward(p', s') -> x.Backward(p',s', EP)
            curLvl <- curLvl + 1u
    member x.ClearStructures () =
        curLvl <- maxBound
        mainPobs.Clear()
        qFront.Clear()
        qBack.Clear()
        currentPobs.Clear()
        answeredPobs.Clear()
        ancestorOf.Clear()
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
            |> Seq.map (fun (loc : codeLocation) -> {loc = Instruction(loc.offset, loc.method); lvl = infty; pc = Memory.EmptyState.pc})
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
            let pob = {loc = Instruction(loc.offset, loc.method); lvl = infty; pc = Memory.EmptyState.pc}
            let result =
                match answeredPobs.[pob] with
                | Witnessed _ -> "Witnessed"
                | status -> status.ToString()
            acc.Add(loc, result)
            acc
        let result = codeLocations |> Seq.fold addLocationStatus (Dictionary<codeLocation, string>())
        k result

