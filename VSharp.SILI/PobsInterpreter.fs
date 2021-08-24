namespace VSharp.Interpreter.IL

open System
open System.Collections.Generic
open System.Text
open FSharpx.Collections
open System.Reflection

open VSharp
open VSharp.Core
open VSharp.Utils
open ipOperations
open CilStateOperations





type public PobsInterpreter(searcher : IBidirectionalSearcher) as this =
    inherit ExplorerBase()
    let bidirectionalEngineStatistics = BidirectionalEngineStatistics()
    let infty = UInt32.MaxValue
    let emptyState = Memory.EmptyState()
    let interpreter = ILInterpreter(this)
    //let qFront : FrontQueue = FrontQueue(maxBound, searcher)





//    let existAnyMainPob() = mainPobs.Count > 0

//    let createPobs () =
//        Seq.iter (fun (mp : pob) ->
//            let p = {mp with lvl = curLvl}
//            let l = List<pob>()
//            l.Add(mp)
//            ancestorOf.Add(p, l)
//            doAddPob p) mainPobs





    let isSat pc =
        emptyState.pc <- pc
        match SolverInteraction.isValid emptyState with
        | SolverInteraction.SmtSat _
        | SolverInteraction.SmtUnknown _ -> true
        | _ -> false

    member x.MakeCilStateForIp (ip : ip) =
        let m = methodOf ip
        let cilStates = x.FormInitialState m
        cilStates |> List.map (fun s -> {s with startingIP = ip} |> setIpStack [ip])

//    NOTE: Must be called for ip with empty evaluation stack!
//    member x.Start (ip : ip) =
//        try
//            let starts = x.MakeCilStateForIp ip
//            searcher.UpdateStates None starts
//        with
//        | :? InsufficientInformationException -> Logger.info "Could not START from %O" ip

    member x.Forward (s : cilState) =
        let goodStates, iieStates, errors = interpreter.ExecuteOneInstruction s
        let newStates =
            match goodStates with
            | s'::goodStates when LanguagePrimitives.PhysicalEquality s s' -> goodStates @ iieStates @ errors
            | _ ->
                match iieStates with
                | s'::iieStates when LanguagePrimitives.PhysicalEquality s s' -> goodStates @ iieStates @ errors
                | _ ->
                    match errors with
                    | s'::errors when LanguagePrimitives.PhysicalEquality s s' -> goodStates @ iieStates @ errors
                    | _ -> __unreachable__()
        searcher.UpdateStates s newStates // update will remove s

    member x.Backward p' s' EP =
        assert(currentLoc s' = p'.loc)
        let sLvl = levelToUnsignedInt s'.level
        if p'.lvl >= sLvl then
            let lvl = p'.lvl - sLvl
            let pc = Memory.WLP s'.state p'.pc
            match isSat pc with
            | true when s'.startingIP = EP -> searcher.Answer p' (Witnessed s')
            | true ->
                let p = {loc = startingLoc s'; lvl = lvl; pc = pc}
                Logger.warning "Backward:\nWas: %O\nNow: %O\n\n" p' p
                searcher.UpdatePobs p' p
            | false ->
                Logger.warning "UNSAT for pob = %O and s'.PC = %s" p' (API.Print.PrintPC s'.state.pc)

    member x.BidirectionalSymbolicExecution (*(mainMethod : MethodBase)*) (EP : ip) mainPobsList =
//        Seq.iter mainPobs.Add mainPobsList

        let mutable canWork = true
        while searcher.ShouldWork() && canWork do
            let action = searcher.Pick()
            bidirectionalEngineStatistics.RememberSearcherAction action
            match action with
            | GoFront s -> x.Forward(s)
            | GoBack(s, p) -> x.Backward p s EP
            | Stop -> canWork <- false

//        while searcher.is do
//            break
//            createPobs()
//            while currentPobs.Count > 0 && existAnyMainPob() do
//                let action = searcher.ChooseAction()
//                bidirectionalEngineStatistics.RememberSearcherAction action
//                match action with
//                | Stop -> currentPobs.Clear()
//                | Start loc -> x.Start(loc)
//                | GoForward s ->
//                    Logger.info "GoForward: ip = %O" (currentIp s)
//                    x.Forward(s)
//                | GoBackward(p', s') -> x.Backward(p',s', EP)
//            curLvl <- curLvl + 1u
//        Seq.iter bidirectionalEngineStatistics.AddUnansweredPob mainPobs
        Logger.warning "BidirectionalSymbolicExecution Statistics:\n%s" (bidirectionalEngineStatistics.PrintStatistics(searcher.GetType().ToString()))

    member x.ClearStructures () =
//        curLvl <- maxBound
//        mainPobs.Clear()
//        searcher.Reset()
//        qBack.Clear()
//        currentPobs.Clear()
//        answeredPobs.Clear()
//        ancestorOf.Clear()
        bidirectionalEngineStatistics.Clear()

    override x.Invoke _ _ _ = __notImplemented__()

    override x.AnswerPobs entryMethod codeLocations k =
        let k x = searcher.Reset(); k x
        let mainPobs =
            codeLocations
            |> Seq.filter (fun loc -> loc.method.GetMethodBody().GetILAsByteArray().Length > loc.offset)
            |> Seq.map (fun (loc : codeLocation) -> {loc = loc; lvl = infty; pc = EmptyPathCondition})
        searcher.Init entryMethod mainPobs
        x.BidirectionalSymbolicExecution (Instruction(0x0, entryMethod)) mainPobs
        let addLocationStatus (d : Dictionary<codeLocation, string>) (pob : pob, status : pobStatus) =
            let result =
                match status with
                | Witnessed _ -> "Witnessed"
                | status -> status.ToString()
            d.Add(pob.loc, result)

        let result = Dictionary<codeLocation, string>()
        Seq.iter (addLocationStatus result) (searcher.Statuses())
        k result
