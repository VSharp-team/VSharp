namespace VSharp.Interpreter.IL

open System
open System.Reflection
open System.Collections.Generic
open FSharpx.Collections

open VSharp
open VSharp.Concolic
open VSharp.Core
open CilStateOperations
open VSharp.Solver

type public SILI(options : SiliOptions) =

    let statistics = SILIStatistics()
    let infty = UInt32.MaxValue
    let emptyState = Memory.EmptyState()
    let isConcolicMode =
        match options.executionMode with
        | ConcolicMode -> true
        | SymbolicMode -> false
    let interpreter = ILInterpreter(isConcolicMode)

    let mutable entryIP : ip = Exit null
    let mutable reportFinished : cilState -> unit = fun _ -> internalfail "reporter not configured!"
    let mutable reportError : cilState -> unit = fun _ -> internalfail "reporter not configured!"
    let mutable reportIncomplete : cilState -> unit = fun _ -> internalfail "reporter not configured!"
    let mutable reportInternalFail : Exception -> unit = fun _ -> internalfail "reporter not configured!"
    let mutable concolicMachines : Dictionary<cilState, ClientMachine> = Dictionary<cilState, ClientMachine>()

    let isSat pc =
        // TODO: consider trivial cases
        emptyState.pc <- pc
        match SolverInteraction.checkSat emptyState with
        | SolverInteraction.SmtSat _
        | SolverInteraction.SmtUnknown _ -> true
        | _ -> false
    let mkForwardSearcher coverageZone = function
        | BFSMode -> BFSSearcher(infty) :> IForwardSearcher
        | DFSMode -> DFSSearcher(infty) :> IForwardSearcher
        | GuidedMode ->
            let baseSearcher = BFSSearcher(infty) :> IForwardSearcher
            GuidedSearcher(infty, options.recThreshold, baseSearcher, StatisticsTargetCalculator(statistics, coverageZone), coverageZone) :> IForwardSearcher

    let searcher : IBidirectionalSearcher =
        match options.explorationMode with
        | TestCoverageMode(coverageZone, searchMode) ->
            BidirectionalSearcher(mkForwardSearcher coverageZone searchMode, BackwardSearcher(), DummyTargetedSearcher.DummyTargetedSearcher()) :> IBidirectionalSearcher
        | StackTraceReproductionMode _ -> __notImplemented__()

    let coveragePobsForMethod (method : MethodBase) =
        let cfg = CFG.findCfg method
        cfg.sortedOffsets |> Seq.map (fun offset ->
            {loc = {offset = offset; method = method}; lvl = infty; pc = EmptyPathCondition})
        |> List.ofSeq

    let reportState reporter isError method cmdArgs state =
        try
            match TestGenerator.state2test isError method cmdArgs state with
            | Some test -> reporter test
            | None -> ()
        with :? InsufficientInformationException as e ->
            state.iie <- Some e
            reportIncomplete state

    let wrapOnTest (action : Action<UnitTest>) (method : MethodBase) cmdArgs (state : cilState) =
        Logger.info "Result of method %s is %O" method.Name state.Result
        reportState action.Invoke false method cmdArgs state

    let wrapOnError (action : Action<UnitTest>) method cmdArgs state =
        reportState action.Invoke true method cmdArgs state

    let wrapOnIIE (action : Action<InsufficientInformationException>) (state : cilState) =
        statistics.IncompleteStates.Add(state)
        action.Invoke state.iie.Value

    let wrapOnInternalFail (action : Action<Exception>) (e : Exception) =
        statistics.InternalFails.Add(e)
        action.Invoke e

    static member private FormInitialStateWithoutStatics (method : MethodBase) =
        let initialState = Memory.EmptyState()
        let cilState = makeInitialState method initialState
        try
            let this(*, isMethodOfStruct*) =
                if method.IsStatic then None // *TODO: use hasThis flag from Reflection
                else
                    let this = Memory.MakeSymbolicThis method
                    !!(IsNullReference this) |> AddConstraint initialState
                    Some this
            ILInterpreter.InitFunctionFrame initialState method this None
        with :? InsufficientInformationException as e ->
            cilState.iie <- Some e
        cilState

    member private x.FormInitialStates (method : MethodBase) : cilState list =
        let cilState = SILI.FormInitialStateWithoutStatics method
        match options.executionMode with
        | ConcolicMode -> List.singleton cilState
        | SymbolicMode -> interpreter.InitializeStatics cilState method.DeclaringType List.singleton

    member private x.Forward (s : cilState) =
        // TODO: update pobs when visiting new methods; use coverageZone
        statistics.TrackStepForward s
        let goodStates, iieStates, errors = interpreter.ExecuteOneInstruction s
        let goodStates, toReportFinished = goodStates |> List.partition (fun s -> isExecutable s || s.startingIP <> entryIP)
        toReportFinished |> List.iter reportFinished
        let errors, toReportExceptions = errors |> List.partition (fun s -> s.startingIP <> entryIP || not <| stoppedByException s)
        toReportExceptions |> List.iter reportFinished
        let iieStates, toReportIIE = iieStates |> List.partition (fun s -> s.startingIP <> entryIP)
        toReportIIE |> List.iter reportIncomplete
        let newStates =
            match goodStates with
            | s'::goodStates when LanguagePrimitives.PhysicalEquality s s' -> goodStates @ iieStates @ errors
            | _ ->
                match iieStates with
                | s'::iieStates when LanguagePrimitives.PhysicalEquality s s' -> goodStates @ iieStates @ errors
                | _ ->
                    match errors with
                    | s'::errors when LanguagePrimitives.PhysicalEquality s s' -> goodStates @ iieStates @ errors
                    | _ -> goodStates @ iieStates @ errors
        let concolicMachine : ClientMachine ref = ref null
        if concolicMachines.TryGetValue(s, concolicMachine) then
            let machine = concolicMachine.Value
            let cilState' = machine.StepDone (s::newStates)
            if not <| LanguagePrimitives.PhysicalEquality s cilState' then
                concolicMachines.Remove(s) |> ignore
                concolicMachines.Add(cilState', machine)
        searcher.UpdateStates s newStates

    member private x.Backward p' s' EP =
        assert(currentLoc s' = p'.loc)
        let sLvl = levelToUnsignedInt s'.level
        if p'.lvl >= sLvl then
            let lvl = p'.lvl - sLvl
            let pc = Memory.WLP s'.state p'.pc
            match isSat pc with
            | true when s'.startingIP = EP -> searcher.Answer p' (Witnessed s')
            | true ->
                statistics.TrackStepBackward p' s'
                let p = {loc = startingLoc s'; lvl = lvl; pc = pc}
                Logger.trace "Backward:\nWas: %O\nNow: %O\n\n" p' p
                searcher.UpdatePobs p' p
            | false ->
                Logger.trace "UNSAT for pob = %O and s'.PC = %s" p' (API.Print.PrintPC s'.state.pc)

    member private x.BidirectionalSymbolicExecution (EP : ip) =
        let mutable action = Stop
        let pick() =
            match searcher.Pick() with
            | Stop -> false
            | a -> action <- a; true
        while pick() do
            match action with
            | GoFront s -> x.Forward(s)
            | GoBack(s, p) -> x.Backward p s EP
            | Stop -> __unreachable__()

    member private x.AnswerPobs entryPoint initialStates =
        statistics.ExplorationStarted()
        let mainPobs = coveragePobsForMethod entryPoint |> Seq.filter (fun pob -> pob.loc.offset <> 0)
        AssemblyManager.reset()
        entryPoint.Module.Assembly |> AssemblyManager.load 1
        searcher.Init entryPoint initialStates mainPobs
        entryIP <- Instruction(0x0, entryPoint)
        match options.executionMode with
        | ConcolicMode ->
            initialStates |> List.iter (fun initialState ->
                let machine = ClientMachine(entryPoint, (fun _ -> ()), initialState)
                if not <| machine.Spawn() then
                    internalfail "Unable to spawn concolic machine!"
                concolicMachines.Add(initialState, machine))
            let machine =
                if concolicMachines.Count = 1 then Seq.head concolicMachines.Values
                else __notImplemented'__ "Forking in concolic mode"
            while machine.State.suspended && machine.ExecCommand() do // TODO: make better interaction between concolic and SILI #do
                x.BidirectionalSymbolicExecution entryIP
            // TODO: need to report? #do
//            Logger.error "result state = %O" machine.State
//            reportFinished.Invoke machine.State
        | SymbolicMode ->
            x.BidirectionalSymbolicExecution entryIP
        searcher.Statuses() |> Seq.iter (fun (pob, status) ->
            match status with
            | pobStatus.Unknown ->
                Logger.warning "Unknown status for pob at %O" pob.loc
            | _ -> ())

    member x.InterpretEntryPoint (method : MethodBase) (mainArguments : string[]) (onFinished : Action<UnitTest>)
                                 (onException : Action<UnitTest>) (onIIE : Action<InsufficientInformationException>)
                                 (onInternalFail : Action<Exception>) : unit =
        assert method.IsStatic
        let optionArgs = if mainArguments = null then None else Some mainArguments
        reportFinished <- wrapOnTest onFinished method optionArgs
        reportError <- wrapOnError onException method optionArgs
        reportIncomplete <- wrapOnIIE onIIE
        reportInternalFail <- wrapOnInternalFail onInternalFail
        interpreter.ConfigureErrorReporter reportError
        let state = Memory.EmptyState()
        let argsToState args =
            let argTerms = Seq.map (fun str -> Memory.AllocateString str state) args
            let stringType = Types.FromDotNetType typeof<string>
            let argsNumber = MakeNumber mainArguments.Length
            Memory.AllocateConcreteVectorArray state argsNumber stringType argTerms
        let arguments = Option.map (argsToState >> List.singleton) optionArgs
        ILInterpreter.InitFunctionFrame state method None arguments
        if Option.isNone optionArgs then
            // NOTE: if args are symbolic, constraint 'args != null' is added
            let parameters = method.GetParameters()
            assert(Array.length parameters = 1)
            let argsParameter = Array.head parameters
            let argsParameterTerm = Memory.ReadArgument state argsParameter
            AddConstraint state (!!(IsNullReference argsParameterTerm))
        Memory.InitializeStaticMembers state (Types.FromDotNetType method.DeclaringType)
        let initialState = makeInitialState method state
        x.AnswerPobs method [initialState]

    member x.InterpretIsolated (method : MethodBase) (onFinished : Action<UnitTest>)
                               (onException : Action<UnitTest>) (onIIE : Action<InsufficientInformationException>)
                               (onInternalFail : Action<Exception>) : unit =
        Reset()
        SolverPool.reset()
        reportFinished <- wrapOnTest onFinished method None
        reportError <- wrapOnError onException method None
        reportIncomplete <- wrapOnIIE onIIE
        reportInternalFail <- wrapOnInternalFail onInternalFail
        interpreter.ConfigureErrorReporter reportError
        let initialStates = x.FormInitialStates method
        let iieStates, initialStates = initialStates |> List.partition (fun state -> state.iie.IsSome)
        iieStates |> List.iter reportIncomplete
        if not initialStates.IsEmpty then
            x.AnswerPobs method initialStates
        Restore()

    member x.Statistics with get() = statistics
