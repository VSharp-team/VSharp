namespace VSharp.Interpreter.IL

open System
open System.Diagnostics
open System.Reflection
open System.Collections.Generic
open FSharpx.Collections

open VSharp
open VSharp.Concolic
open VSharp.Core
open CilStateOperations
open VSharp.Interpreter.IL
open VSharp.Solver

type public SILI(options : SiliOptions) =

    let stopwatch = Stopwatch()
    let () = stopwatch.Start()
    let timeout = if options.timeout <= 0 then Int64.MaxValue else int64 options.timeout * 1000L
    let branchReleaseTimeout = if options.timeout <= 0 then Int64.MaxValue else timeout * 80L / 100L
    let mutable branchesReleased = false

    let statistics = SILIStatistics()
    let infty = UInt32.MaxValue
    let emptyState = Memory.EmptyState()
    let isConcolicMode =
        match options.executionMode with
        | ConcolicMode -> true
        | SymbolicMode -> false
    let interpreter = ILInterpreter(isConcolicMode)

    let mutable entryIP : ip = Unchecked.defaultof<ip>
    let mutable reportFinished : cilState -> unit = fun _ -> internalfail "reporter not configured!"
    let mutable reportError : cilState -> unit = fun _ -> internalfail "reporter not configured!"
    let mutable reportIncomplete : cilState -> unit = fun _ -> internalfail "reporter not configured!"
    let mutable reportInternalFail : Exception -> unit = fun _ -> internalfail "reporter not configured!"
    let mutable concolicMachines : Dictionary<cilState, ClientMachine> = Dictionary<cilState, ClientMachine>()

    let () =
        if options.visualize then
            DotVisualizer options.outputDirectory :> IVisualizer |> Application.setVisualizer

    let isSat pc =
        // TODO: consider trivial cases
        emptyState.pc <- pc
        match SolverInteraction.checkSat emptyState with
        | SolverInteraction.SmtSat _
        | SolverInteraction.SmtUnknown _ -> true
        | _ -> false

    let rec mkForwardSearcher coverageZone = function
        | BFSMode -> BFSSearcher(infty) :> IForwardSearcher
        | DFSMode -> DFSSearcher(infty) :> IForwardSearcher
        | ShortestDistanceBasedMode -> ShortestDistanceBasedSearcher(infty, statistics)
        | GuidedMode baseMode ->
            let baseSearcher = mkForwardSearcher coverageZone baseMode
            GuidedSearcher(infty, options.recThreshold, baseSearcher, StatisticsTargetCalculator(statistics, coverageZone), coverageZone) :> IForwardSearcher

    let mutable searcher : IBidirectionalSearcher =
        match options.explorationMode with
        | TestCoverageMode(coverageZone, searchMode) ->
            BidirectionalSearcher(mkForwardSearcher coverageZone searchMode, BackwardSearcher(), DummyTargetedSearcher.DummyTargetedSearcher()) :> IBidirectionalSearcher
        | StackTraceReproductionMode _ -> __notImplemented__()

    let releaseBranches() =
        if not branchesReleased then
            branchesReleased <- true
            let dfsSearcher = DFSSearcher(infty) :> IForwardSearcher
            let bidirectionalSearcher = OnlyForwardSearcher(dfsSearcher)
            dfsSearcher.Init <| searcher.States()
            searcher <- bidirectionalSearcher


    let coveragePobsForMethod (method : Method) =
        let cfg = method.CFG
        cfg.SortedOffsets |> Seq.map (fun offset ->
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

    let wrapOnTest (action : Action<UnitTest>) (method : Method) cmdArgs (state : cilState) =
        Logger.info "Result of method %s is %O" method.FullName state.Result
        Application.terminateState state
        reportState action.Invoke false method cmdArgs state

    let wrapOnError (action : Action<UnitTest>) method cmdArgs state =
        Application.terminateState state
        reportState action.Invoke true method cmdArgs state

    let wrapOnIIE (action : Action<InsufficientInformationException>) (state : cilState) =
        statistics.IncompleteStates.Add(state)
        Application.terminateState state
        action.Invoke state.iie.Value

    let wrapOnInternalFail (action : Action<Exception>) (e : Exception) =
        statistics.InternalFails.Add(e)
        action.Invoke e

    static member private FormInitialStateWithoutStatics (method : Method) =
        let initialState = Memory.EmptyState()
        initialState.model <- Some (Memory.EmptyModel method)
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

    member private x.FormInitialStates (method : Method) : cilState list =
        let cilState = SILI.FormInitialStateWithoutStatics method
        match options.executionMode with
        | ConcolicMode -> List.singleton cilState
        | SymbolicMode -> interpreter.InitializeStatics cilState method.DeclaringType List.singleton

    member private x.Forward (s : cilState) =
        let loc = s.currentLoc
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
        Application.moveState loc s (Seq.cast<_> newStates)
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
                Application.addGoal p.loc
                searcher.UpdatePobs p' p
            | false ->
                Logger.trace "UNSAT for pob = %O and s'.PC = %s" p' (API.Print.PrintPC s'.state.pc)

    member private x.BidirectionalSymbolicExecution (EP : ip) =
        let mutable action = Stop
        let pick() =
            match searcher.Pick() with
            | Stop -> false
            | a -> action <- a; true
        (* TODO: checking for timeout here is not fine-grained enough (that is, we can work significantly beyond the
                 timeout, but we'll live with it for now. *)
        while pick() && stopwatch.ElapsedMilliseconds < timeout do
            if stopwatch.ElapsedMilliseconds >= branchReleaseTimeout then
                releaseBranches()
            match action with
            | GoFront s -> x.Forward(s)
            | GoBack(s, p) -> x.Backward p s EP
            | Stop -> __unreachable__()

    member private x.AnswerPobs entryPoint initialStates =
        statistics.ExplorationStarted()
        branchesReleased <- false
        let mainPobs = coveragePobsForMethod entryPoint |> Seq.filter (fun pob -> pob.loc.offset <> 0<offsets>)
        Application.spawnStates (Seq.cast<_> initialStates)
        mainPobs |> Seq.map (fun pob -> pob.loc) |> Seq.toArray |> Application.addGoals
        AssemblyManager.reset()
        entryPoint.Module.Assembly |> AssemblyManager.load 1
        searcher.Init entryPoint initialStates mainPobs
        entryIP <- Instruction(0<offsets>, entryPoint)
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
        let method = Application.getMethod method
        reportFinished <- wrapOnTest onFinished method optionArgs
        reportError <- wrapOnError onException method optionArgs
        reportIncomplete <- wrapOnIIE onIIE
        reportInternalFail <- wrapOnInternalFail onInternalFail
        interpreter.ConfigureErrorReporter reportError
        let state = Memory.EmptyState()
        state.model <- Some (Memory.EmptyModel method)
        let argsToState args =
            let argTerms = Seq.map (fun str -> Memory.AllocateString str state) args
            let stringType = Types.FromDotNetType typeof<string>
            let argsNumber = MakeNumber mainArguments.Length
            Memory.AllocateConcreteVectorArray state argsNumber stringType argTerms
        let arguments = Option.map (argsToState >> List.singleton) optionArgs
        ILInterpreter.InitFunctionFrame state method None arguments
        if Option.isNone optionArgs then
            // NOTE: if args are symbolic, constraint 'args != null' is added
            let parameters = method.Parameters
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
        let method = Application.getMethod method
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
