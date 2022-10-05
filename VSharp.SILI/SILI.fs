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
    let timeout = if options.timeout <= 0 then Int64.MaxValue else int64 options.timeout * 1000L
    let branchReleaseTimeout = if options.timeout <= 0 || not options.releaseBranches then Int64.MaxValue else timeout * 80L / 100L
    let mutable branchesReleased = false
    let mutable isStopped = false

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
    let mutable reportError : cilState -> string -> unit = fun _ -> internalfail "reporter not configured!"
    let reportUnspecifiedError state = reportError state "Unspecified"
    let mutable reportIncomplete : cilState -> unit = fun _ -> internalfail "reporter not configured!"
    let mutable reportInternalFail : Exception -> unit = fun _ -> internalfail "reporter not configured!"
    let mutable concolicMachines : Dictionary<cilState, ClientMachine> = Dictionary<cilState, ClientMachine>()

    let () =
        if options.visualize then
            DotVisualizer options.outputDirectory :> IVisualizer |> Application.setVisualizer
        SetMaxBuferSize options.maxBufferSize
        TestGenerator.setMaxBufferSize options.maxBufferSize

    let inCoverageZone coverageZone (startingMethod : Method) =
        match coverageZone with
        | MethodZone -> (=) startingMethod
        | ClassZone -> fun method -> method.DeclaringType.TypeHandle = startingMethod.DeclaringType.TypeHandle
        | ModuleZone -> fun method -> method.Module.ModuleHandle = startingMethod.Module.ModuleHandle

    let isSat pc =
        // TODO: consider trivial cases
        emptyState.pc <- pc
        match SolverInteraction.checkSat emptyState with
        | SolverInteraction.SmtSat _
        | SolverInteraction.SmtUnknown _ -> true
        | _ -> false

    let rec mkForwardSearcher = function
        | BFSMode -> BFSSearcher(infty) :> IForwardSearcher
        | DFSMode -> DFSSearcher(infty) :> IForwardSearcher
        | ShortestDistanceBasedMode -> ShortestDistanceBasedSearcher(infty, statistics)
        | ContributedCoverageMode -> DFSSortedByContributedCoverageSearcher(infty, statistics)
        | InterleavedMode(base1, stepCount1, base2, stepCount2) ->
            InterleavedSearcher([mkForwardSearcher base1, stepCount1; mkForwardSearcher base2, stepCount2])
        | GuidedMode baseMode ->
            let baseSearcher = mkForwardSearcher baseMode
            GuidedSearcher(infty, options.recThreshold, baseSearcher, StatisticsTargetCalculator(statistics)) :> IForwardSearcher

    let mutable searcher : IBidirectionalSearcher =
        match options.explorationMode with
        | TestCoverageMode(_, searchMode) ->
            BidirectionalSearcher(mkForwardSearcher searchMode, BackwardSearcher(), DummyTargetedSearcher.DummyTargetedSearcher()) :> IBidirectionalSearcher
        | StackTraceReproductionMode _ -> __notImplemented__()

    let releaseBranches() =
        if not branchesReleased then
            branchesReleased <- true
            ReleaseBranches()
            let dfsSearcher = DFSSortedByContributedCoverageSearcher(infty, statistics) :> IForwardSearcher
            let bidirectionalSearcher = OnlyForwardSearcher(dfsSearcher)
            dfsSearcher.Init <| searcher.States()
            searcher <- bidirectionalSearcher

    let coveragePobsForMethod (method : Method) =
        let cfg = method.CFG
        cfg.SortedOffsets |> Seq.map (fun offset ->
            {loc = {offset = offset; method = method}; lvl = infty; pc = EmptyPathCondition})
        |> List.ofSeq

    let reportState reporter isError (method : Method) cmdArgs cilState message =
        try
            if isError || cilState.history |> Seq.exists (not << CodeLocation.isBasicBlockCoveredByTest)
            then
                let hasException =
                    match cilState.state.exceptionsRegister with
                    | Unhandled _ -> true
                    | _ -> false
                if not isError || hasException
                then statistics.TrackFinished cilState
                let callStackSize = Memory.CallStackSize cilState.state
                let methodHasByRefParameter (m : Method) = m.Parameters |> Seq.exists (fun pi -> pi.ParameterType.IsByRef)
                if isError && not hasException
                    then
                        if method.DeclaringType.IsValueType || methodHasByRefParameter method
                        then Memory.ForcePopFrames (callStackSize - 2) cilState.state
                        else Memory.ForcePopFrames (callStackSize - 1) cilState.state
                match TestGenerator.state2test isError method cmdArgs cilState message with
                | Some test -> reporter test
                | None -> ()
        with :? InsufficientInformationException as e ->
            cilState.iie <- Some e
            reportIncomplete cilState

    let wrapOnTest (action : Action<UnitTest>) (method : Method) cmdArgs (state : cilState) =
        Logger.info "Result of method %s is %O" method.FullName state.Result
        Application.terminateState state
        reportState action.Invoke false method cmdArgs state null

    let wrapOnError (action : Action<UnitTest>) (method : Method) cmdArgs (state : cilState) errorMessage =
        let message = sprintf "%s error of method %s" errorMessage method.FullName
        Logger.info "%s" message
        Application.terminateState state
        reportState action.Invoke true method cmdArgs state message

    let wrapOnIIE (action : Action<InsufficientInformationException>) (state : cilState) =
        statistics.IncompleteStates.Add(state)
        Application.terminateState state
        action.Invoke state.iie.Value

    let wrapOnInternalFail (action : Action<Exception>) (e : Exception) =
        statistics.InternalFails.Add(e)
        action.Invoke e

    static member private FormInitialStateWithoutStatics (method : Method) =
        let initialState = Memory.EmptyState()
        initialState.model <- Memory.EmptyModel method
        let cilState = makeInitialState method initialState
        try
            let this(*, isMethodOfStruct*) =
                if method.IsStatic then None // *TODO: use hasThis flag from Reflection
                else
                    let this =
                        if Types.IsValueType method.DeclaringType then
                            Memory.NewStackFrame initialState None []
                            Memory.AllocateTemporaryLocalVariableOfType initialState "this" 0 method.DeclaringType
                        else
                            Memory.MakeSymbolicThis method
                    !!(IsNullReference this) |> AddConstraint initialState
                    Some this
            ILInterpreter.InitFunctionFrame initialState method this None
        with :? InsufficientInformationException as e ->
            cilState.iie <- Some e
        cilState

    static member private TrySubstituteTypeParameters (method : MethodBase) =
        let vsMethod = Application.getMethod method
        let getConcreteType = function
        | ConcreteType t -> Some t
        | _ -> None
        try
            match SolveGenericMethodParameters vsMethod with
            | Some(classParams, methodParams) ->
                let classParams = classParams |> Array.choose getConcreteType
                let methodParams = methodParams |> Array.choose getConcreteType
                if classParams.Length = method.DeclaringType.GetGenericArguments().Length &&
                    (method.IsConstructor || methodParams.Length = method.GetGenericArguments().Length) then
                    let declaringType = Reflection.concretizeTypeParameters method.DeclaringType classParams
                    let method = Reflection.concretizeMethodParameters declaringType method methodParams
                    Some method
                else
                    None
            | _ -> None
        with :? InsufficientInformationException -> None

    member private x.FormInitialStates (method : Method) : cilState list =
        let cilState = SILI.FormInitialStateWithoutStatics method
        let cilStates = ILInterpreter.CheckDisallowNullAssumptions cilState method false
        assert (List.length cilStates = 1)
        let [cilState] = cilStates
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
        let runtimeExceptions, userExceptions = toReportExceptions |> List.partition hasRuntimeException
        runtimeExceptions |> List.iter reportUnspecifiedError
        userExceptions |> List.iter reportFinished
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
        statistics.TrackFork s newStates
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
        while not isStopped && pick() && stopwatch.ElapsedMilliseconds < timeout do
            if stopwatch.ElapsedMilliseconds >= branchReleaseTimeout then
                releaseBranches()
            match action with
            | GoFront s -> x.Forward(s)
            | GoBack(s, p) -> x.Backward p s EP
            | Stop -> __unreachable__()

    member private x.AnswerPobs entryPoint initialStates =
        match options.explorationMode with
        | TestCoverageMode(coverageZone, _) ->
            Application.setCoverageZone (inCoverageZone coverageZone entryPoint)
        | StackTraceReproductionMode _ -> __notImplemented__()
        Application.setAttributesZone (fun _ -> options.checkAttributes)
        Application.resetMethodStatistics()
        statistics.ExplorationStarted()
        isStopped <- false
        branchesReleased <- false
        AcquireBranches()
        searcher.Reset()
        let mainPobs = coveragePobsForMethod entryPoint |> Seq.filter (fun pob -> pob.loc.offset <> 0<offsets>)
        Application.spawnStates (Seq.cast<_> initialStates)
        mainPobs |> Seq.map (fun pob -> pob.loc) |> Seq.toArray |> Application.addGoals
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

    member private x.InterpretEntryPointInternal (method : MethodBase) (mainArguments : string[]) (onFinished : Action<UnitTest>)
                                         (onException : Action<UnitTest>) (onIIE : Action<InsufficientInformationException>)
                                         (onInternalFail : Action<Exception>) : unit =
        assert method.IsStatic
        stopwatch.Restart()
        Reset()
        SolverPool.reset()
        let optionArgs = if mainArguments = null then None else Some mainArguments
        let method = Application.getMethod method
        reportFinished <- wrapOnTest onFinished method optionArgs
        reportError <- wrapOnError onException method optionArgs
        reportIncomplete <- wrapOnIIE onIIE
        interpreter.ConfigureErrorReporter reportError
        let state = Memory.EmptyState()
        state.model <- Memory.EmptyModel method
        let argsToState args =
            let argTerms = Seq.map (fun str -> Memory.AllocateString str state) args
            let stringType = typeof<string>
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
        Memory.InitializeStaticMembers state method.DeclaringType
        let initialState = makeInitialState method state
        x.AnswerPobs method [initialState]
        Restore()

    member private x.InterpretIsolatedInternal (method : MethodBase) (onFinished : Action<UnitTest>)
                                               (onException : Action<UnitTest>) (onIIE : Action<InsufficientInformationException>)
                                               (onInternalFail : Action<Exception>) : unit =
        stopwatch.Restart()
        Reset()
        SolverPool.reset()
        let method = Application.getMethod method
        reportFinished <- wrapOnTest onFinished method None
        reportError <- wrapOnError onException method None
        reportIncomplete <- wrapOnIIE onIIE
        interpreter.ConfigureErrorReporter reportError
        let initialStates = x.FormInitialStates method
        let iieStates, initialStates = initialStates |> List.partition (fun state -> state.iie.IsSome)
        iieStates |> List.iter reportIncomplete
        if not initialStates.IsEmpty then
            x.AnswerPobs method initialStates
        Restore()

    member x.InterpretEntryPoint (method : MethodBase) (mainArguments : string[]) (onFinished : Action<UnitTest>)
                                 (onException : Action<UnitTest>) (onIIE : Action<InsufficientInformationException>)
                                 (onInternalFail : Action<Exception>) : unit =
        reportInternalFail <- wrapOnInternalFail onInternalFail
        try
            try
                // TODO: resolve type parameters by mainArguments?
                match SILI.TrySubstituteTypeParameters method with
                | Some newMethod -> x.InterpretEntryPointInternal newMethod mainArguments onFinished onException onIIE onInternalFail
                | None -> x.InterpretEntryPointInternal method mainArguments onFinished onException onIIE onInternalFail
            with
            | e -> reportInternalFail e
        finally
            searcher.Reset()

    member x.InterpretIsolated (method : MethodBase) (onFinished : Action<UnitTest>)
                               (onException : Action<UnitTest>) (onIIE : Action<InsufficientInformationException>)
                               (onInternalFail : Action<Exception>) : unit =
        reportInternalFail <- wrapOnInternalFail onInternalFail
        try
            try
                match SILI.TrySubstituteTypeParameters method with
                | Some newMethod -> x.InterpretIsolatedInternal newMethod onFinished onException onIIE onInternalFail
                | None -> x.InterpretIsolatedInternal method onFinished onException onIIE onInternalFail
            with
            | e -> reportInternalFail e
        finally
            searcher.Reset()

    member x.Stop() = isStopped <- true

    member x.Statistics with get() = statistics
