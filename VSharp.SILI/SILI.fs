namespace VSharp.Interpreter.IL

open System
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

    let timeout = if options.timeout <= 0 then Double.PositiveInfinity else float options.timeout * 1000.0
    let branchReleaseTimeout = if options.timeout <= 0 || not options.releaseBranches then Double.PositiveInfinity else timeout * 80.0 / 100.0

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

    let mutable reportFinished : cilState -> unit = fun _ -> internalfail "reporter not configured!"
    let mutable reportError : cilState -> string -> unit = fun _ -> internalfail "reporter not configured!"
    let mutable reportIncomplete : cilState -> unit = fun _ -> internalfail "reporter not configured!"
    let mutable reportInternalFail : cilState option -> Exception -> unit = fun _ -> internalfail "reporter not configured!"
    let mutable concolicMachines : Dictionary<cilState, ClientMachine> = Dictionary<cilState, ClientMachine>()

    let () =
        if options.visualize then
            DotVisualizer options.outputDirectory :> IVisualizer |> Application.setVisualizer
        SetMaxBuferSize options.maxBufferSize
        TestGenerator.setMaxBufferSize options.maxBufferSize

    let inCoverageZone coverageZone (entryMethods : Method list) =
        match coverageZone with
        | MethodZone -> fun method -> entryMethods |> List.contains method
        | ClassZone -> fun method -> entryMethods |> List.exists (fun m -> method.DeclaringType.TypeHandle = m.DeclaringType.TypeHandle)
        | ModuleZone -> fun method -> entryMethods |> List.exists (fun m -> method.Module.ModuleHandle = m.Module.ModuleHandle)

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
        | ShortestDistanceBasedMode isRandom -> ShortestDistanceBasedSearcher(infty, statistics, isRandom)
        | ContributedCoverageMode -> DFSSortedByContributedCoverageSearcher(infty, statistics)
        | FairMode baseMode ->
            FairSearcher((fun _ -> mkForwardSearcher baseMode), uint branchReleaseTimeout, statistics)
        | InterleavedMode(base1, stepCount1, base2, stepCount2) ->
            InterleavedSearcher([mkForwardSearcher base1, stepCount1; mkForwardSearcher base2, stepCount2])
        | GuidedMode baseMode ->
            let baseSearcher = mkForwardSearcher baseMode
            GuidedSearcher(infty, options.recThreshold, baseSearcher, StatisticsTargetCalculator(statistics)) :> IForwardSearcher
        | searchMode.ConcolicMode baseMode -> ConcolicSearcher(mkForwardSearcher baseMode)

    let mutable searcher : IBidirectionalSearcher =
        match options.explorationMode with
        | TestCoverageMode(_, searchMode) ->
            let baseSearcher = mkForwardSearcher searchMode
            let baseSearcher = if isConcolicMode then ConcolicSearcher(baseSearcher) :> IForwardSearcher else baseSearcher
            BidirectionalSearcher(baseSearcher, BackwardSearcher(), DummyTargetedSearcher.DummyTargetedSearcher()) :> IBidirectionalSearcher
        | StackTraceReproductionMode _ -> __notImplemented__()

    let releaseBranches() =
        if not branchesReleased then
            branchesReleased <- true
            ReleaseBranches()
            let dfsSearcher = DFSSortedByContributedCoverageSearcher(infty, statistics) :> IForwardSearcher
            let dfsSearcher = if isConcolicMode then ConcolicSearcher(dfsSearcher) :> IForwardSearcher else dfsSearcher
            let bidirectionalSearcher = OnlyForwardSearcher(dfsSearcher)
            dfsSearcher.Init <| searcher.States()
            searcher <- bidirectionalSearcher

    let reportState reporter isError cmdArgs cilState message =
        try
            searcher.Remove cilState
            if cilState.history |> Seq.exists (not << CodeLocation.isBasicBlockCoveredByTest)
            then
                let hasException =
                    match cilState.state.exceptionsRegister with
                    | Unhandled _ -> true
                    | _ -> false
                if not isError || hasException
                then statistics.TrackFinished cilState
                let callStackSize = Memory.CallStackSize cilState.state
                let methodHasByRefParameter (m : Method) = m.Parameters |> Seq.exists (fun pi -> pi.ParameterType.IsByRef)
                let entryMethod = entryMethodOf cilState
                if isError && not hasException
                    then
                        if entryMethod.DeclaringType.IsValueType || methodHasByRefParameter entryMethod
                        then Memory.ForcePopFrames (callStackSize - 2) cilState.state
                        else Memory.ForcePopFrames (callStackSize - 1) cilState.state
                if not isError || statistics.EmitError cilState message
                then
                    match TestGenerator.state2test isError entryMethod cmdArgs cilState message with
                    | Some test -> reporter test
                    | None -> ()
        with :? InsufficientInformationException as e ->
            cilState.iie <- Some e
            reportIncomplete cilState

    let wrapOnTest (action : Action<UnitTest>) cmdArgs (state : cilState) =
        Logger.info "Result of method %s is %O" (entryMethodOf state).FullName state.Result
        Application.terminateState state
        reportState action.Invoke false cmdArgs state null

    let wrapOnError (action : Action<UnitTest>) cmdArgs (state : cilState) errorMessage =
        if not <| String.IsNullOrWhiteSpace errorMessage then
            Logger.info "Error in %s: %s" (entryMethodOf state).FullName errorMessage
        Application.terminateState state
        reportState action.Invoke true cmdArgs state errorMessage

    let wrapOnIIE (action : Action<InsufficientInformationException>) (state : cilState) =
        statistics.IncompleteStates.Add(state)
        Application.terminateState state
        searcher.Remove state
        action.Invoke state.iie.Value

    let wrapOnInternalFail (action : Action<Method option, Exception>) (state : cilState option) (e : Exception) =
        match e with
        | :? InsufficientInformationException as e ->
            match state with
            | Some s ->
                if s.iie.IsNone then
                    s.iie <- Some e
                reportIncomplete s
            | _ -> ()
        | _ ->
            statistics.InternalFails.Add(e)
            match state with
            | Some s ->
                Application.terminateState s
                searcher.Remove s
            | _ -> ()
            action.Invoke(Option.bind (fun s -> s.entryMethod) state, e)

    static member private AllocateByRefParameters initialState (method : Method) =
        let allocateIfByRef (pi : ParameterInfo) =
            if pi.ParameterType.IsByRef then
                if Memory.CallStackSize initialState = 0 then
                    Memory.NewStackFrame initialState None []
                let stackRef = Memory.AllocateTemporaryLocalVariableOfType initialState pi.Name (pi.Position + 1) (pi.ParameterType.GetElementType())
                Some stackRef
            else
                None
        method.Parameters |> Array.map allocateIfByRef |> Array.toList

    member private x.TrySubstituteTypeParameters (method : MethodBase) =
        let getConcreteType = function
        | ConcreteType t -> Some t
        | _ -> None
        try
            let vsMethod = Application.getMethod method
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
        with
        | e ->
            reportInternalFail None e
            None

    member private x.FormIsolatedInitialStates (method : Method) =
        try
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
                let parameters = SILI.AllocateByRefParameters initialState method
                ILInterpreter.InitFunctionFrame initialState method this (Some parameters)
            with :? InsufficientInformationException as e ->
                cilState.iie <- Some e
            let cilStates = ILInterpreter.CheckDisallowNullAssumptions cilState method false
            assert (List.length cilStates = 1)
            let [cilState] = cilStates
            match options.executionMode with
            | ConcolicMode -> List.singleton cilState
            | SymbolicMode -> interpreter.InitializeStatics cilState method.DeclaringType List.singleton
        with
        | e ->
            reportInternalFail None e
            []

    member private x.FormEntryPointInitialStates (method : Method, mainArguments : string[]) : cilState list =
        try
            assert method.IsStatic
            let optionArgs = if mainArguments = null then None else Some mainArguments
            let state = Memory.EmptyState()
            state.model <- Memory.EmptyModel method
            let argsToState args =
                let argTerms = Seq.map (fun str -> Memory.AllocateString str state) args
                let stringType = typeof<string>
                let argsNumber = MakeNumber mainArguments.Length
                Memory.AllocateConcreteVectorArray state argsNumber stringType argTerms
            let arguments = Option.map (argsToState >> Some >> List.singleton) optionArgs
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
            [initialState]
        with
        | e ->
            reportInternalFail None e
            []

    member private x.Forward (s : cilState) =
        let loc = s.currentLoc
        // TODO: update pobs when visiting new methods; use coverageZone
        statistics.TrackStepForward s
        let goodStates, iieStates, errors = interpreter.ExecuteOneInstruction s
        let goodStates, toReportFinished = goodStates |> List.partition (fun s -> isExecutable s || isIsolated s)
        toReportFinished |> List.iter reportFinished
        let errors, toReportExceptions = errors |> List.partition (fun s -> isIsolated s || not <| stoppedByException s)
        let runtimeExceptions, userExceptions = toReportExceptions |> List.partition hasRuntimeException
        runtimeExceptions |> List.iter (fun state -> reportError state null)
        userExceptions |> List.iter reportFinished
        let iieStates, toReportIIE = iieStates |> List.partition isIsolated
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

    member private x.Backward p' s' =
        assert(currentLoc s' = p'.loc)
        let sLvl = levelToUnsignedInt s'.level
        if p'.lvl >= sLvl then
            let lvl = p'.lvl - sLvl
            let pc = Memory.WLP s'.state p'.pc
            match isSat pc with
            | true when not <| isIsolated s' -> searcher.Answer p' (Witnessed s')
            | true ->
                statistics.TrackStepBackward p' s'
                let p = {loc = startingLoc s'; lvl = lvl; pc = pc}
                Logger.trace "Backward:\nWas: %O\nNow: %O\n\n" p' p
                Application.addGoal p.loc
                searcher.UpdatePobs p' p
            | false ->
                Logger.trace "UNSAT for pob = %O and s'.PC = %s" p' (API.Print.PrintPC s'.state.pc)

    member private x.BidirectionalSymbolicExecution() =
        let mutable action = Stop
        let pick() =
            match searcher.Pick() with
            | Stop -> false
            | a -> action <- a; true
        (* TODO: checking for timeout here is not fine-grained enough (that is, we can work significantly beyond the
                 timeout, but we'll live with it for now. *)
        while not isStopped && pick() && statistics.CurrentExplorationTime.TotalMilliseconds < timeout do
            if statistics.CurrentExplorationTime.TotalMilliseconds >= branchReleaseTimeout then
                releaseBranches()
            match action with
            | GoFront s ->
                try
                    x.Forward(s)
                with
                | e -> reportInternalFail (Some s) e
            | GoBack(s, p) ->
                try
                    x.Backward p s
                with
                | e -> reportInternalFail (Some s) e
            | Stop -> __unreachable__()

    member private x.AnswerPobs initialStates =
        statistics.ExplorationStarted()

        // For backward compatibility. TODO: remove main pobs at all
        let mainPobs = []
(*            initialStates
            |> Seq.map entryMethodOf
            |> Seq.collect coveragePobsForMethod
            |> Seq.filter (fun pob -> pob.loc.offset <> 0<offsets>)*)
        Application.spawnStates (Seq.cast<_> initialStates)
        mainPobs |> Seq.map (fun pob -> pob.loc) |> Seq.toArray |> Application.addGoals
        searcher.Init initialStates mainPobs
        initialStates |> Seq.filter isIIEState |> Seq.iter reportIncomplete
        match options.executionMode with
        | ConcolicMode ->
            initialStates |> List.iter (fun initialState ->
                let machine = ClientMachine(entryMethodOf initialState, (fun _ -> ()), initialState)
                if not <| machine.Spawn() then
                    internalfail "Unable to spawn concolic machine!"
                concolicMachines.Add(initialState, machine))
            let machine =
                if concolicMachines.Count = 1 then Seq.head concolicMachines.Values
                else __notImplemented'__ "Forking in concolic mode"
            while machine.State.suspended && machine.ExecCommand() do // TODO: make better interaction between concolic and SILI #do
                x.BidirectionalSymbolicExecution()
            // TODO: need to report? #do
//            Logger.error "result state = %O" machine.State
//            reportFinished.Invoke machine.State
        | SymbolicMode ->
            x.BidirectionalSymbolicExecution()
        searcher.Statuses() |> Seq.iter (fun (pob, status) ->
            match status with
            | pobStatus.Unknown ->
                Logger.warning "Unknown status for pob at %O" pob.loc
            | _ -> ())

    member x.Reset entryMethods =
        API.Reset()
        SolverPool.reset()
        statistics.Reset()
        searcher.Reset()
        isStopped <- false
        branchesReleased <- false
        SolverInteraction.setOnSolverStarted statistics.SolverStarted
        SolverInteraction.setOnSolverStopped statistics.SolverStopped
        AcquireBranches()
        match options.explorationMode with
        | TestCoverageMode(coverageZone, _) ->
            Application.setCoverageZone (inCoverageZone coverageZone entryMethods)
        | StackTraceReproductionMode _ -> __notImplemented__()
        Application.resetMethodStatistics()

    member private x.InterpretInternal (isolated : MethodBase seq) (entryPoints : (MethodBase * string[]) seq) (onFinished : Action<UnitTest>)
                       (onException : Action<UnitTest>) (onIIE : Action<InsufficientInformationException>)
                       (onInternalFail : Action<Method option, Exception>) : unit =
        try
            reportFinished <- wrapOnTest onFinished None
            reportError <- wrapOnError onException None
            reportIncomplete <- wrapOnIIE onIIE
            interpreter.ConfigureErrorReporter reportError
            let isolated = isolated |> Seq.toList |> List.map Application.getMethod
            let entryPoints = entryPoints |> Seq.toList |> List.map (fun (m, a) -> Application.getMethod m, a)
            x.Reset (isolated @ (entryPoints |> List.map fst))
            let isolatedInitialStates = isolated |> List.collect x.FormIsolatedInitialStates
            let entryPointsInitialStates = entryPoints |> List.collect x.FormEntryPointInitialStates
            let iieStates, initialStates = isolatedInitialStates @ entryPointsInitialStates |> List.partition (fun state -> state.iie.IsSome)
            iieStates |> List.iter reportIncomplete
            if not initialStates.IsEmpty then
                x.AnswerPobs initialStates
        finally
            statistics.ExplorationFinished()
            API.Restore()
            searcher.Reset()

    member x.Interpret (isolated : MethodBase seq) (entryPoints : (MethodBase * string[]) seq) (onFinished : Action<UnitTest>)
                       (onException : Action<UnitTest>) (onIIE : Action<InsufficientInformationException>)
                       (onInternalFail : Action<Method option, Exception>) : unit =
        reportInternalFail <- wrapOnInternalFail onInternalFail
        try
            let substituteTypeParametersIfPossible method =
                match x.TrySubstituteTypeParameters method with
                | Some newMethod -> newMethod
                | None -> method
            let isolated = Seq.map substituteTypeParametersIfPossible isolated
            let entryPoints = Seq.map (fun (m, p) -> substituteTypeParametersIfPossible m, p) entryPoints
            x.InterpretInternal isolated entryPoints onFinished onException onIIE onInternalFail
        with
        | e -> reportInternalFail None e

    member x.Stop() = isStopped <- true

    member x.Statistics with get() = statistics
