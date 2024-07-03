namespace VSharp.Explorer

open System
open System.IO
open System.Reflection
open System.Threading
open System.Threading.Tasks
open FSharpx.Collections

open VSharp
open VSharp.Core
open VSharp.Interpreter.IL
open CilState
open VSharp.Explorer
open VSharp.Solver

type IReporter =
    abstract member ReportFinished: UnitTest -> unit
    abstract member ReportException : UnitTest -> unit
    abstract member ReportIIE : InsufficientInformationException -> unit
    abstract member ReportInternalFail : Method -> Exception -> unit
    abstract member ReportCrash : Exception -> unit

type EntryPointConfiguration(mainArguments : string[]) =

    member x.Args with get() =
        if mainArguments = null then None
        else Some mainArguments

type WebConfiguration(mainArguments : string[], environmentName : string, contentRootPath : DirectoryInfo, applicationName : string) =
    inherit EntryPointConfiguration(mainArguments)

    member internal x.ToWebConfig() =
        {
            environmentName = environmentName
            contentRootPath = contentRootPath
            applicationName = applicationName
        }

type private IExplorer =
    abstract member Reset: seq<Method> -> unit
    abstract member StartExploration: (Method * state) list -> (Method * EntryPointConfiguration * state) list -> Task

type private SVMExplorer(explorationOptions: ExplorationOptions, statistics: SVMStatistics, reporter: IReporter) =

    let options = explorationOptions.svmOptions

    let hasTimeout = explorationOptions.timeout.TotalMilliseconds > 0

    let solverTimeout =
        if options.solverTimeout > 0 then options.solverTimeout * 1000
        // Setting timeout / 2 as solver's timeout doesn't guarantee that SILI
        // stops exactly in timeout. To guarantee that we need to pass timeout
        // based on remaining time to solver dynamically.
        elif hasTimeout then int explorationOptions.timeout.TotalMilliseconds / 2
        else -1

    let branchReleaseTimeout =
        let doubleTimeout = double explorationOptions.timeout.TotalMilliseconds
        if not hasTimeout then Double.PositiveInfinity
        elif not options.releaseBranches then doubleTimeout
        else doubleTimeout * 80.0 / 100.0

    let hasStepsLimit = options.stepsLimit > 0u

    do
        API.ConfigureSolver(SolverPool.mkSolver(solverTimeout))
        VSharp.System.SetUp.ConfigureInternalCalls()
        API.ConfigureChars(options.prettyChars)

    let mutable branchesReleased = false
    let mutable isStopped = false
    let mutable isCoverageAchieved : unit -> bool = always false

    let emptyState = Memory.EmptyIsolatedState()
    let interpreter = ILInterpreter()

    do
        if options.visualize then
            DotVisualizer explorationOptions.outputDirectory :> IVisualizer |> Application.setVisualizer
        SetMaxBuferSize options.maxBufferSize
        TestGenerator.setMaxBufferSize options.maxBufferSize

    let isSat pc =
        // TODO: consider trivial cases
        emptyState.pc <- pc
        match SolverInteraction.checkSat emptyState with
        | SolverInteraction.SmtSat _
        | SolverInteraction.SmtUnknown _ -> true
        | _ -> false

    let rec mkForwardSearcher mode =
        let getRandomSeedOption() = if options.randomSeed < 0 then None else Some options.randomSeed
        match mode with
        | BFSMode -> BFSSearcher() :> IForwardSearcher
        | DFSMode -> DFSSearcher() :> IForwardSearcher
        | ShortestDistanceBasedMode -> ShortestDistanceBasedSearcher statistics :> IForwardSearcher
        | RandomShortestDistanceBasedMode -> RandomShortestDistanceBasedSearcher(statistics, getRandomSeedOption()) :> IForwardSearcher
        | ContributedCoverageMode -> DFSSortedByContributedCoverageSearcher statistics :> IForwardSearcher
        | ExecutionTreeMode -> ExecutionTreeSearcher(getRandomSeedOption())
        | FairMode baseMode ->
            FairSearcher((fun _ -> mkForwardSearcher baseMode), uint branchReleaseTimeout, statistics) :> IForwardSearcher
        | InterleavedMode(base1, stepCount1, base2, stepCount2) ->
            InterleavedSearcher([mkForwardSearcher base1, stepCount1; mkForwardSearcher base2, stepCount2]) :> IForwardSearcher

    let mutable searcher : IBidirectionalSearcher =
        match options.explorationMode with
        | TestCoverageMode(_, searchMode) ->
            let baseSearcher =
                if options.recThreshold > 0u then
                    GuidedSearcher(mkForwardSearcher searchMode, RecursionBasedTargetManager(statistics, options.recThreshold)) :> IForwardSearcher
                else
                    mkForwardSearcher searchMode
            BidirectionalSearcher(baseSearcher, BackwardSearcher(), DummyTargetedSearcher.DummyTargetedSearcher()) :> IBidirectionalSearcher
        | StackTraceReproductionMode _ -> __notImplemented__()

    let releaseBranches() =
        if not branchesReleased then
            branchesReleased <- true
            statistics.OnBranchesReleased()
            ReleaseBranches()
            let dfsSearcher = DFSSortedByContributedCoverageSearcher statistics :> IForwardSearcher
            let bidirectionalSearcher = OnlyForwardSearcher(dfsSearcher)
            dfsSearcher.Init <| searcher.States()
            searcher <- bidirectionalSearcher

    let reportStateIncomplete (state : cilState) =
        searcher.Remove state
        statistics.IncompleteStates.Add(state)
        Application.terminateState state
        reporter.ReportIIE state.iie.Value

    let reportIncomplete = reporter.ReportIIE

    let reportState (suite : testSuite) (cilState : cilState) =
        try
            let isNewHistory() =
                let methodHistory = Set.filter (fun h -> h.method.InCoverageZone) cilState.history
                Set.isEmpty methodHistory
                || Set.exists (not << statistics.IsBasicBlockCoveredByTest) methodHistory
            let isError = suite.IsErrorSuite
            let isNewTest =
                match suite with
                | Test -> isNewHistory()
                | Error(msg, isFatal) -> statistics.IsNewError cilState msg isFatal
            if isNewTest then
                let state = cilState.state
                let callStackSize = Memory.CallStackSize state
                let entryMethod = cilState.EntryMethod
                let hasException = cilState.IsUnhandledException
                if isError && not hasException then
                    if entryMethod.HasParameterOnStack then
                        Memory.ForcePopFrames (callStackSize - 2) state
                    else Memory.ForcePopFrames (callStackSize - 1) state
                match TestGenerator.state2test suite entryMethod state with
                | Some test ->
                    statistics.TrackFinished(cilState, isError)
                    match suite with
                    | Test -> reporter.ReportFinished test
                    | Error _ -> reporter.ReportException test
                    if isCoverageAchieved() then
                        isStopped <- true
                | None -> ()
        with :? InsufficientInformationException as e ->
            cilState.SetIIE e
            reportStateIncomplete cilState

    let reportStateInternalFail (state : cilState) (e : Exception) =
        match e with
        | :? InsufficientInformationException as e ->
            if not state.IsIIEState then
                state.SetIIE e
            reportStateIncomplete state
        | _ ->
            searcher.Remove state
            statistics.InternalFails.Add(e)
            Application.terminateState state
            reporter.ReportInternalFail state.EntryMethod e

    let reportInternalFail (method : Method) (e : Exception) =
        match e with
        | :? InsufficientInformationException as e ->
            reportIncomplete e
        | _ ->
            statistics.InternalFails.Add(e)
            reporter.ReportInternalFail method e

    let reportFinished (state : cilState) =
        let result = Memory.StateResult state.state
        Logger.info $"Result of method {state.EntryMethod.FullName} is {result}"
        Application.terminateState state
        reportState Test state

    let wrapOnError isFatal (state : cilState) errorMessage =
        if not <| String.IsNullOrWhiteSpace errorMessage then
            Logger.info $"Error in {state.EntryMethod.FullName}: {errorMessage}"
        Application.terminateState state
        let testSuite = Error(errorMessage, isFatal)
        reportState testSuite state

    let reportError = wrapOnError false
    let reportFatalError = wrapOnError true
    let reportCrash = reporter.ReportCrash

    let isTimeoutReached() = hasTimeout && statistics.CurrentExplorationTime.TotalMilliseconds >= explorationOptions.timeout.TotalMilliseconds
    let shouldReleaseBranches() = options.releaseBranches && statistics.CurrentExplorationTime.TotalMilliseconds >= branchReleaseTimeout
    let isStepsLimitReached() = hasStepsLimit && statistics.StepsCount >= options.stepsLimit

    static member private AllocateByRefParameters initialState (method : Method) =
        let allocateIfByRef (pi : ParameterInfo) =
            let parameterType = pi.ParameterType
            if parameterType.IsByRef then
                if Memory.CallStackSize initialState = 0 then
                    Memory.NewStackFrame initialState None []
                let typ = parameterType.GetElementType()
                let position = pi.Position + 1
                let stackRef = Memory.AllocateTemporaryLocalVariableOfType initialState pi.Name position typ
                Some stackRef
            else
                None
        method.Parameters |> Array.map allocateIfByRef |> Array.toList

    member private x.FormIsolatedInitialStates (method : Method, initialState : state) =
        try
            initialState.model <- Memory.EmptyModel method
            let declaringType = method.DeclaringType
            let cilState = cilState.CreateInitial method initialState
            let this =
                if method.HasThis then
                    if Types.IsValueType declaringType then
                        Memory.NewStackFrame initialState None []
                        Memory.AllocateTemporaryLocalVariableOfType initialState "this" 0 declaringType |> Some
                    else
                        let this = Memory.MakeSymbolicThis initialState method
                        !!(IsNullReference this) |> AddConstraint initialState
                        Some this
                else None
            let parameters = SVMExplorer.AllocateByRefParameters initialState method
            Memory.InitFunctionFrame initialState method this (Some parameters)
            match this with
            | Some this -> SolveThisType initialState this
            | _ -> ()
            let cilStates = ILInterpreter.CheckDisallowNullAttribute method None cilState false id
            assert(List.length cilStates = 1)
            if not method.IsStaticConstructor then
                let cilState = List.head cilStates
                interpreter.InitializeStatics cilState declaringType List.singleton
            else
                Memory.MarkTypeInitialized initialState declaringType
                cilStates
        with
        | e ->
            reportInternalFail method e
            []

    member private x.FormEntryPointInitialStates (method : Method, config : EntryPointConfiguration, initialState : state) : cilState list =
        try
            assert method.IsStatic
            let optionArgs = config.Args
            let parameters = method.Parameters
            let hasParameters = Array.length parameters > 0
            let state = { initialState with complete = not hasParameters || Option.isSome optionArgs }
            state.model <- Memory.EmptyModel method
            match optionArgs with
            | Some args ->
                let stringType = typeof<string>
                let argsNumber = MakeNumber args.Length
                let argsRef = Memory.AllocateConcreteVectorArray state argsNumber stringType args
                let args = Some (List.singleton (Some argsRef))
                Memory.InitFunctionFrame state method None args
            | None when hasParameters ->
                Memory.InitFunctionFrame state method None None
                // NOTE: if args are symbolic, constraint 'args != null' is added
                assert(Array.length parameters = 1)
                let argsParameter = Array.head parameters
                let argsParameterTerm = Memory.ReadArgument state argsParameter
                AddConstraint state (!!(IsNullReference argsParameterTerm))
                // Filling model with default args to match PC
                let modelState =
                    match state.model with
                    | StateModel modelState -> modelState
                    | _ -> __unreachable__()
                let argsForModel = Memory.AllocateVectorArray modelState (MakeNumber 0) typeof<String>
                Memory.WriteStackLocation modelState (ParameterKey argsParameter) argsForModel
            | None ->
                let args = Some List.empty
                Memory.InitFunctionFrame state method None args
            Memory.InitializeStaticMembers state method.DeclaringType
            let initialState =
                match config with
                | :? WebConfiguration as config ->
                    let webConfig = config.ToWebConfig()
                    cilState.CreateWebInitial method state webConfig
                | _ -> cilState.CreateInitial method state
            [initialState]
        with
        | e ->
            reportInternalFail method e
            []

    member private x.Forward (s : cilState) =
        let loc = s.approximateLoc
        let ip = s.CurrentIp
        let stackSize = s.StackSize
        // TODO: update pobs when visiting new methods; use coverageZone
        let goodStates, iieStates, errors = interpreter.ExecuteOneInstruction s
        for s in goodStates @ iieStates @ errors do
            if not s.HasRuntimeExceptionOrError then
                statistics.TrackStepForward s ip stackSize
        let goodStates, toReportFinished = goodStates |> List.partition (fun s -> s.IsExecutable || s.IsIsolated)
        toReportFinished |> List.iter reportFinished
        let errors, _ = errors |> List.partition (fun s -> not s.HasReportedError)
        let errors, toReportExceptions = errors |> List.partition (fun s -> s.IsIsolated || not s.IsStoppedByException)
        let runtimeExceptions, userExceptions = toReportExceptions |> List.partition (fun s -> s.HasRuntimeExceptionOrError)
        runtimeExceptions |> List.iter (fun state -> reportError state null)
        userExceptions |> List.iter reportFinished
        let iieStates, toReportIIE = iieStates |> List.partition (fun s -> s.IsIsolated)
        toReportIIE |> List.iter reportStateIncomplete
        let mutable sIsStopped = false
        let newStates =
            match goodStates, iieStates, errors with
            | s'::goodStates, _, _ when LanguagePrimitives.PhysicalEquality s s' ->
                goodStates @ iieStates @ errors
            | _, s'::iieStates, _ when LanguagePrimitives.PhysicalEquality s s' ->
                goodStates @ iieStates @ errors
            | _, _, s'::errors when LanguagePrimitives.PhysicalEquality s s' ->
                goodStates @ iieStates @ errors
            | _ ->
                sIsStopped <- true
                goodStates @ iieStates @ errors
        Application.moveState loc s (Seq.cast<_> newStates)
        statistics.TrackFork s newStates
        searcher.UpdateStates s newStates
        if sIsStopped then
            searcher.Remove s

    member private x.Backward p' (s' : cilState) =
        assert(s'.CurrentLoc = p'.loc)
        let sLvl = s'.Level
        if p'.lvl >= sLvl then
            let lvl = p'.lvl - sLvl
            let pc = Memory.WLP s'.state p'.pc
            match isSat pc with
            | true when not s'.IsIsolated -> searcher.Answer p' (Witnessed s')
            | true ->
                statistics.TrackStepBackward p' s'
                let p = {loc = s'.StartingLoc; lvl = lvl; pc = pc}
                Logger.trace $"Backward:\nWas: {p'}\nNow: {p}\n\n"
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
        while not isStopped && not <| isStepsLimitReached() && not <| isTimeoutReached() && pick() do
            if shouldReleaseBranches() then
                releaseBranches()
            match action with
            | GoFront s ->
                try
                    x.Forward(s)
                with
                | e -> reportStateInternalFail s e
            | GoBack(s, p) ->
                try
                    x.Backward p s
                with
                | e -> reportStateInternalFail s e
            | Stop -> __unreachable__()

    member private x.AnswerPobs initialStates =
        statistics.ExplorationStarted()

        // For backward compatibility. TODO: remove main pobs at all
        let mainPobs = []
        Application.spawnStates (Seq.cast<_> initialStates)
        mainPobs |> Seq.map (fun pob -> pob.loc) |> Seq.toArray |> Application.addGoals
        searcher.Init initialStates mainPobs
        initialStates |> Seq.filter (fun s -> s.IsIIEState) |> Seq.iter reportStateIncomplete
        x.BidirectionalSymbolicExecution()
        searcher.Statuses() |> Seq.iter (fun (pob, status) ->
            match status with
            | pobStatus.Unknown ->
                Logger.warning $"Unknown status for pob at {pob.loc}"
            | _ -> ())

    interface IExplorer with
        member x.Reset entryMethods =
            HashMap.clear()
            API.Reset()
            SolverPool.reset()
            searcher.Reset()
            isStopped <- false
            branchesReleased <- false
            SolverInteraction.setOnSolverStarted statistics.SolverStarted
            SolverInteraction.setOnSolverStopped statistics.SolverStopped
            AcquireBranches()
            isCoverageAchieved <- always false
            match options.explorationMode with
            | TestCoverageMode _ ->
                if options.stopOnCoverageAchieved > 0 then
                    let checkCoverage() =
                        let cov = statistics.GetCurrentCoverage entryMethods
                        cov >= options.stopOnCoverageAchieved
                    isCoverageAchieved <- checkCoverage
            | StackTraceReproductionMode _ -> __notImplemented__()

        member x.StartExploration isolated entryPoints =
            task {
                try
                    try
                        interpreter.ConfigureErrorReporter reportError reportFatalError
                        let isolatedInitialStates = isolated |> List.collect x.FormIsolatedInitialStates
                        let entryPointsInitialStates = entryPoints |> List.collect x.FormEntryPointInitialStates
                        let iieStates, initialStates =
                            isolatedInitialStates @ entryPointsInitialStates
                            |> List.partition (fun state -> state.IsIIEState)
                        iieStates |> List.iter reportStateIncomplete
                        statistics.SetStatesGetter(fun () -> searcher.States())
                        statistics.SetStatesCountGetter(fun () -> searcher.StatesCount)
                        if not initialStates.IsEmpty then
                            x.AnswerPobs initialStates
                    with e -> reportCrash e
                finally
                    try
                        statistics.ExplorationFinished()
                        API.Restore()
                        searcher.Reset()
                    with e -> reportCrash e
            }

    member private x.Stop() = isStopped <- true

type private FuzzerExplorer(explorationOptions: ExplorationOptions, statistics: SVMStatistics) =

    interface IExplorer with

        member x.Reset _ = ()

        member x.StartExploration isolated _ =
            let saveStatistic = statistics.SetBasicBlocksAsCoveredByTest
            let outputDir = explorationOptions.outputDirectory.FullName
            let cancellationTokenSource = new CancellationTokenSource()
            cancellationTokenSource.CancelAfter(explorationOptions.timeout)
            let methodsBase = isolated |> List.map (fun (m, _) -> (m :> IMethod).MethodBase)
            task {
                try
                    let targetAssemblyPath = (Seq.head methodsBase).Module.Assembly.Location
                    let onCancelled () = Logger.warning "Fuzzer canceled"
                    let interactor = Fuzzer.Interactor(
                        targetAssemblyPath,
                        methodsBase,
                        cancellationTokenSource.Token,
                        outputDir,
                        saveStatistic,
                        onCancelled
                    )
                    do! interactor.StartFuzzing ()
                with e ->
                    Logger.error $"Fuzzer unhandled exception: {e}"
                    raise e
            }

type public Explorer(options : ExplorationOptions, reporter: IReporter) =

    let statistics = new SVMStatistics(Seq.empty, true)

    let explorers =
        let createFuzzer () =
            FuzzerExplorer(options, statistics) :> IExplorer

        let createSVM () =
            SVMExplorer(options, statistics, reporter) :> IExplorer

        match options.explorationModeOptions with
        | Fuzzing _ -> createFuzzer() |> Array.singleton
        | SVM _ -> createSVM() |> Array.singleton
        | Combined _ ->
            [|
                createFuzzer()
                createSVM()
            |]

    let inCoverageZone coverageZone (entryMethods : Method list) =
        match coverageZone with
        | MethodZone -> fun method -> entryMethods |> List.contains method
        | ClassZone -> fun method -> entryMethods |> List.exists (fun m -> method.DeclaringType.TypeHandle = m.DeclaringType.TypeHandle)
        | ModuleZone -> fun method -> entryMethods |> List.exists (fun m -> method.Module.ModuleHandle = m.Module.ModuleHandle)

    member private x.TrySubstituteTypeParameters (state : state) (methodBase : MethodBase) =
        let method = Application.getMethod methodBase
        let getConcreteType = function
        | ConcreteType t -> Some t
        | _ -> None
        try
            if method.ContainsGenericParameters then
                match SolveGenericMethodParameters state.typeStorage method with
                | Some(classParams, methodParams) ->
                    let classParams = classParams |> Array.choose getConcreteType
                    let methodParams = methodParams |> Array.choose getConcreteType
                    let declaringType = methodBase.DeclaringType
                    let isSuitableType() =
                        not declaringType.IsGenericType
                        || classParams.Length = declaringType.GetGenericArguments().Length
                    let isSuitableMethod() =
                        methodBase.IsConstructor
                        || not methodBase.IsGenericMethod
                        || methodParams.Length = methodBase.GetGenericArguments().Length
                    if isSuitableType() && isSuitableMethod() then
                        let reflectedType = Reflection.concretizeTypeParameters methodBase.ReflectedType classParams
                        let method = Reflection.concretizeMethodParameters reflectedType methodBase methodParams
                        Some method
                    else
                        None
                | _ -> None
            else Some methodBase
        with
        | e ->
            reporter.ReportInternalFail method e
            None

    member x.Reset entryMethods =
        statistics.Reset entryMethods
        Application.setCoverageZone (inCoverageZone options.coverageZone entryMethods)
        for explorer in explorers do
            explorer.Reset entryMethods

    member x.StartExploration (isolated : MethodBase seq) (entryPoints : (MethodBase * EntryPointConfiguration) seq) : unit =

        try
            let trySubstituteTypeParameters method =
                let emptyState = Memory.EmptyIsolatedState()
                (Option.defaultValue method (x.TrySubstituteTypeParameters emptyState method), emptyState)
            let isolated =
                isolated
                |> Seq.map trySubstituteTypeParameters
                |> Seq.map (fun (m, s) -> Application.getMethod m, s) |> Seq.toList
            let entryPoints =
                entryPoints
                |> Seq.map (fun (m, a) ->
                    let m, s = trySubstituteTypeParameters m
                    (Application.getMethod m, a, s))
                |> Seq.toList

            (List.map fst isolated)
            @ (List.map (fun (x, _, _) -> x) entryPoints)
            |> x.Reset

            let explorationTasks =
                explorers
                |> Array.map (fun e -> e.StartExploration isolated entryPoints)

            let finished = Task.WaitAll(explorationTasks, options.timeout)

            if not finished then Logger.warning "Exploration cancelled"

            for explorationTask in explorationTasks do
                if explorationTask.IsFaulted then
                    for ex in explorationTask.Exception.InnerExceptions do
                    reporter.ReportCrash ex

        with
        | :? AggregateException as e -> reporter.ReportCrash e.InnerException
        | e -> reporter.ReportCrash e

    member x.Statistics with get() = statistics

    interface IDisposable with
        member x.Dispose() = (statistics :> IDisposable).Dispose()
