namespace VSharp.Fuzzer

open System
open System.Diagnostics
open System.IO
open System.Reflection
open System.Threading
open System.Threading.Tasks
open VSharp
open VSharp.CSharpUtils
open VSharp.Fuzzer.Communication
open Logger
open VSharp.Fuzzer.Communication.Contracts
open VSharp.Fuzzer.Communication.Services
open VSharp.Fuzzer.Startup

type private SiliStatisticConverter() =

    let methods = System.Collections.Generic.Dictionary<uint32, Method>()

    member this.GetMethod token = methods[token]

    member this.ToSiliStatistic
        (methodInfo: System.Collections.Generic.Dictionary<int, RawMethodInfo>)
        (loc: RawCoverageLocation seq) =

        let getMethod l =
            match methods.TryGetValue(l.methodToken) with
            | true, m -> m
            | false, _ ->
                let methodBase = Reflection.resolveMethodBase l.assemblyName l.moduleName (int l.methodToken)
                let method = Application.getMethod methodBase
                methods.Add (l.methodToken, method)
                method

        let toCodeLocation l =
            {
                offset = LanguagePrimitives.Int32WithMeasure (int l.offset)
                method = methodInfo[l.methodId] |> getMethod
            }

        loc |> Seq.map toCodeLocation

type private TestRestorer (fuzzerOptions, assemblyPath, outputDirectory) =
    let executionInfo = System.Collections.Generic.Dictionary<int, ExecutionData>()
    let typeSolver = TypeSolver()
    let generator = Generator(fuzzerOptions, typeSolver)
    let errorTestIdGenerator = VSharp.Fuzzer.Utils.IdGenerator(0)

    member this.TrackExecutionInfo threadId executionData =
        executionInfo[threadId] <- executionData

    member this.RestoreTest threadId exceptionName =
        let executionData = executionInfo[threadId]

        let typeSolverRnd = Random(executionData.typeSolverSeed)

        let assembly = AssemblyManager.LoadFromAssemblyPath assemblyPath
        let methodBase =
            Reflection.resolveMethodBaseFromAssembly assembly executionData.moduleName executionData.methodId
            |> AssemblyManager.NormalizeMethod
            |> Application.getMethod

        match typeSolver.SolveGenericMethodParameters methodBase (generator.GenerateObject typeSolverRnd) with
        | Some(methodBase, typeStorage) ->

            let data = generator.Generate methodBase typeStorage executionData.fuzzerSeed

            let thrown =
                match exceptionName with
                | "System.AccessViolationException" -> AccessViolationException() :> exn
                | "System.StackOverflowException" -> StackOverflowException() :> exn
                | _ -> internalfail $"Unexpected exception: {exceptionName}"

            match TestGeneration.fuzzingResultToTest data (Thrown thrown) with
            | Some test ->
                let testPath = $"{outputDirectory}{Path.DirectorySeparatorChar}fuzzer_error_{errorTestIdGenerator.NextId()}.vst"
                test.Serialize(testPath)
            | None _ -> error "Failed to create test while restoring"

        | None -> internalfail "Unexpected generic solving fail"

type private FuzzingProcessState =
    | SuccessfullyFuzzedMethod
    | SuccessfullyFuzzedMethodWithException of (int * string) option
    | UnexpectedExit
    | FuzzingInProcess
    | TimeoutReached
    | UnhandledException of exn
    | NotStarted
    | Started
    | RestartingInProcess
    | Restarted

type private FuzzingProcess(outputPath, targetAssemblyPath, fuzzerOptions, fuzzerDeveloperOptions) =
    let mutable fuzzerService = Unchecked.defaultof<IFuzzerService>
    let mutable fuzzerProcess = Unchecked.defaultof<Process>
    let mutable state = NotStarted
    let mutable lastRequestTime = Unchecked.defaultof<DateTime>


    let unhandledExceptionPath = $"{outputPath}{Path.DirectorySeparatorChar}exception.info"
    let logFuzzingProcess msg = traceCommunication $"[FuzzingProcess] {msg}"

    let fuzzerStarted () = fuzzerProcess <> null
    let fuzzerAlive () = fuzzerStarted () && (not fuzzerProcess.HasExited)

    let markException ex =
        state <- UnhandledException ex

    let mutable newSuccessfullyFuzzedNotificationReceived = false

    let waitStartupTimeout = TimeSpanBuilders.FromMilliseconds(10000)
    let waitFuzzerNotificationTimeout = TimeSpanBuilders.FromMilliseconds(fuzzerOptions.timeLimitPerMethod * 3)
    let pollingDelay = TimeSpanBuilders.FromMilliseconds(fuzzerOptions.timeLimitPerMethod / 2)

    let mutable currentTimeout = waitStartupTimeout

    let updateTimerOnFuzzingRequest () =
        lastRequestTime <- DateTime.Now
        currentTimeout <- waitFuzzerNotificationTimeout

    let updateTimerOnStartRequest () =
        lastRequestTime <- DateTime.Now
        currentTimeout <- waitStartupTimeout

    let timeoutReached () =
        DateTime.Now - lastRequestTime > currentTimeout

    let (|UnhandledExn|StackOverflow|UnexpectedExit|TimeoutReached|WorkInProgress|SuccessfullyFuzzed|) () =
            assert (state = FuzzingInProcess)
            if newSuccessfullyFuzzedNotificationReceived then
                newSuccessfullyFuzzedNotificationReceived <- false
                SuccessfullyFuzzed
            elif timeoutReached () then
                TimeoutReached
            elif fuzzerAlive () |> not then
                if File.Exists(unhandledExceptionPath) then
                    UnhandledExn unhandledExceptionPath
                elif fuzzerProcess.ExitCode = 0 then
                    // In case of StackOverflowException fuzzer can't write exception.info, but will exited with code 0
                    StackOverflow
                else
                    UnexpectedExit
            else
                WorkInProgress

    let updateState =
        function
        | UnhandledExn path ->
            let text = File.ReadAllText(path).Split(" ")
            assert (text.Length = 2)
            File.Delete path
            state <- SuccessfullyFuzzedMethodWithException <| Some (int text[0], text[1])
        | StackOverflow ->
            state <- SuccessfullyFuzzedMethodWithException None
        | UnexpectedExit ->
            state <- UnexpectedExit
        | TimeoutReached ->
            state <- TimeoutReached
        | SuccessfullyFuzzed ->
            state <- SuccessfullyFuzzedMethod
        | WorkInProgress -> ()


    member this.Start () =
        logFuzzingProcess "Starting"
        assert (fuzzerAlive () |> not)
        updateTimerOnStartRequest ()
        task {
            fuzzerProcess <- startFuzzer fuzzerOptions fuzzerDeveloperOptions
            fuzzerService <- connectFuzzerService ()
            waitFuzzerForReady fuzzerService
            logFuzzingProcess $"Setup Fuzzer: {outputPath} {targetAssemblyPath}"
            do! fuzzerService.SetupOutputDirectory { stringValue = outputPath }
            do! fuzzerService.SetupAssembly { assemblyName = targetAssemblyPath }
            state <- Started
        }

    member this.FuzzMethod (method: MethodBase) =
        logFuzzingProcess $"Fuzzing method: {method.Name}"
        assert fuzzerAlive ()
        assert (state <> FuzzingInProcess)
        updateTimerOnFuzzingRequest ()
        task {
            try
                lastRequestTime <- DateTime.Now
                state <- FuzzingInProcess
                do! fuzzerService.Fuzz {
                    moduleName = method.Module.FullyQualifiedName
                    methodId = method.MetadataToken
                }
            with e -> markException e
        }

    member this.RestartFuzzing () =
        state <- RestartingInProcess
        logFuzzingProcess "Restarting fuzzing"
        task {
            assert fuzzerStarted ()
            try
                if fuzzerAlive () then
                    logFuzzingProcess "Fuzzer alive, killing"
                    fuzzerProcess.Kill()
                    fuzzerProcess.WaitForExit()
                do! this.Start ()
                state <- Restarted
            with e -> markException e
        }

    member this.NotifyMethodFuzzingFinished () =
        logFuzzingProcess "Fuzzing method finished"
        newSuccessfullyFuzzedNotificationReceived <- true

    member this.Poll handleState =
        task {
            try
                if state = FuzzingInProcess then
                    updateState ()
                while state = FuzzingInProcess do
                    do! Task.Delay(pollingDelay)
                    updateState()
            with e -> markException e
            do! handleState state
        }

    member this.WaitForExit () =
        logFuzzingProcess "Wait for exit"
        assert fuzzerStarted ()
        task {
            try
                do! fuzzerService.Finish(UnitData())
                match fuzzerDeveloperOptions.sanitizersMode with
                | Disabled ->
                    logFuzzingProcess "Sanitizers disabled, wait for exit"
                    do! fuzzerProcess.WaitForExitAsync ()
                | Enabled _ ->
                    logFuzzingProcess "Sanitizers enabled, killing process"
                    this.Kill()
            with e -> markException e
        }

    member this.Kill () =
        logFuzzingProcess "Killing process, if not exited"
        assert fuzzerStarted ()
        try
            if fuzzerProcess.HasExited |> not then fuzzerProcess.Kill()
        with e -> markException e

type Interactor (
    targetAssemblyPath: string,
    isolated: MethodBase seq,
    cancellationToken: CancellationToken,
    outputPath: string,
    saveStatistic,
    onCancelled: unit -> unit
    ) =

    // TODO: make options configurable (CLI & Tests)
    let fuzzerOptions =
        {
            initialSeed = 42
            timeLimitPerMethod = 1000
            arrayMaxSize = 10
            stringMaxSize = 10
        }

    let fuzzerDeveloperOptions =
        {
            logPath = Directory.GetCurrentDirectory()
            redirectStdout = true
            redirectStderr = true
            waitDebuggerAttachedFuzzer = false
            waitDebuggerAttachedCoverageTool = false
            waitDebuggerAttachedOnAssertCoverageTool = false
            sanitizersMode = Disabled
        }

    let siliStatisticsConverter = SiliStatisticConverter()
    let testRestorer = TestRestorer(fuzzerOptions, targetAssemblyPath, outputPath)
    let queued = System.Collections.Generic.Queue<_>(isolated)

    let hasNextMethod () = queued.Count <> 0

    let nextMethod () =
        if hasNextMethod () |> not then
            None
        else
            queued.Dequeue() |> Some

    let methodsCount = queued.Count
    let fuzzingProcess = FuzzingProcess(outputPath, targetAssemblyPath, fuzzerOptions, fuzzerDeveloperOptions)

    let mutable successfullyFuzzed = 0
    let mutable failedFuzzed = 0

    let isAllMethodsWereSent () = queued.Count = 0
    let isFuzzingFinished () =
        (successfullyFuzzed + failedFuzzed) = methodsCount
        || cancellationToken.IsCancellationRequested

    let handleFuzzingProcessState state =

        let onFailed () =
            failedFuzzed <- failedFuzzed + 1
            task {
                if hasNextMethod () then
                    do! fuzzingProcess.RestartFuzzing ()
            }

        let fuzzNextMethod () =
            task {
                match nextMethod () with
                | Some method -> do! fuzzingProcess.FuzzMethod method
                | None -> ()
            }

        Logger.error $"Current state: {state}"
        task {
            match state with
            | SuccessfullyFuzzedMethod ->
                successfullyFuzzed <- successfullyFuzzed + 1
                do! fuzzNextMethod ()
            | SuccessfullyFuzzedMethodWithException (Some (threadId, exceptionName)) ->
                successfullyFuzzed <- successfullyFuzzed + 1
                testRestorer.RestoreTest threadId exceptionName
            | UnexpectedExit ->
                do! onFailed()
            | SuccessfullyFuzzedMethodWithException None
            | TimeoutReached ->
                do! onFailed ()
            | NotStarted ->
                do! fuzzingProcess.Start()
            | FuzzingInProcess ->
                ()
            | Started
            | Restarted ->
                do! fuzzNextMethod ()
            | RestartingInProcess as x ->
                failwith $"Unexpected state: {x}"
            | UnhandledException exn when (exn :? TaskCanceledException) ->
                onCancelled ()
            | UnhandledException exn when (exn :? Grpc.Core.RpcException || exn :? System.Net.Http.HttpRequestException) ->
                do! onFailed ()
            | UnhandledException exn ->
                failwith $"Unhandled exception: {exn}"
        }

    let masterProcessService =

        let onMethodFuzzingFinished () =
            task {
                fuzzingProcess.NotifyMethodFuzzingFinished ()
            } :> Task

        let onTrackCoverage methods (rawData: RawCoverageReport[]) =

            let trackReport report =
                siliStatisticsConverter.ToSiliStatistic methods report.rawCoverageLocations
                |> saveStatistic

            Task.FromResult {
                boolValues = rawData |> Array.map trackReport
            }

        let onTrackExecutionSeed (x: ExecutionData) =
            task {
                testRestorer.TrackExecutionInfo x.threadId x
            } :> Task

        MasterProcessService(onTrackCoverage, onTrackExecutionSeed, onMethodFuzzingFinished)

    member this.StartFuzzing () =
        task {
            try
                cancellationToken.Register(fun () -> fuzzingProcess.Kill()) |> ignore
                startMasterProcessService masterProcessService CancellationToken.None |> ignore
                while isFuzzingFinished () |> not do
                    Logger.error $"{successfullyFuzzed} + {failedFuzzed} = {methodsCount}"
                    do! fuzzingProcess.Poll handleFuzzingProcessState
                do! fuzzingProcess.WaitForExit()
            finally
                fuzzingProcess.Kill()
        }
