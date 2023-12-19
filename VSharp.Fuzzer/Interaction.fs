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

type private FuzzingProcess(outputPath, targetAssemblyPath, fuzzerOptions, fuzzerDeveloperOptions) =
    let mutable fuzzerService = Unchecked.defaultof<IFuzzerService>
    let mutable fuzzerProcess = Unchecked.defaultof<Process>
    let mutable state = NotStarted
    let mutable lastFuzzedMethodTime = Unchecked.defaultof<DateTime>
    let mutable lastFuzzingRequestTime = Unchecked.defaultof<DateTime>

    let fuzzerExternalTimeLimitPerMethod =
        let needToAttach =
            fuzzerDeveloperOptions.waitDebuggerAttachedFuzzer
            || fuzzerDeveloperOptions.waitDebuggerAttachedCoverageTool
            || fuzzerDeveloperOptions.waitDebuggerAttachedOnAssertCoverageTool
        if needToAttach then
            TimeSpan.MaxValue
        else
            TimeSpanBuilders.FromMilliseconds(fuzzerOptions.timeLimitPerMethod * 3)

    let unhandledExceptionPath = $"{outputPath}{Path.DirectorySeparatorChar}exception.info"

    let logFuzzingProcess msg = traceCommunication $"[FuzzingProcess] {msg}"

    let fuzzerStarted () = fuzzerProcess <> null
    let fuzzerAlive () = fuzzerStarted () && (not fuzzerProcess.HasExited)

    let markFinished newState =
        assert fuzzerStarted ()
        assert (state = FuzzingInProcess)
        assert (newState <> FuzzingInProcess)
        lastFuzzedMethodTime <- DateTime.Now
        state <- newState

    let markException ex =
        UnhandledException ex |> markFinished

    let updateState () =
        assert (state = FuzzingInProcess)
        if DateTime.Now - lastFuzzingRequestTime > fuzzerExternalTimeLimitPerMethod then
            state <- TimeoutReached
        elif fuzzerAlive () |> not then
            if File.Exists(unhandledExceptionPath) then
                let text = File.ReadAllText(unhandledExceptionPath).Split(" ")
                assert (text.Length = 2)
                File.Delete unhandledExceptionPath
                state <- SuccessfullyFuzzedMethodWithException <| Some (int text[0], text[1])
            elif fuzzerProcess.ExitCode = 0 then
                // In case of StackOverflowException fuzzer can't write exception.info, but will exited with code 0
                state <- SuccessfullyFuzzedMethodWithException None
            else
                state <- UnexpectedExit

    member this.Start () =
        logFuzzingProcess "Starting"
        assert (fuzzerStarted () |> not)
        task {
            fuzzerProcess <- startFuzzer fuzzerOptions fuzzerDeveloperOptions
            fuzzerService <- connectFuzzerService ()
            waitFuzzerForReady fuzzerService
            logFuzzingProcess $"Setup Fuzzer: {outputPath} {targetAssemblyPath}"
            do! fuzzerService.SetupOutputDirectory { stringValue = outputPath }
            do! fuzzerService.SetupAssembly { assemblyName = targetAssemblyPath }
        }

    member this.FuzzMethod (method: MethodBase) =
        logFuzzingProcess $"Fuzzing method: {method.Name}"
        assert fuzzerAlive ()
        assert (state <> FuzzingInProcess)
        task {
            try
                lastFuzzingRequestTime <- DateTime.Now
                state <- FuzzingInProcess
                do! fuzzerService.Fuzz {
                    moduleName = method.Module.FullyQualifiedName
                    methodId = method.MetadataToken
                }
            with e -> markException e
        }

    member this.RestartFuzzing () =
        logFuzzingProcess "Restarting fuzzing"
        task {
            assert fuzzerStarted ()
            try
                if fuzzerAlive () |> not then
                    logFuzzingProcess "Fuzzer alive, killing"
                    fuzzerProcess.Kill()
                do! this.Start ()
            with e -> markException e
        }

    member this.NotifyMethodFuzzingFinished () =
        logFuzzingProcess "Fuzzing method finished"
        try
            markFinished SuccessfullyFuzzedMethod
        with e -> markException e

    member this.Poll handleState =
        assert fuzzerStarted ()
        try
            Logger.error $"{state}"
            if state = FuzzingInProcess then
                updateState ()
        with e -> markException e
        handleState state

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
    let nextMethod () =
        if queued.Count = 0 then
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
                match nextMethod () with
                | Some method ->
                    do! fuzzingProcess.RestartFuzzing ()
                    do! fuzzingProcess.FuzzMethod method
                | None -> ()
            }

        task {
            match state with
            | SuccessfullyFuzzedMethod ->
                successfullyFuzzed <- successfullyFuzzed + 1
            | SuccessfullyFuzzedMethodWithException (Some (threadId, exceptionName)) ->
                testRestorer.RestoreTest threadId exceptionName
            | SuccessfullyFuzzedMethodWithException None
            | UnexpectedExit
            | TimeoutReached ->
                do! onFailed ()
            | NotStarted
            | FuzzingInProcess ->
                ()
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
                successfullyFuzzed <- successfullyFuzzed + 1
                fuzzingProcess.NotifyMethodFuzzingFinished ()
                match nextMethod () with
                | Some method -> do! fuzzingProcess.FuzzMethod method
                | None -> ()
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
                match nextMethod () with
                | Some method ->
                    cancellationToken.Register(fun () -> fuzzingProcess.Kill()) |> ignore
                    startMasterProcessService masterProcessService CancellationToken.None |> ignore
                    do! fuzzingProcess.Start()
                    do! fuzzingProcess.FuzzMethod method
                    while isFuzzingFinished () |> not do
                        do! Task.Delay(fuzzerOptions.timeLimitPerMethod)
                        do! fuzzingProcess.Poll handleFuzzingProcessState
                    do! fuzzingProcess.WaitForExit()
                | None -> ()
            finally
                fuzzingProcess.Kill()
        }
