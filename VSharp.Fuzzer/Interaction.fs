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

type private TestRestorer () =
    let executionInfo = System.Collections.Generic.Dictionary<int, ExecutionData>()

    member this.TrackExecutionInfo threadId executionData =
        executionInfo[threadId] <- executionData

    // TODO: Add test restoring
    member this.RestoreTest failReportPath =
        ()

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
            timeLimitPerMethod = 3000
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

    let fuzzerExternalTimelimitPerMethod = TimeSpanBuilders.FromMilliseconds(fuzzerOptions.timeLimitPerMethod * 3)

    let testRestorer = TestRestorer()
    let queued = System.Collections.Generic.Queue<_>(isolated)
    let methodsCount = queued.Count

    let mutable fuzzerService = Unchecked.defaultof<IFuzzerService>
    let mutable fuzzerProcess = Unchecked.defaultof<Process>
    let mutable lastFuzzedMethodTime = DateTime.Now
    let mutable successfullyFuzzedMethodsCount = 0
    let mutable failedFuzzedMethodsCount = 0

    let isAllMethodsWereSent () = queued.Count = 0

    let isFuzzingFinished () =
        successfullyFuzzedMethodsCount + failedFuzzedMethodsCount = methodsCount
        || cancellationToken.IsCancellationRequested


    let handleExit () =
        let unhandledExceptionPath = $"{Directory.GetCurrentDirectory()}{Path.DirectorySeparatorChar}exception.info"
        if File.Exists(unhandledExceptionPath) then
            testRestorer.RestoreTest unhandledExceptionPath

    let setupFuzzer targetAssemblyPath =
        task {
            try
                do! fuzzerService.SetupOutputDirectory { stringValue = outputPath }
                do! fuzzerService.SetupAssembly { assemblyName = targetAssemblyPath }
            with :? TaskCanceledException -> onCancelled ()
        }

    let fuzzNextMethod () =
        task {
            try
                traceCommunication "fuzzNextMethod"
                if isAllMethodsWereSent () |> not then
                    let method = queued.Dequeue()
                    do! fuzzerService.Fuzz {
                        moduleName = method.Module.FullyQualifiedName
                        methodId = method.MetadataToken
                    }
            with :? TaskCanceledException -> onCancelled ()
        }

    let masterProcessService =
        let siliStatisticsConverter = SiliStatisticConverter()

        let onTrackCoverage methods (rawData: RawCoverageReport) =
            let alreadyTracked =
                siliStatisticsConverter.ToSiliStatistic methods rawData.rawCoverageLocations
                |> saveStatistic
            Task.FromResult {
                boolValue = alreadyTracked
            }

        let onTrackExecutionSeed (x: ExecutionData) =
            task {
                testRestorer.TrackExecutionInfo x.threadId x
            } :> Task

        let onFinished () =
            task {
                successfullyFuzzedMethodsCount <- successfullyFuzzedMethodsCount + 1
                lastFuzzedMethodTime <- DateTime.Now
                if isAllMethodsWereSent () |> not then
                    do! fuzzNextMethod ()
                else
                    return ()
            } :> Task

        MasterProcessService(onTrackCoverage, onTrackExecutionSeed, onFinished)

    let startFuzzer () =
        fuzzerProcess <- startFuzzer fuzzerOptions fuzzerDeveloperOptions
        fuzzerService <- connectFuzzerService ()
        waitFuzzerForReady fuzzerService

    let startMasterProcess () =
        startMasterProcessService masterProcessService CancellationToken.None
        |> ignore

    let initialize () =
        cancellationToken.Register(fun () ->
        if not fuzzerProcess.HasExited  then
            fuzzerProcess.Kill ()
        ) |> ignore
        startMasterProcess ()

    let rec startFuzzingLoop (targetAssemblyPath: string) =

        let logLoop msg = traceCommunication $"[Loop] {msg}"

        let startFuzzing () =
            task {
                startFuzzer ()
                do! setupFuzzer targetAssemblyPath
                lastFuzzedMethodTime <- DateTime.Now
                do! fuzzNextMethod ()
            }

        let restartFuzzing () =
            task {
                handleExit ()
                failedFuzzedMethodsCount <- failedFuzzedMethodsCount + 1
                if isFuzzingFinished () then
                    logLoop "Fuzzing finished, no need to restart fuzzer"
                else
                    if isAllMethodsWereSent () |> not then
                        if not fuzzerProcess.HasExited then
                            logLoop "Fuzzer alive, killing and restarting"
                            fuzzerProcess.Kill()
                        do! startFuzzingLoop targetAssemblyPath
                    else
                        logLoop "Has not queued method, no need to restart fuzzer"
            }

        let finish () = fuzzerService.Finish (UnitData())

        let waitForExit () =
            task {
                match fuzzerDeveloperOptions.sanitizersMode with
                | Disabled ->  do! fuzzerProcess.WaitForExitAsync cancellationToken
                | Enabled _ -> do fuzzerProcess.Kill()
            }

        task {
            try
                logLoop $"Start fuzzing {methodsCount} methods"
                do! startFuzzing ()
                while isFuzzingFinished () |> not do
                    let timeFromLastResponse = DateTime.Now - lastFuzzedMethodTime
                    do! Task.Delay(100)
                    if fuzzerProcess.HasExited then
                        logLoop "Fuzzing not finished but fuzzer exited, restarting"
                        do! restartFuzzing ()
                    elif timeFromLastResponse > fuzzerExternalTimelimitPerMethod then
                        logLoop "Fuzzer external timeout per method achieved, restarting"
                        do! restartFuzzing ()

                logLoop "Fuzzer finished, finish loop"
                logLoop $"Successfully fuzzed: {successfullyFuzzedMethodsCount}"
                logLoop $"Failed fuzzed: {failedFuzzedMethodsCount}"
                do! finish ()
                do! waitForExit ()
            with
                | :? TaskCanceledException ->
                    logLoop "Cancelled"
                    onCancelled ()
                | :? System.Net.Http.HttpRequestException
                | :? Grpc.Core.RpcException as ex ->
                    logLoop "GRPC Exception, restarting"
                    logLoop $"{ex.ToString()}"
                    do! restartFuzzing ()
        }

    member this.StartFuzzing () =
        initialize ()
        startFuzzingLoop targetAssemblyPath
