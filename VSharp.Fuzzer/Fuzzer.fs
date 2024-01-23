namespace VSharp.Fuzzer

open System
open System.Collections.Generic
open System.Diagnostics
open System.IO
open System.Reflection
open System.Threading
open System.Threading.Tasks
open VSharp
open VSharp.CSharpUtils
open VSharp.CoverageTool
open VSharp.Fuzzer.Communication.Contracts
open VSharp.Fuzzer.Startup
open VSharp.Fuzzer.TestGeneration
open Logger

module private CancellableThreads =
    let private internalAbort = typeof<System.Runtime.ControlledExecution>.GetMethod("AbortThread", Reflection.allBindingFlags)
    let private internalGetThreadHandle = typeof<Thread>.GetMethod("GetNativeHandle", Reflection.allBindingFlags)
    let private abortedWasTrying = HashSet<int>()
    let mutable private cancellableThreadStartedCount = 0
    let private cancellableThreadHandledCount = ref 0

    let startCancellableThread f threadId (cancellationToken: CancellationToken) =
        cancellableThreadStartedCount <- cancellableThreadStartedCount + 1

        let systemThread =
            let thread = Thread(fun () -> f(); Interlocked.Increment(cancellableThreadHandledCount) |> ignore)
            thread.Start()
            thread

        let abortThread () =
            if systemThread.IsAlive then
                abortedWasTrying.Add(threadId) |> ignore
                traceFuzzing $"Start aborting: {systemThread.ManagedThreadId}"
                let nativeHandle = internalGetThreadHandle.Invoke(systemThread, [||])
                internalAbort.Invoke(null, [| nativeHandle |]) |> ignore
                systemThread.Join()
                traceFuzzing $"Aborted: {systemThread.ManagedThreadId}"
                Interlocked.Increment(cancellableThreadHandledCount) |> ignore
                Logger.error $"{cancellableThreadStartedCount} = {cancellableThreadHandledCount.Value}"
            else
                traceFuzzing $"Thread is dead: {systemThread.ManagedThreadId}"

        Action abortThread |> cancellationToken.Register |> ignore
        systemThread

    let wasAbortTried = abortedWasTrying.Contains

    let waitAllThreadsHandled () =
        let spinner = SpinWait()
        while cancellableThreadStartedCount <> cancellableThreadHandledCount.Value do
            spinner.SpinOnce()

type internal Fuzzer(
    fuzzerOptions: FuzzerOptions,
    symbolicExecutionService: IMasterProcessService,
    coverageTool: InteractionCoverageTool) =

    let rnd = Random(fuzzerOptions.initialSeed)
    let typeSolver = TypeSolver()
    let generator = Generator(fuzzerOptions, typeSolver)
    let threadIdGenerator = Utils.IdGenerator(0)
    let testIdGenerator =
        let testDir = DirectoryInfo(fuzzerOptions.outputDir)
        let tests = testDir.EnumerateFiles("*.vst")

        let lastTestId =
            tests
            |> Seq.map (fun x -> x.Name)
            |> Seq.filter (fun x -> x.StartsWith "fuzzer_test_")
            |> Seq.map (fun x -> x.Replace("fuzzer_test_", "").Replace(".vst", "") |> int)
            |> Seq.cons 0
            |> Seq.max

        Utils.IdGenerator(lastTestId)

    let batchSize = Process.GetCurrentProcess().Threads.Count
    let outputDir = fuzzerOptions.outputDir

    let mutable generatedCount = 0
    let mutable abortedCount = 0
    let mutable ignoredCount = 0

    let stopwatch = Stopwatch()
    let getAvailableTime () =
        fuzzerOptions.timeLimitPerMethod - int stopwatch.ElapsedMilliseconds

    let copyArgs (args: obj array) =
        let wrap o = { object = o }
        let unwrap pa = pa.object
        let copier = Utils.Copier()
        args |> Array.map (wrap >> copier.DeepCopy >> unwrap)

    let invoke (method: MethodBase) this args =
        try
            let returned = method.Invoke(this, copyArgs args)
            Returned returned
        with
        | :? TargetInvocationException as e ->
            Thrown e.InnerException
        | e ->
            logUnhandledException e

    let fuzzOnce method (generationDatas: GenerationData[]) (results: InvocationResult[]) i threadId =
        fun () ->
            coverageTool.SetCurrentThreadId threadId
            let generationData = generationDatas[i]
            results[i] <- invoke method generationData.this generationData.args

    let generateTest (test: UnitTest) =
        let testPath = $"{outputDir}{Path.DirectorySeparatorChar}fuzzer_test_{testIdGenerator.NextId()}.vst"
        Task.Run(fun () ->
            test.Serialize(testPath)
            infoFuzzing $"Generated test: {testPath}"
        ).ForgetUntilExecutionRequested()

    let fuzzBatch mockedGenerics typeSolverSeed (rnd: Random) (method: MethodBase) typeStorage =
        use fuzzingCancellationTokenSource = new CancellationTokenSource()

        traceFuzzing "Generate data"
        let data = Array.init batchSize (fun _ -> generator.Generate mockedGenerics method typeStorage (rnd.Next()))
        let invocationResults = Array.init batchSize (fun _ -> Unchecked.defaultof<InvocationResult>)
        let threadIds = Array.init batchSize (fun _ -> threadIdGenerator.NextId())
        traceFuzzing "Data generated"

        let indices = [|0..batchSize - 1|]

        traceFuzzing "Send execution seeds"
        indices
        |> Array.iter (fun i ->
            symbolicExecutionService.TrackExecutionSeed({
                moduleName = method.Module.FullyQualifiedName
                methodId = method.MetadataToken
                threadId = threadIds[i]
                fuzzerSeed = data[i].seed
                typeSolverSeed = typeSolverSeed
            }).Wait()
        )
        traceFuzzing "Execution seeds sent"

        let availableTime = getAvailableTime ()

        if availableTime <= 0 then
            traceFuzzing "Method invocation not started, no time available"
            None
        else
            traceFuzzing $"Start method invocation, available time: {availableTime}"
            let threads =
                indices
                |> Array.map (fun i  ->
                    CancellableThreads.startCancellableThread
                        (fuzzOnce method data invocationResults i threadIds[i])
                        threadIds[i]
                        fuzzingCancellationTokenSource.Token
                )
            fuzzingCancellationTokenSource.CancelAfter(availableTime)
            threads |> Array.iter (fun t -> t.Join())
            CancellableThreads.waitAllThreadsHandled ()
            traceFuzzing "Method invoked"

            (threadIds, data, invocationResults) |> Some

    let printStatistics (method: Method) =
        printfn $"\nMethod[{method.Name}]"
        printfn $"Generated: {generatedCount}"
        printfn $"Ignored: {ignoredCount}"
        printfn $"Aborted: {abortedCount}"
        generatedCount <- 0
        ignoredCount <- 0
        abortedCount <- 0

    let handleResults (method: Method) result =

        let sendCoverage (methods: Dictionary<int, RawMethodInfo>) coverageReports =
            task {
                let mainMethod =
                    methods
                    |> Seq.find (fun x -> int x.Value.methodToken = method.MetadataToken)

                let reports =
                    coverageReports
                    |> Array.map (fun coverageReport ->
                        coverageReport.rawCoverageLocations
                        |> Array.filter (fun x -> x.methodId = mainMethod.Key)
                        |> fun x -> {
                            coverageReport with rawCoverageLocations = x
                        }
                    )

                let! isNewCoverage = symbolicExecutionService.TrackCoverage({
                    rawData = reports
                    methods = methods
                })

                return isNewCoverage
            }

        let onNewCoverage generationData invocationResult =
            let test = fuzzingResultToTest generationData invocationResult
            match test with
            | Some test ->
                generateTest test
                generatedCount <- generatedCount + 1
                infoFuzzing "Test will be generated"
            | None ->
                infoFuzzing "Failed to create test"
                ignoredCount <- ignoredCount + 1
                ()

        let onKnownCoverage () =
            ignoredCount <- ignoredCount + 1
            traceFuzzing "Coverage already tracked"

        task {
            let (threadIds: int[], data: GenerationData[], invocationResults: InvocationResult[]) = result
            let indices = [|0..batchSize - 1|]
            traceFuzzing "Coverage requested"
            let rawCoverage = coverageTool.GetRawHistory()
            traceFuzzing "Coverage received"
            let coverages = CoverageDeserializer.getRawReports rawCoverage
            traceFuzzing $"Coverage reports[{coverages.reports.Length}] deserialized"
            assert (coverages.reports.Length = batchSize)

            let reports =
                coverages.reports
                |> Array.sortBy (fun x -> x.threadId)
                |> Array.mapi (fun i x ->
                    let abortedInsideFuzzerCode =
                        x.rawCoverageLocations <> [||]
                        && Utils.isNull invocationResults[i]
                        && CancellableThreads.wasAbortTried x.threadId
                    if abortedInsideFuzzerCode then
                        { x with rawCoverageLocations = [| |] }
                    else
                        x
                )

            let existNonAborted = reports |> Seq.exists (fun x -> x.rawCoverageLocations <> [||])

            if existNonAborted then
                let! isNewCoverages = sendCoverage coverages.methods reports

                for i in indices do
                    let coverage = reports[i]
                    let threadId = threadIds[i]
                    let invocationResult = invocationResults[i]
                    let generationData = data[i]
                    let isNewCoverage = isNewCoverages.boolValues[i]

                    assert (int coverage.threadId = threadId)

                    traceFuzzing $"Handler result for {threadIds[i]}"
                    match coverage.rawCoverageLocations with
                    | [||] ->
                        traceFuzzing "Aborted"
                        abortedCount <- abortedCount + 1
                    | _ ->
                        traceFuzzing "Invoked"
                        assert(not <| Utils.isNull invocationResult)
                        if isNewCoverage then
                            onNewCoverage generationData invocationResult
                        else
                            onKnownCoverage ()
            else
                traceFuzzing "All aborted"
                abortedCount <- abortedCount + reports.Length
        }

    member this.AsyncFuzz (method: Method) =
        task {
            try
                stopwatch.Reset()

                traceFuzzing $"Start fuzzing: {method.Name}, batch size: {batchSize}"

                let typeSolverSeed = rnd.Next()
                let typeSolverRnd = Random(typeSolverSeed)

                match typeSolver.SolveGenericMethodParameters method (generator.GenerateClauseObject typeSolverRnd) with
                | Some(methodBase, typeStorage, mockedGenerics) ->

                    traceFuzzing "Generics successfully solved"
                    while int stopwatch.ElapsedMilliseconds < fuzzerOptions.timeLimitPerMethod do
                        traceFuzzing "Start fuzzing iteration"

                        stopwatch.Start()
                        match fuzzBatch mockedGenerics typeSolverSeed rnd methodBase typeStorage with
                        | Some results -> do! handleResults method results
                        | None -> ()
                        stopwatch.Stop()

                    printStatistics method
                | None -> traceFuzzing "Generics solving failed"
            with
                | :? InsufficientInformationException as e ->
                    errorFuzzing $"Insufficient information: {e.Message}\nStack trace:\n{e.StackTrace}"
                | :? NotImplementedException as e ->
                    errorFuzzing $"Not implemented: {e.Message}\nStack trace:\n{e.StackTrace}"
        }
