namespace VSharp.Fuzzer

open System
open System.Collections.Generic
open System.Diagnostics
open System.IO
open System.Reflection
open System.Threading
open System.Threading.Tasks
open VSharp
open VSharp.Core

open VSharp.Fuzzer.Communication.Contracts
open VSharp.Fuzzer.Startup

open VSharp.Fuzzer.TestGeneration
open Logger

module private CancellableThreads =
    let private internalAbort = typeof<System.Runtime.ControlledExecution>.GetMethod("AbortThread", Reflection.allBindingFlags)
    let private internalGetThreadHandle = typeof<Thread>.GetMethod("GetNativeHandle", Reflection.allBindingFlags)

    let startCancellableThread f (cancellationToken: CancellationToken) =
        let systemThread =
            let thread = Thread(fun () -> f())
            thread.Start()
            thread

        let abortThread () =
            if not systemThread.IsAlive then
                traceFuzzing $"Start aborting: {systemThread.ManagedThreadId}"
                let nativeHandle = internalGetThreadHandle.Invoke(systemThread, [||])
                internalAbort.Invoke(null, [| nativeHandle |]) |> ignore
                traceFuzzing $"Aborted: {systemThread.ManagedThreadId}"
                systemThread.Join()

        Action abortThread |> cancellationToken.Register |> ignore
        systemThread

type internal Fuzzer(
    fuzzerOptions: FuzzerOptions,
    symbolicExecutionService: IMasterProcessService,
    coverageTool: CoverageTool
    ) =

    let rnd = Random(fuzzerOptions.initialSeed)
    let typeSolver = TypeSolver()
    let generator = Generator(fuzzerOptions, typeSolver)
    let threadIdGenerator = Utils.IdGenerator(0)
    let testIdGenerator = Utils.IdGenerator(0)
    let batchSize = Process.GetCurrentProcess().Threads.Count

    let mutable currentOutputDir = ""
    let mutable currentMillisecondsElapsed = 0

    let mutable generatedCount = 0
    let mutable abortedCount = 0
    let mutable ignoredCount = 0

    let getAvailableTime () =
        fuzzerOptions.timeLimitPerMethod - currentMillisecondsElapsed

    let copyArgs (args: obj array) =
        let wrap o = { object = o }
        let unwrap pa = pa.object
        let copier = Utils.Copier()
        args |> Array.map (wrap >> copier.DeepCopy >> unwrap)

    let invoke (method: MethodBase) this args =
        try
            let returned = method.Invoke(this, copyArgs args)
            traceFuzzing "Method returned"
            Returned returned
        with
        | :? TargetInvocationException as e ->
            traceFuzzing "Method thrown exception"
            Thrown e

    let fuzzOnce method (generationDatas: GenerationData[]) (results: InvocationResult[]) i threadId =
        fun () ->
            coverageTool.SetCurrentThreadId threadId
            let generationData = generationDatas[i]
            results[i] <- invoke method generationData.this generationData.args

    let fuzzBatch typeSolverSeed (rnd: Random) (method: MethodBase) typeStorage =
        use fuzzingCancellationTokenSource = new CancellationTokenSource()

        traceFuzzing "Generate data"
        let data = Array.init batchSize (fun _ -> generator.Generate method typeStorage (rnd.Next()))
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

        traceFuzzing "Start method invocation"
        let threads = 
            indices
            |> Array.map (fun i  ->
                CancellableThreads.startCancellableThread
                    (fuzzOnce method data invocationResults i threadIds[i])
                    fuzzingCancellationTokenSource.Token
            )

        let availableTime = getAvailableTime ()
        if availableTime > 0 then
            fuzzingCancellationTokenSource.CancelAfter(availableTime)
        else
            fuzzingCancellationTokenSource.Cancel()

        threads |> Array.iter (fun t -> t.Join())
        traceFuzzing "Method invoked"

        threadIds, data, invocationResults

    let printStatistics (method: Method) =
        printfn $"\nMethod[{method.Id}]: {method.Name}"
        printfn $"Generated: {generatedCount}"
        printfn $"Ignored: {ignoredCount}"
        printfn $"Aborted: {abortedCount}"
        generatedCount <- 0
        ignoredCount <- 0
        abortedCount <- 0

    let handleResults result =
        
        let onCollected methods coverageReport generationData invocationResult =
            task {
                let! isNewCoverage = symbolicExecutionService.TrackCoverage({
                    rawData = coverageReport
                    methods = methods
                })
                if isNewCoverage.boolValue then
                    let test = fuzzingResultToTest generationData invocationResult
                    match test with
                    | Some test ->
                        let testPath = $"{currentOutputDir}{Path.DirectorySeparatorChar}fuzzer_test_{testIdGenerator.NextId()}.vst"
                        test.Serialize(testPath)
                        generatedCount <- generatedCount + 1
                        infoFuzzing $"Generated test: {testPath}"
                    | None ->
                        ignoredCount <- ignoredCount + 1
                        ()
                else
                    infoFuzzing "Coverage already tracked"
            }

        task {
            let (threadIds: int[], data: GenerationData[], invocationResults: InvocationResult[]) = result
            let indices = [|0..batchSize - 1|]
            traceFuzzing "Coverage requested"
            let rawCoverage = coverageTool.GetRawHistory()
            traceFuzzing "Coverage received"
            let coverages = CoverageDeserializer.getRawReports rawCoverage
            traceFuzzing "Coverage deserialized"
            assert (coverages.reports.Length = batchSize)
            let coverages = {
                methods = coverages.methods
                reports = Array.sortBy (fun x -> x.threadId) coverages.reports
            }
            for i in indices do
                let coverage = coverages.reports[i]
                let threadId = threadIds[i]
                let invocationResult = invocationResults[i]
                let generationData = data[i]
                assert (int coverage.threadId = threadId)

                traceFuzzing $"Handler result for {threadIds[i]}"
                match coverage.rawCoverageLocations with
                | [||] ->
                    abortedCount <- abortedCount + 1
                    traceFuzzing "Aborted"
                | _ ->
                    traceFuzzing "Finished"
                    assert(not <| Utils.isNull invocationResult)
                    do! onCollected coverages.methods coverage generationData invocationResult
        }

    member this.AsyncFuzz (method: Method) =
        task {
            try
                let stopwatch = Stopwatch()
                currentMillisecondsElapsed <- 0

                traceFuzzing $"Start fuzzing: {method.Name}, batch size: {batchSize}"

                let typeSolverSeed = rnd.Next()
                let typeSolverRnd = Random(typeSolverSeed)

                match typeSolver.SolveGenericMethodParameters method (generator.GenerateObject typeSolverRnd) with
                | Some(methodBase, typeStorage) ->

                    traceFuzzing "Generics successfully solved"
                    stopwatch.Start()

                    while currentMillisecondsElapsed < fuzzerOptions.timeLimitPerMethod do
                        traceFuzzing "Start fuzzing iteration"
                        let results = fuzzBatch typeSolverSeed rnd methodBase typeStorage
                        do! handleResults results

                        stopwatch.Stop()
                        currentMillisecondsElapsed <- currentMillisecondsElapsed + int stopwatch.ElapsedMilliseconds

                    printStatistics method
                | None -> traceFuzzing "Generics solving failed"
            with
                | :? InsufficientInformationException as e ->
                    errorFuzzing $"Insufficient information: {e.Message}\nStack trace:\n{e.StackTrace}"
                | :? NotImplementedException as e ->
                    errorFuzzing $"Not implemented: {e.Message}\nStack trace:\n{e.StackTrace}"
        }

    member this.SetOutputDirectory dir =
        currentOutputDir <- dir
