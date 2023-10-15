namespace VSharp.Fuzzer

open System
open System.Diagnostics
open System.IO
open System.Reflection
open System.Threading
open System.Threading.Tasks
open VSharp
open VSharp.Fuzzer.Communication
open Logger
open VSharp.Fuzzer.Communication.Contracts
open VSharp.Fuzzer.Communication.Services
open VSharp.Fuzzer.Startup

type private SiliStatisticConverter() =
    
    let methods = System.Collections.Generic.Dictionary<uint32, Method>()

    member this.GetMethod kek = methods[kek]

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

        // let restoreTest unhandledExceptionPath =
    //     let text = File.ReadAllText(unhandledExceptionPath).Split()
    //     assert (text.Length = 2)
    //     let threadId = text[0] |> int
    //     let exceptionName = text[1]
    //
    //     let invocationResult =
    //         match exceptionName with
    //         | "System.StackOverflowException" -> StackOverflowException() :> Exception |> Thrown
    //         | "System.AccessViolationException" -> AccessViolationException() :> Exception |> Thrown
    //         | _ -> internalfail $"Unexpected exception: {exceptionName}"
    //
    //     let executionData = executionInfo[threadId]
    //     let typeSolver = TypeSolver()
        // let method = Application.getMethod 
        // match typeSolver.SolveGenericMethodParameters method (generator.GenerateObject typeSolverRnd) with
        //         | Some(method, typeStorage) ->
        //             traceFuzzing "Generics successfully solved"
        //             stopwatch.Start()
        //             while currentMillisecondsElapsed < fuzzerOptions.timeLimitPerMethod do
        //                 traceFuzzing "Start fuzzing iteration"
        //                 let results = fuzzBatch typeSolverSeed rnd method typeStorage
        //                 do! handleResults results
        //                 stopwatch.Stop()
        //                 currentMillisecondsElapsed <- currentMillisecondsElapsed + int stopwatch.ElapsedMilliseconds
        //         | None -> traceFuzzing "Generics solving failed"
        //()
    member this.RestoreTest threadId exceptionName =
        ()

type Interactor (
    cancellationToken: CancellationToken,
    outputPath: string,
    saveStatistic,
    onCancelled: unit -> unit
    ) =

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

    let testRestorer = TestRestorer()
    let mutable fuzzerService = Unchecked.defaultof<IFuzzerService>
    let mutable fuzzerProcess = Unchecked.defaultof<Process>
    let mutable queued = System.Collections.Generic.Queue<MethodBase>()

    let startFuzzer () =
        fuzzerProcess <- startFuzzer fuzzerOptions fuzzerDeveloperOptions 
        fuzzerService <- connectFuzzerService ()
        waitFuzzerForReady fuzzerService

    let initialize () =
        startFuzzer ()
        cancellationToken.Register(fun () ->
            if not fuzzerProcess.HasExited  then
                fuzzerProcess.Kill ()
        ) |> ignore



    let handleExit () =
        let unhandledExceptionPath = $"{Directory.GetCurrentDirectory()}{Path.DirectorySeparatorChar}kek.info"
        if File.Exists(unhandledExceptionPath) then
            testRestorer.RestoreTest 0 0 //unhandledExceptionPath

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
                if queued.Count <> 0 then
                    do! fuzzNextMethod ()
                else
                    return ()
            } :> Task

        MasterProcessService(onTrackCoverage, onTrackExecutionSeed, onFinished)

    member this.StartFuzzing (targetAssemblyPath: string) (isolated: MethodBase seq) =

        initialize ()
        queued <- System.Collections.Generic.Queue<_>(isolated)

        let onFuzzerFailed () =
            task {
                handleExit ()
                startFuzzer ()
                do! setupFuzzer targetAssemblyPath
                do! fuzzNextMethod ()
            }

        let pollFuzzer () =
            task {
                if queued.Count = 0 then
                    return false
                else
                    if fuzzerProcess.HasExited then
                        do! onFuzzerFailed ()
                    return true
            }

        task {
            try
                do! fuzzNextMethod ()
                let mutable cont = false
                while cont do
                    do! Task.Delay(100)
                    let! contValue = pollFuzzer () 
                    cont <- contValue

                do! fuzzerService.Finish (UnitData())
                match fuzzerDeveloperOptions.sanitizersMode with
                | Disabled ->  do! fuzzerProcess.WaitForExitAsync cancellationToken
                | Enabled _ -> do fuzzerProcess.Kill()

            with :? TaskCanceledException -> onCancelled ()
        }
