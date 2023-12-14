namespace VSharp.Fuzzer

open System
open System.Reflection
open System.Threading
open VSharp.Fuzzer.Communication.Contracts
open VSharp.Fuzzer.Utils
open VSharp
open VSharp.CSharpUtils
open VSharp.Fuzzer.Communication
open Logger
open VSharp.Fuzzer.Communication.Services


type internal Application (fuzzerOptions: Startup.FuzzerOptions) =
    let fuzzerCancellationToken = new CancellationTokenSource()
    let coverageTool = CoverageTool()
    let masterProcessService = connectMasterProcessService ()
    let fuzzer = Fuzzer.Fuzzer(fuzzerOptions, masterProcessService, coverageTool)

    let mutable assembly = Unchecked.defaultof<Assembly>

    let createFuzzerService () =

        let onSetupAssembly pathToTargetAssembly =
            task {
                assembly <- AssemblyManager.LoadFromAssemblyPath pathToTargetAssembly
                traceFuzzing $"Target assembly was set to {assembly.FullName}"
            } |> withExceptionLogging

        let onFuzz moduleName methodToken =

            System.Threading.Tasks.Task.Run(fun () ->
                try
                    failIfNull assembly "onFuzz called before assembly initialization"

                    let methodBase = Reflection.resolveMethodBaseFromAssembly assembly moduleName methodToken
                    traceFuzzing $"Resolved MethodBase {methodToken}"

                    let method = Application.getMethod methodBase
                    traceFuzzing $"Resolved Method {methodToken}"

                    coverageTool.SetEntryMain assembly moduleName methodToken
                    traceFuzzing $"Was set entry main {moduleName} {methodToken}"

                    (fuzzer.AsyncFuzz method).Wait()
                    traceFuzzing $"Successfully fuzzed {moduleName} {methodToken}"

                    (masterProcessService.NotifyFinished (UnitData())).Wait()
                    traceFuzzing $"Notified master process: finished {moduleName} {methodToken}"
                with e -> errorFuzzing $"{e}"
            ).Forget()

            System.Threading.Tasks.Task.FromResult() :> System.Threading.Tasks.Task

        let onFinish () =
            task {
                traceCommunication "Fuzzer cancelled"
                fuzzerCancellationToken.Cancel()
            } |> withExceptionLogging

        let onSetupDir dir =
            task {
                fuzzer.SetOutputDirectory dir
            } |> withExceptionLogging

        FuzzerService(onFinish, onFuzz, onSetupAssembly, onSetupDir)

    member this.Start () =
        try
            let fuzzerService = createFuzzerService ()
            let fuzzerTask = startFuzzerService fuzzerService fuzzerCancellationToken.Token

            waitMasterProcessForReady masterProcessService

            traceFuzzing "Ready to work"
            fuzzerTask
        with e ->
            errorFuzzing $"Unhandled exception: {e}"
            exit 1
