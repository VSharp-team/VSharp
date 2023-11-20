open System.Diagnostics
open System.IO
open System.Reflection
open VSharp.CSharpUtils
open VSharp.Explorer
open VSharp.Test
open VSharp.TestRenderer
open VSharp.TestRunner


type FuzzerTestRunner (assemblyName: string, outputDir: string) =

    let outputDirInfo = DirectoryInfo(outputDir)

    let methodsToTest =
        let testAssembly = Assembly.Load(assemblyName)
        let testTypes = testAssembly.GetTypes()

        testTypes
        |> Array.map (fun x -> x.GetMethods())
        |> Array.concat
        |> Array.filter (fun x -> x.GetCustomAttribute<TestSvmAttribute>() <> null)
        |> Array.map (fun x -> x :> MethodBase)
        |> Array.filter (fun x -> x.DeclaringType.Name = "Arithmetics")
        |> Array.filter (fun x -> x.Name = "ImpossibleBug")

    let runFuzzer () =

        let fuzzerOptions =
            {
                isolation = fuzzerIsolation.Process
                coverageZone = coverageZone.ModuleZone
            }

        let explorerOptions =
            {
                timeout = TimeSpanBuilders.Infinite
                outputDirectory = outputDirInfo
                explorationModeOptions = Fuzzing fuzzerOptions
            }

        let reporter =
            {
                new IReporter with
                    member this.ReportCrash ex = failwith $"Crash: {ex}"
                    member this.ReportException _ = ()
                    member this.ReportFinished _ = ()
                    member this.ReportIIE _ = ()
                    member this.ReportInternalFail method ex = failwith $"InternalFail: {method.Name}\n{ex}"
            }

        let explorer = new Explorer(explorerOptions, reporter)
        explorer.StartExploration methodsToTest Seq.empty

    let renderTests () =
        let tests = outputDirInfo.EnumerateFiles("*.vst")
        Renderer.Render(tests, wrapErrors = true, singleFile = false, declaringType = null, outputDir = outputDirInfo)

    let reproduceTests () =
        let reproduced = TestRunner.ReproduceTests(outputDirInfo)
        if not reproduced then
            failwith "Reproducing tests failed"

    let reproduceRenderedTests () =
        let processInfo = ProcessStartInfo(
            WorkingDirectory = $"{outputDir}{Path.DirectorySeparatorChar}{assemblyName}.Tests",
            FileName = DotnetExecutablePath.ExecutablePath,
            Arguments = "test -c Release --filter \"TestCategory=Generated&TestCategory!=FatalError\""
        )
        let reproduceProcess = Process.Start(processInfo)
        reproduceProcess.WaitForExit()
        if reproduceProcess.ExitCode <> 0 then
            failwith "Reproducing rendered tests failed"

    let setupLogger () =
        let writer = new StreamWriter (File.OpenWrite $"{outputDir}{Path.DirectorySeparatorChar}fuzzerTests.log")
        VSharp.Logger.configureWriter writer
        VSharp.Logger.enableTag "Communication" VSharp.Logger.Trace

    let cleanupOutputDur () =
        if outputDirInfo.Exists then outputDirInfo.Delete(recursive = true)
        outputDirInfo.Create()

    member this.RunTests() =
        for m in methodsToTest do
            printfn $"{m.Name}"
        cleanupOutputDur ()
        setupLogger ()
        runFuzzer ()
        reproduceTests ()
        renderTests ()
        reproduceRenderedTests ()


[<EntryPoint>]
let main _ =
    DotnetExecutablePath.OverridePath(@"C:\Users\vikto\.dotnet\dotnet.exe")
    let testRunner = FuzzerTestRunner(
        assemblyName = "VSharp.Test",
        outputDir = $"{Directory.GetCurrentDirectory()}{Path.DirectorySeparatorChar}fuzzerTests"
    )
    testRunner.RunTests()
    0
