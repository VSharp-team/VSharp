module VSharp.Fuzzer.Startup

open System
open VSharp
open VSharp.CSharpUtils
open VSharp.CoverageTool
open VSharp.Utils.EnvironmentUtils
open VSharp.Fuzzer.Utils

type internal FuzzerOptions = {
    initialSeed: int
    timeLimitPerMethod: int
    arrayMaxSize: int
    stringMaxSize: int
}

type internal SanitizersMode =
    | Disabled
    | Enabled of string

type internal FuzzerDeveloperOptions = {
    logPath: string
    redirectStdout: bool
    redirectStderr: bool
    waitDebuggerAttachedFuzzer: bool
    waitDebuggerAttachedCoverageTool: bool
    waitDebuggerAttachedOnAssertCoverageTool: bool
    sanitizersMode: SanitizersMode
}

let private optionalFromEnv name =
    let value = Environment.GetEnvironmentVariable(name)
    if value = null then
        None
    else
        Some value

let private fromEnv name =
    match optionalFromEnv name with
    | Some value -> value
    | None -> Prelude.internalfail $"Required env var[{name}] not specified"

let private coreclrProfiler = "{2800fea6-9667-4b42-a2b6-45dc98e77e9e}"
let private coreclrProfilerPath = $"{IO.Directory.GetCurrentDirectory()}{System.IO.Path.DirectorySeparatorChar}libvsharpCoverage{getLibExtension ()}"
let private enabled = "1"

let internal fuzzerOptionsFromEnv () =
    {
        initialSeed = fromEnv "INITIAL_SEED" |> int
        timeLimitPerMethod = fromEnv "TIME_LIMIT" |> int
        arrayMaxSize = fromEnv "ARRAY_MAX_SIZE" |> int
        stringMaxSize = fromEnv "STRING_MAX_SIZE" |> int
    }

let internal getLogPath () =
    fromEnv "LOG_PATH"

let internal waitDebuggerAttached () =
    let value = Environment.GetEnvironmentVariable("WAIT_DEBUGGER_ATTACHED_FUZZER")
    if value = enabled then
        while not Diagnostics.Debugger.IsAttached do ()


let internal startFuzzer options developerOptions =
    let info = Diagnostics.ProcessStartInfo()
    info.WorkingDirectory <- IO.Directory.GetCurrentDirectory()
    info.FileName <- DotnetExecutablePath.ExecutablePath
    info.Arguments <- "VSharp.Fuzzer.dll"

    info.EnvironmentVariables["LOG_PATH"] <- System.IO.Directory.GetCurrentDirectory()
    info.EnvironmentVariables["INITIAL_SEED"] <- options.initialSeed |> string
    info.EnvironmentVariables["TIME_LIMIT"] <- options.timeLimitPerMethod |> string
    info.EnvironmentVariables["ARRAY_MAX_SIZE"] <- options.arrayMaxSize |> string
    info.EnvironmentVariables["STRING_MAX_SIZE"] <- options.stringMaxSize |> string

    if developerOptions.redirectStderr then
        info.RedirectStandardError <- true
    if developerOptions.redirectStdout then
        info.RedirectStandardOutput <- true

    let proc = System.Diagnostics.Process.Start info

    let stderrTag = "Fuzzer STDERR"
    let stdoutTag = "Fuzzer STDOUT"

    if developerOptions.redirectStderr then
        Logger.enableTag stderrTag Logger.Info
        proc.ErrorDataReceived.Add (fun x -> Logger.infoWithTag stderrTag $"{x.Data}")
        proc.BeginErrorReadLine ()
    if developerOptions.redirectStdout then
        Logger.enableTag stdoutTag Logger.Info
        proc.OutputDataReceived.Add (fun x -> Logger.infoWithTag stdoutTag $"{x.Data}")
        proc.BeginOutputReadLine ()

    if
        developerOptions.waitDebuggerAttachedFuzzer
        || developerOptions.waitDebuggerAttachedCoverageTool
        || developerOptions.waitDebuggerAttachedOnAssertCoverageTool then
        Logger.warning $"One of \"Wait debugger\" options is enabled, you may attach to process by pid {proc.Id}"

    if proc.HasExited then
        failwith "Cannot start fuzzer process"

    Logger.traceCommunication "Fuzzer process started"
    proc
