namespace VSharp.Concolic

open System.Diagnostics
open System.IO
open System.Reflection
open System.Runtime.InteropServices
open VSharp
open VSharp.Core


type ClientMachine(assembly : Assembly, state : state) =
    let extension =
        if RuntimeInformation.IsOSPlatform(OSPlatform.Windows) then ".dll"
        elif RuntimeInformation.IsOSPlatform(OSPlatform.Linux) then ".so"
        elif RuntimeInformation.IsOSPlatform(OSPlatform.OSX) then ".dylib"
        else __notImplemented__()
    let pathToClient = "libicsharpConcolic" + extension
    [<DefaultValue>] val mutable probes : probes
    [<DefaultValue>] val mutable instrumenter : Instrumenter
    let environment (assembly : Assembly) =
        let result = ProcessStartInfo()
        result.EnvironmentVariables.["CORECLR_PROFILER"] <- "{cf0d821e-299b-5307-a3d8-b283c03916dd}"
        result.EnvironmentVariables.["CORECLR_ENABLE_PROFILING"] <- "1"
        result.EnvironmentVariables.["CORECLR_PROFILER_PATH"] <- Directory.GetCurrentDirectory() + "/" + pathToClient
        result.WorkingDirectory <- Directory.GetCurrentDirectory()
        result.FileName <- "dotnet"
        result.UseShellExecute <- false
        result.RedirectStandardOutput <- true
        result.RedirectStandardError <- true
        result.Arguments <- assembly.Location
        result

    [<DefaultValue>] val mutable private communicator : Communicator
    member x.Spawn() =
        let env = environment assembly
        x.communicator <- new Communicator()
        let proc = Process.Start env
        proc.OutputDataReceived.Add <| fun args -> Logger.trace "CONCOLIC OUTPUT: %s" args.Data
        proc.ErrorDataReceived.Add <| fun args -> Logger.trace "CONCOLIC ERROR: %s" args.Data
        proc.BeginOutputReadLine()
        proc.BeginErrorReadLine()
        Logger.info "Successfully spawned pid %d, working dir \"%s\"" proc.Id env.WorkingDirectory
        if not <| x.communicator.Connect() then false
        else
            x.probes <- x.communicator.ReadProbes()
            x.instrumenter <- Instrumenter x.probes
            true

    member x.ExecCommand() =
        Logger.trace "Reading next command..."
        match x.communicator.ReadCommand() with
        | Instrument methodBody ->
            Logger.trace "Got instrument command! bytes count = %d, max stack size = %d, eh count = %d" methodBody.il.Length methodBody.properties.maxStackSize methodBody.ehs.Length
            let mb = x.instrumenter.Instrument x.communicator methodBody
//            let rewriter = ILRewriter methodBody
//            rewriter.Import()
//            let mb = rewriter.Export()
            x.communicator.SendMethodBody mb
            true
        | ExecuteInstruction _ ->
            Logger.trace "Got execute instruction command!"
            true
        | Terminate ->
            Logger.trace "Got terminate command!"
            false
