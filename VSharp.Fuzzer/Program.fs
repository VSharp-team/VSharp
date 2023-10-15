open VSharp
open VSharp.Fuzzer

[<EntryPoint>]
let main _ =
    Startup.waitDebuggerAttached ()
    Startup.getLogPath () |> Logger.setupLogger
    let app = Fuzzer.Application(Startup.fuzzerOptionsFromEnv())
    app.Start().Wait()
    VSharp.CSharpUtils.TaskExtensions.RequestForgottenTaskExecution()
    0
