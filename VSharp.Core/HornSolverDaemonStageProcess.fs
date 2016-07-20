namespace VSharp.Core

open JetBrains.ReSharper.Feature.Services.Daemon
open JetBrains.ReSharper.Psi
open JetBrains.ReSharper.Psi.CSharp.Tree

type HornSolverDaemonStageProcess(daemonProcess : IDaemonProcess, file : ICSharpFile) =
    interface IDaemonStageProcess with
        member x.DaemonProcess with get() = daemonProcess
        member x.Execute(commiter) =
            ()