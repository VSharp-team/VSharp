namespace VSharp.System

open global.System
open VSharp
open VSharp.Core
open VSharp.Interpreter.IL
open VSharp.Interpreter.IL.CilState

// ------------------------------ mscorlib.System.Threading.Thread --------------------------------

module internal Thread =

    [<Implements("System.AppDomain System.Threading.Thread.GetFastDomainInternal()")>]
    val GetFastDomainInternal : state -> term list -> term

    [<Implements("System.AppDomain System.Threading.Thread.GetDomainInternal()")>]
    val GetDomainInternal : state -> term list -> term

    [<Implements("System.Void System.Threading.Thread.SpinWaitInternal(System.Int32)")>]
    val SpinWaitInternal : state -> term list -> term

    [<Implements("System.Void System.Threading.SpinWait.SpinOnce(this)")>]
    val SpinOnce : state -> term list -> term

    [<Implements("System.Boolean System.Threading.Thread.Yield()")>]
    val Yield : state -> term list -> term

    [<Implements("System.Void System.Threading.Thread.SleepInternal(System.Int32)")>]
    val SleepInternal : state -> term list -> term

    [<Implements("System.Void System.Threading.Monitor.ReliableEnter(System.Object, System.Boolean&)")>]
    val MonitorReliableEnter : IInterpreter -> cilState -> term list -> cilState list

    [<Implements("System.Void System.Threading.Monitor.Enter(System.Object)")>]
    val MonitorEnter : IInterpreter -> cilState -> term list -> cilState list

    [<Implements("System.Void System.Threading.Monitor.PulseAll(System.Object)")>]
    val MonitorPulseAll : IInterpreter -> cilState -> term list -> cilState list

    [<Implements("System.Void System.Threading.Thread.Initialize(this)")>]
    val Initialize : IInterpreter -> cilState -> term list -> cilState list

    [<Implements("System.Boolean System.Threading.WaitHandle.WaitOne(this)")>]
    val WaitOne : state -> term list -> term

    [<Implements("System.Boolean System.Threading.WaitHandle.WaitOneNoCheck(this, System.Int32)")>]
    val WaitOneNoCheck : state -> term list -> term

    [<Implements("System.Boolean System.Threading.Monitor.Wait(System.Object)")>]
    val MonitorWait : IInterpreter -> cilState -> term list -> cilState list

    [<Implements("System.Boolean System.Threading.Monitor.Wait(System.Object, System.Int32)")>]
    val MonitorWaitTimeout : IInterpreter -> cilState -> term list -> cilState list
