namespace VSharp.System

open global.System
open VSharp
open VSharp.Core

// ------------------------------ mscorlib.System.Threading.Thread --------------------------------

module Thread =

    [<Implements("System.AppDomain System.Threading.Thread.GetFastDomainInternal()")>]
    val internal GetFastDomainInternal : state -> term list -> term

    [<Implements("System.AppDomain System.Threading.Thread.GetDomainInternal()")>]
    val internal GetDomainInternal : state -> term list -> term

    [<Implements("System.Void System.Threading.Thread.SpinWaitInternal(System.Int32)")>]
    val internal SpinWaitInternal : state -> term list -> term

    [<Implements("System.Void System.Threading.SpinWait.SpinOnce(this)")>]
    val internal SpinOnce : state -> term list -> term

    [<Implements("System.Boolean System.Threading.Thread.Yield()")>]
    val internal Yield : state -> term list -> term

    [<Implements("System.Void System.Threading.Thread.SleepInternal(System.Int32)")>]
    val internal SleepInternal : state -> term list -> term
