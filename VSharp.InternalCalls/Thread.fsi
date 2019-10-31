namespace VSharp.System

open global.System
open VSharp
open VSharp.Core

// ------------------------------ mscorlib.System.Threading.Thread --------------------------------

module Thread =

    [<Implements("System.AppDomain System.Threading.Thread.GetFastDomainInternal()")>]
    val internal GetFastDomainInternal : state -> term list -> term * state

    [<Implements("System.AppDomain System.Threading.Thread.GetDomainInternal()")>]
    val internal GetDomainInternal : (state -> term list -> term * state)
