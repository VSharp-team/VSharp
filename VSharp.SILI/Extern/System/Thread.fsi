namespace VSharp.System

open global.System
open VSharp
open VSharp.Core
open VSharp.Interpreter

// ------------------------------ mscorlib.System.Threading.Thread --------------------------------

module Thread =

    [<Implements("System.AppDomain System.Threading.Thread.GetFastDomainInternal()")>]
    val internal GetFastDomainInternal : state -> term list -> statementResult * state

    [<Implements("System.AppDomain System.Threading.Thread.GetDomainInternal()")>]
    val internal GetDomainInternal : (state -> term list -> statementResult * state)
