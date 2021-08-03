namespace VSharp.System

open global.System
open VSharp
open VSharp.Core

// ------------------------------- mscorlib.System.Math -------------------------------

module Environment =

    [<Implements("System.String System.Environment.GetResourceFromDefault(System.String)")>]
    val internal GetResourceFromDefault : state -> term list -> term * state

    [<Implements("System.Int32 System.Environment.get_CurrentManagedThreadId()")>]
    val internal GetCurrentManagedThreadId : state -> term list -> term * state
