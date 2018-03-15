namespace VSharp.System

open global.System
open VSharp
open VSharp.Core
open VSharp.Interpreter

// ------------------------------- mscorlib.System.Math -------------------------------

module Environment =

    [<Implements("System.String System.Environment.GetResourceFromDefault(System.String)")>]
    val internal GetResourceFromDefault : state -> term list -> statementResult * state
