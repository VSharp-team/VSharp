namespace VSharp.System

open global.System
open VSharp
open VSharp.Core

// ------------------------------- mscorlib.System.Math -------------------------------

module Environment =

    [<Implements("System.String System.Environment.GetResourceFromDefault(System.String)")>]
    val internal GetResourceFromDefault : state -> term list -> term

    [<Implements("System.Int32 System.Environment.get_CurrentManagedThreadId()")>]
    val internal GetCurrentManagedThreadId : state -> term list -> term

    [<Implements("System.Void System.Console.WriteLine(System.String)")>]
    val internal WriteLine : state -> term list -> term
