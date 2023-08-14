namespace VSharp.System

open global.System
open VSharp
open VSharp.Core

// ------------------------------- mscorlib.System.Environment -------------------------------

module Environment =

    [<Implements("System.String System.Environment.GetResourceFromDefault(System.String)")>]
    val internal GetResourceFromDefault : state -> term list -> term

    [<Implements("System.Int32 System.Environment.get_CurrentManagedThreadId()")>]
    val internal GetCurrentManagedThreadId : state -> term list -> term

    [<Implements("System.Int32 System.Threading.Thread.get_ManagedThreadId(this)")>]
    val internal GetManagedThreadId : state -> term list -> term

    [<Implements("System.Void System.Console.WriteLine(System.String)")>]
    val internal WriteLine : state -> term list -> term

    [<Implements("System.Boolean System.Console.get_IsOutputRedirected()")>]
    val internal get_IsOutputRedirected : state -> term list -> term

    [<Implements("System.Void System.Console.Clear()")>]
    val internal consoleClear : state -> term list -> term
