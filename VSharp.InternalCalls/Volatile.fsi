namespace VSharp.System

open global.System
open VSharp
open VSharp.Core

// ------------------------------ mscorlib.System.Threading.Volatile --------------------------------

module internal Volatile =

    [<Implements("T System.Threading.Volatile.Read(T&)")>]
    val Read : state -> term list -> term
