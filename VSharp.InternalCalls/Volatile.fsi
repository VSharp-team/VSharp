namespace VSharp.System

open global.System
open VSharp
open VSharp.Core

// ------------------------------ mscorlib.System.Threading.Volatile --------------------------------

module Volatile =

    [<Implements("T System.Threading.Volatile.Read(T&)")>]
    val internal Read : state -> term list -> term
