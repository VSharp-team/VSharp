namespace VSharp.System

open global.System
open VSharp
open VSharp.Core

module internal Buffer =

    [<Implements("System.Void System.Buffer.Memmove(T&, T&, System.UIntPtr)")>]
    val internal Memmove : state -> term list -> term
