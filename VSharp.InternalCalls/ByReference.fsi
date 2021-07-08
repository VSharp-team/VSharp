namespace VSharp.System

open global.System
open VSharp
open VSharp.Core

module internal ByReference =

    [<Implements("System.Void System.ByReference`1[T]..ctor(this, T&)")>]
    val internal ctor : state -> term list -> (term * state) list
