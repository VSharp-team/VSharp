namespace VSharp.System

open global.System
open VSharp
open VSharp.Core

module internal IntPtr =

    [<Implements("System.Void System.IntPtr..ctor(this, System.Int32)")>]
    val internal ctor : state -> term list -> (term * state) list
