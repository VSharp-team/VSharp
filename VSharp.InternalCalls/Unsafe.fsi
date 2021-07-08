namespace VSharp.System

open global.System
open VSharp
open VSharp.Core

module internal Unsafe =

    [<Implements("System.Void* Internal.Runtime.CompilerServices.Unsafe.AsPointer(T&)")>]
    val internal AsPointer : state -> term list -> term * state

    [<Implements("T Internal.Runtime.CompilerServices.Unsafe.As(System.Object)")>]
    val internal As : state -> term list -> term * state
