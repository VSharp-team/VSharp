namespace VSharp.System

open global.System
open VSharp
open VSharp.Core

module internal Unsafe =

    [<Implements("System.Void* Internal.Runtime.CompilerServices.Unsafe.AsPointer(System.RuntimeType&)")>]
    val internal AsPointer : state -> term list -> term * state
