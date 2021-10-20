namespace VSharp.System

open global.System
open VSharp
open VSharp.Core

module internal Unsafe =

    [<Implements("System.Void* Internal.Runtime.CompilerServices.Unsafe.AsPointer(T&)")>]
    val internal AsPointer : state -> term list -> term

    [<Implements("T Internal.Runtime.CompilerServices.Unsafe.As(System.Object)")>]
    val internal As : state -> term list -> term

    [<Implements("T& Internal.Runtime.CompilerServices.Unsafe.NullRef()")>]
    val internal NullRef : state -> term list -> term

    [<Implements("System.Boolean Internal.Runtime.CompilerServices.Unsafe.IsNullRef(T&)")>]
    val internal IsNullRef : state -> term list -> term
