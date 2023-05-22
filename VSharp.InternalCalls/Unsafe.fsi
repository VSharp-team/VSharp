namespace VSharp.System

open global.System
open VSharp
open VSharp.Core

module internal Unsafe =

    [<Implements("System.Void* Internal.Runtime.CompilerServices.Unsafe.AsPointer(T&)")>]
    val internal AsPointer : state -> term list -> term

    [<Implements("T Internal.Runtime.CompilerServices.Unsafe.As(System.Object)")>]
    val internal ObjectAsT : state -> term list -> term

    [<Implements("T& Internal.Runtime.CompilerServices.Unsafe.AsRef(T&)")>]
    val internal AsRef : state -> term list -> term

    [<Implements("TTo& Internal.Runtime.CompilerServices.Unsafe.As(TFrom&)")>]
    val internal TFromAsTTo : state -> term list -> term

    [<Implements("T& Internal.Runtime.CompilerServices.Unsafe.NullRef()")>]
    val internal NullRef : state -> term list -> term

    [<Implements("System.Boolean Internal.Runtime.CompilerServices.Unsafe.IsNullRef(T&)")>]
    val internal IsNullRef : state -> term list -> term

    [<Implements("T& Internal.Runtime.CompilerServices.Unsafe.AddByteOffset(T&, System.UIntPtr)")>]
    val internal AddByteOffset : state -> term list -> term

    [<Implements("T& Internal.Runtime.CompilerServices.Unsafe.Add(T&, System.IntPtr)")>]
    val internal Add : state -> term list -> term

    [<Implements("T Internal.Runtime.CompilerServices.Unsafe.ReadUnaligned(System.Byte&)")>]
    val internal ReadUnaligned : state -> term list -> term

    [<Implements("System.Int32 Internal.Runtime.CompilerServices.Unsafe.SizeOf()")>]
    val internal SizeOf : state -> term list -> term

    [<Implements("System.Boolean Internal.Runtime.CompilerServices.Unsafe.AreSame(T&, T&)")>]
    val internal AreSame : state -> term list -> term
