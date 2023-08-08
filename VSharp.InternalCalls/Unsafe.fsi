namespace VSharp.System

open global.System
open VSharp
open VSharp.Core

module internal Unsafe =

    [<Implements("System.Void* Internal.Runtime.CompilerServices.Unsafe.AsPointer(T&)")>]
    [<Implements("System.Void* System.Runtime.CompilerServices.Unsafe.AsPointer(T&)")>]
    val internal AsPointer : state -> term list -> term

    [<Implements("T Internal.Runtime.CompilerServices.Unsafe.As(System.Object)")>]
    [<Implements("T System.Runtime.CompilerServices.Unsafe.As(System.Object)")>]
    val internal ObjectAsT : state -> term list -> term

    [<Implements("T& Internal.Runtime.CompilerServices.Unsafe.AsRef(T&)")>]
    [<Implements("T& System.Runtime.CompilerServices.Unsafe.AsRef(T&)")>]
    val internal AsRef : state -> term list -> term

    [<Implements("TTo& Internal.Runtime.CompilerServices.Unsafe.As(TFrom&)")>]
    [<Implements("TTo& System.Runtime.CompilerServices.Unsafe.As(TFrom&)")>]
    val internal TFromAsTTo : state -> term list -> term

    [<Implements("T& Internal.Runtime.CompilerServices.Unsafe.NullRef()")>]
    [<Implements("T& System.Runtime.CompilerServices.Unsafe.NullRef()")>]
    val internal NullRef : state -> term list -> term

    [<Implements("System.Boolean Internal.Runtime.CompilerServices.Unsafe.IsNullRef(T&)")>]
    [<Implements("System.Boolean System.Runtime.CompilerServices.Unsafe.IsNullRef(T&)")>]
    val internal IsNullRef : state -> term list -> term

    [<Implements("T& Internal.Runtime.CompilerServices.Unsafe.AddByteOffset(T&, System.UIntPtr)")>]
    [<Implements("T& System.Runtime.CompilerServices.Unsafe.AddByteOffset(T&, System.UIntPtr)")>]
    [<Implements("T& Internal.Runtime.CompilerServices.Unsafe.AddByteOffset(T&, System.IntPtr)")>]
    [<Implements("T& System.Runtime.CompilerServices.Unsafe.AddByteOffset(T&, System.IntPtr)")>]
    val internal AddByteOffset : state -> term list -> term

    [<Implements("T& Internal.Runtime.CompilerServices.Unsafe.Add(T&, System.IntPtr)")>]
    [<Implements("T& System.Runtime.CompilerServices.Unsafe.Add(T&, System.IntPtr)")>]
    val internal AddIntPtr : state -> term list -> term

    [<Implements("T& Internal.Runtime.CompilerServices.Unsafe.Add(T&, System.Int32)")>]
    [<Implements("T& System.Runtime.CompilerServices.Unsafe.Add(T&, System.Int32)")>]
    val internal AddInt : state -> term list -> term

    [<Implements("T Internal.Runtime.CompilerServices.Unsafe.ReadUnaligned(System.Byte&)")>]
    [<Implements("T System.Runtime.CompilerServices.Unsafe.ReadUnaligned(System.Byte&)")>]
    val internal ReadUnaligned : state -> term list -> term

    [<Implements("System.Void Internal.Runtime.CompilerServices.Unsafe.WriteUnaligned(System.Byte&, T)")>]
    [<Implements("System.Void System.Runtime.CompilerServices.Unsafe.WriteUnaligned(System.Byte&, T)")>]
    val internal WriteUnaligned : state -> term list -> (term * state) list

    [<Implements("System.Int32 Internal.Runtime.CompilerServices.Unsafe.SizeOf()")>]
    [<Implements("System.Int32 System.Runtime.CompilerServices.Unsafe.SizeOf()")>]
    val internal SizeOf : state -> term list -> term

    [<Implements("System.Boolean Internal.Runtime.CompilerServices.Unsafe.AreSame(T&, T&)")>]
    [<Implements("System.Boolean System.Runtime.CompilerServices.Unsafe.AreSame(T&, T&)")>]
    val internal AreSame : state -> term list -> term

    [<Implements("System.Byte& System.Runtime.CompilerServices.RuntimeHelpers.GetRawData(System.Object)")>]
    [<Implements("System.Byte& Internal.Runtime.CompilerServices.RuntimeHelpers.GetRawData(System.Object)")>]
    val internal GetRawData : state -> term list -> term

    [<Implements("System.Void Internal.Runtime.CompilerServices.Unsafe.SkipInit(T&)")>]
    [<Implements("System.Void System.Runtime.CompilerServices.Unsafe.SkipInit(T&)")>]
    val internal SkipInit : state -> term list -> term
