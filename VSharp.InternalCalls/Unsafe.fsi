namespace VSharp.System

open global.System
open VSharp
open VSharp.Core
open VSharp.Interpreter.IL

module internal Unsafe =

    [<Implements("System.Void* Internal.Runtime.CompilerServices.Unsafe.AsPointer(T&)")>]
    [<Implements("System.Void* System.Runtime.CompilerServices.Unsafe.AsPointer(T&)")>]
    val AsPointer : state -> term list -> term

    [<Implements("T Internal.Runtime.CompilerServices.Unsafe.As(System.Object)")>]
    [<Implements("T System.Runtime.CompilerServices.Unsafe.As(System.Object)")>]
    val ObjectAsT : state -> term list -> term

    [<Implements("T& Internal.Runtime.CompilerServices.Unsafe.AsRef(T&)")>]
    [<Implements("T& System.Runtime.CompilerServices.Unsafe.AsRef(T&)")>]
    val AsRef : state -> term list -> term

    [<Implements("T& Internal.Runtime.CompilerServices.Unsafe.AsRef(System.Void*)")>]
    [<Implements("T& System.Runtime.CompilerServices.Unsafe.AsRef(System.Void*)")>]
    val PointerAsRef : state -> term list -> term

    [<Implements("TTo& Internal.Runtime.CompilerServices.Unsafe.As(TFrom&)")>]
    [<Implements("TTo& System.Runtime.CompilerServices.Unsafe.As(TFrom&)")>]
    val TFromAsTTo : state -> term list -> term

    [<Implements("T& Internal.Runtime.CompilerServices.Unsafe.NullRef()")>]
    [<Implements("T& System.Runtime.CompilerServices.Unsafe.NullRef()")>]
    val NullRef : state -> term list -> term

    [<Implements("System.Boolean Internal.Runtime.CompilerServices.Unsafe.IsNullRef(T&)")>]
    [<Implements("System.Boolean System.Runtime.CompilerServices.Unsafe.IsNullRef(T&)")>]
    val IsNullRef : state -> term list -> term

    [<Implements("T& Internal.Runtime.CompilerServices.Unsafe.AddByteOffset(T&, System.UIntPtr)")>]
    [<Implements("T& System.Runtime.CompilerServices.Unsafe.AddByteOffset(T&, System.UIntPtr)")>]
    [<Implements("T& Internal.Runtime.CompilerServices.Unsafe.AddByteOffset(T&, System.IntPtr)")>]
    [<Implements("T& System.Runtime.CompilerServices.Unsafe.AddByteOffset(T&, System.IntPtr)")>]
    [<Implements("System.Void* Internal.Runtime.CompilerServices.Unsafe.Add(System.Void*, System.Int32)")>]
    [<Implements("System.Void* System.Runtime.CompilerServices.Unsafe.Add(System.Void*, System.Int32)")>]
    val AddByteOffset : state -> term list -> term

    [<Implements("System.IntPtr Internal.Runtime.CompilerServices.Unsafe.ByteOffset(System.Byte&, System.Byte&)")>]
    [<Implements("System.IntPtr System.Runtime.CompilerServices.Unsafe.ByteOffset(System.Byte&, System.Byte&)")>]
    val ByteOffset : state -> term list -> term

    [<Implements("T& Internal.Runtime.CompilerServices.Unsafe.Add(T&, System.IntPtr)")>]
    [<Implements("T& System.Runtime.CompilerServices.Unsafe.Add(T&, System.IntPtr)")>]
    [<Implements("T& Internal.Runtime.CompilerServices.Unsafe.Add(T&, System.UIntPtr)")>]
    [<Implements("T& System.Runtime.CompilerServices.Unsafe.Add(T&, System.UIntPtr)")>]
    val AddIntPtr : state -> term list -> term

    [<Implements("T& Internal.Runtime.CompilerServices.Unsafe.Add(T&, System.Int32)")>]
    [<Implements("T& System.Runtime.CompilerServices.Unsafe.Add(T&, System.Int32)")>]
    val AddInt : state -> term list -> term

    [<Implements("T Internal.Runtime.CompilerServices.Unsafe.ReadUnaligned(System.Byte&)")>]
    [<Implements("T System.Runtime.CompilerServices.Unsafe.ReadUnaligned(System.Byte&)")>]
    [<Implements("T System.Runtime.CompilerServices.Unsafe.ReadUnaligned(System.Void*)")>]
    val ReadUnaligned : IInterpreter -> cilState -> term list -> cilState list

    [<Implements("System.Void Internal.Runtime.CompilerServices.Unsafe.WriteUnaligned(System.Byte&, T)")>]
    [<Implements("System.Void System.Runtime.CompilerServices.Unsafe.WriteUnaligned(System.Byte&, T)")>]
    [<Implements("System.Void System.Runtime.CompilerServices.Unsafe.WriteUnaligned(System.Void*, T)")>]
    val WriteUnalignedGeneric : IInterpreter -> cilState -> term list -> cilState list

    [<Implements("System.Void System.Runtime.CompilerServices.Unsafe.WriteUnaligned(System.Void*, System.UInt32)")>]
    val WriteUnaligned : IInterpreter -> cilState -> term list -> cilState list

    [<Implements("System.Int32 Internal.Runtime.CompilerServices.Unsafe.SizeOf()")>]
    [<Implements("System.Int32 System.Runtime.CompilerServices.Unsafe.SizeOf()")>]
    val SizeOf : state -> term list -> term

    [<Implements("System.Boolean Internal.Runtime.CompilerServices.Unsafe.AreSame(T&, T&)")>]
    [<Implements("System.Boolean System.Runtime.CompilerServices.Unsafe.AreSame(T&, T&)")>]
    val AreSame : state -> term list -> term

    [<Implements("System.Byte& System.Runtime.CompilerServices.RuntimeHelpers.GetRawData(System.Object)")>]
    [<Implements("System.Byte& Internal.Runtime.CompilerServices.RuntimeHelpers.GetRawData(System.Object)")>]
    val GetRawData : state -> term list -> term

    [<Implements("System.Void Internal.Runtime.CompilerServices.Unsafe.SkipInit(T&)")>]
    [<Implements("System.Void System.Runtime.CompilerServices.Unsafe.SkipInit(T&)")>]
    val SkipInit : state -> term list -> term
