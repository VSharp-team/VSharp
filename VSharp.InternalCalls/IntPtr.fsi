namespace VSharp.System

open global.System
open VSharp
open VSharp.Core

module internal IntPtr =

    [<Implements("System.Void System.IntPtr..ctor(this, System.Int32)")>]
    val internal intPtrCtorFromInt : state -> term list -> (term * state) list

    [<Implements("System.Void System.IntPtr..ctor(this, System.Void*)")>]
    val internal intPtrCtorFromPtr : state -> term list -> (term * state) list

    [<Implements("System.Void System.IntPtr..ctor(this, System.Int64)")>]
    val internal intPtrCtorFromLong : state -> term list -> (term * state) list

    [<Implements("System.Void* System.IntPtr.ToPointer(this)")>]
    val internal intPtrToPointer : state -> term list -> term

module internal UIntPtr =

    [<Implements("System.Void System.UIntPtr..ctor(this, System.UInt32)")>]
    val internal uintPtrCtorFromUInt : state -> term list -> (term * state) list

    [<Implements("System.Void System.UIntPtr..ctor(this, System.Void*)")>]
    val internal uintPtrCtorFromPtr : state -> term list -> (term * state) list

    [<Implements("System.Void System.UIntPtr..ctor(this, System.UInt64)")>]
    val internal uintPtrCtorFromULong : state -> term list -> (term * state) list

    [<Implements("System.Void* System.UIntPtr.ToPointer(this)")>]
    val internal uintPtrToPointer : state -> term list -> term
