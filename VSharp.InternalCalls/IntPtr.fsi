namespace VSharp.System

open global.System
open VSharp
open VSharp.Core

module internal IntPtr =

    [<Implements("System.Void System.IntPtr..ctor(this, System.Int32)")>]
    val intPtrCtorFromInt : state -> term list -> (term * state) list

    [<Implements("System.Void System.IntPtr..ctor(this, System.Void*)")>]
    val intPtrCtorFromPtr : state -> term list -> (term * state) list

    [<Implements("System.Void System.IntPtr..ctor(this, System.Int64)")>]
    val intPtrCtorFromLong : state -> term list -> (term * state) list

    [<Implements("System.Void* System.IntPtr.ToPointer(this)")>]
    val intPtrToPointer : state -> term list -> term

module internal UIntPtr =

    [<Implements("System.Void System.UIntPtr..ctor(this, System.UInt32)")>]
    val uintPtrCtorFromUInt : state -> term list -> (term * state) list

    [<Implements("System.Void System.UIntPtr..ctor(this, System.Void*)")>]
    val uintPtrCtorFromPtr : state -> term list -> (term * state) list

    [<Implements("System.Void System.UIntPtr..ctor(this, System.UInt64)")>]
    val uintPtrCtorFromULong : state -> term list -> (term * state) list

    [<Implements("System.Void* System.UIntPtr.ToPointer(this)")>]
    val uintPtrToPointer : state -> term list -> term
