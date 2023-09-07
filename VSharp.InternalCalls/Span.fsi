namespace VSharp.System

open global.System
open VSharp
open VSharp.Core

module internal ReadOnlySpan =

    val GetContentsRef : state -> term -> term
    val GetLength : state -> term -> term

    [<Implements("T& System.ReadOnlySpan`1[T].get_Item(this, System.Int32)")>]
    val GetItemFromReadOnlySpan : state -> term list -> term

    [<Implements("T& System.Span`1[T].get_Item(this, System.Int32)")>]
    val GetItemFromSpan : state -> term list -> term

    [<Implements("System.Void System.Span`1[T]..ctor(this, System.Void*, System.Int32)")>]
    val CtorFromPtrForSpan : state -> term list -> (term * state) list

    [<Implements("System.Void System.ReadOnlySpan`1[T]..ctor(this, System.Void*, System.Int32)")>]
    val CtorFromPtrForReadOnlySpan : state -> term list -> (term * state) list

    [<Implements("System.Void System.ReadOnlySpan`1[T]..ctor(this, T[])")>]
    val CtorFromArrayForReadOnlySpan : state -> term list -> (term * state) list

    [<Implements("System.ReadOnlySpan`1[System.Char] System.String.op_Implicit(System.String)")>]
    val ReadOnlySpanCreateFromString : state -> term list -> term
