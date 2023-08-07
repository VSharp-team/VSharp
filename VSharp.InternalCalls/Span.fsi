namespace VSharp.System

open global.System
open VSharp
open VSharp.Core

module internal ReadOnlySpan =

    val internal GetContentsRef : state -> term -> term
    val internal GetLength : state -> term -> term

    [<Implements("T& System.ReadOnlySpan`1[T].get_Item(this, System.Int32)")>]
    val internal GetItemFromReadOnlySpan : state -> term list -> term

    [<Implements("T& System.Span`1[T].get_Item(this, System.Int32)")>]
    val internal GetItemFromSpan : state -> term list -> term

    [<Implements("System.Void System.Span`1[T]..ctor(this, System.Void*, System.Int32)")>]
    val internal CtorFromPtrForSpan : state -> term list -> (term * state) list

    [<Implements("System.Void System.ReadOnlySpan`1[T]..ctor(this, System.Void*, System.Int32)")>]
    val internal CtorFromPtrForReadOnlySpan : state -> term list -> (term * state) list

    [<Implements("System.Void System.ReadOnlySpan`1[T]..ctor(this, T[])")>]
    val internal CtorFromArrayForReadOnlySpan : state -> term list -> (term * state) list
