namespace VSharp.System

open global.System
open VSharp
open VSharp.Core
open VSharp.Interpreter.IL

module internal ReadOnlySpan =

    val GetContentsRef : cilState -> term -> term
    val GetLength : cilState -> term -> term

    [<Implements("T& System.ReadOnlySpan`1[T].get_Item(this, System.Int32)")>]
    [<Implements("T& System.Span`1[T].get_Item(this, System.Int32)")>]
    val GetItem : IInterpreter -> cilState -> term list -> cilState list

    [<Implements("System.Void System.ReadOnlySpan`1[T]..ctor(this, System.Void*, System.Int32)")>]
    [<Implements("System.Void System.Span`1[T]..ctor(this, System.Void*, System.Int32)")>]
    [<Implements("System.Void System.ReadOnlySpan`1[T]..ctor(this, T&, System.Int32)")>]
    [<Implements("System.Void System.Span`1[T]..ctor(this, T&, System.Int32)")>]
    val CtorFromPtr : IInterpreter -> cilState -> term list -> cilState list

    [<Implements("System.Void System.ReadOnlySpan`1[T]..ctor(this, T[])")>]
    [<Implements("System.Void System.Span`1[T]..ctor(this, T[])")>]
    val CtorFromArray : IInterpreter -> cilState -> term list -> cilState list

    [<Implements("System.ReadOnlySpan`1[System.Char] System.MemoryExtensions.AsSpan(System.String)")>]
    [<Implements("System.ReadOnlySpan`1[System.Char] System.String.op_Implicit(System.String)")>]
    val CreateFromString : IInterpreter -> cilState -> term list -> cilState list
