namespace VSharp.System

open global.System
open VSharp
open VSharp.Core
open VSharp.Interpreter.IL
open VSharp.Interpreter.IL.CilState

module internal List =

    [<Implements("System.Int32 System.Collections.Generic.List`1[T].get_Count(this)")>]
    [<Implements("System.Int32 System.Collections.Generic.LinkedList`1[T].get_Count(this)")>]
    val GetCount : state -> term list -> term

    [<Implements("System.Void System.Collections.Generic.List`1[T].Add(this, T)")>]
    [<Implements("System.Void System.Collections.Generic.LinkedList`1[T].Add(this, T)")>]
    val AddItem : IInterpreter -> cilState -> term list -> cilState list

    [<Implements("System.Void System.Collections.Generic.List`1[T].set_Item(this, System.Int32, T)")>]
    [<Implements("System.Void System.Collections.Generic.LinkedList`1[T].set_Item(this, System.Int32, T)")>]
    val SetItem : IInterpreter -> cilState -> term list -> cilState list

    [<Implements("T System.Collections.Generic.List`1[T].get_Item(this, System.Int32)")>]
    [<Implements("T System.Collections.Generic.LinkedList`1[T].get_Item(this, System.Int32)")>]
    val ReadByIndex : IInterpreter -> cilState -> term list -> cilState list

    [<Implements("T System.Collections.Generic.List`1[T].First(this)")>]
    [<Implements("T System.Collections.Generic.LinkedList`1[T].First(this)")>]
    val ReadFirst : IInterpreter -> cilState -> term list -> cilState list

    [<Implements("T System.Collections.Generic.List`1[T].Last(this)")>]
    [<Implements("T System.Collections.Generic.LinkedList`1[T].Last(this)")>]
    val ReadLast : IInterpreter -> cilState -> term list -> cilState list

    [<Implements("System.Void System.Collections.Generic.List`1[T].RemoveAt(this, System.Int32)")>]
    [<Implements("System.Void System.Collections.Generic.LinkedList`1[T].RemoveAt(this, System.Int32)")>]
    val RemoveAt : IInterpreter -> cilState -> term list -> cilState list

    [<Implements("System.Void System.Collections.Generic.List`1[T].Insert(this, System.Int32, T)")>]
    [<Implements("System.Void System.Collections.Generic.LinkedList`1[T].Insert(this, System.Int32, T)")>]
    val Insert : IInterpreter -> cilState -> term list -> cilState list

    [<Implements("System.Void System.Collections.Generic.List`1[T].CopyTo(this, System.Int32, T[], System.Int32, System.Int32)")>]
    [<Implements("System.Void System.Collections.Generic.LinkedList`1[T].CopyTo(this, System.Int32, T[], System.Int32, System.Int32)")>]
    val CopyToRange : IInterpreter -> cilState -> term list -> cilState list

    [<Implements("System.Void System.Collections.Generic.List`1[T].CopyTo(this, T[], System.Int32)")>]
    [<Implements("System.Void System.Collections.Generic.LinkedList`1[T].CopyTo(this, T[], System.Int32)")>]
    val CopyToFull : IInterpreter -> cilState -> term list -> cilState list

    [<Implements("System.Void System.Collections.Generic.List`1[T].CopyTo(this, T[])")>]
    [<Implements("System.Void System.Collections.Generic.LinkedList`1[T].CopyTo(this, T[])")>]
    val CopyToSimple : IInterpreter -> cilState -> term list -> cilState list
