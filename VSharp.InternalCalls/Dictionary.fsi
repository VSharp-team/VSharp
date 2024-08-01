namespace VSharp.System

open global.System
open VSharp
open VSharp.Core
open VSharp.Interpreter.IL
open VSharp.Interpreter.IL.CilState

module internal Dictionary =

    [<Implements("System.Int32 System.Collections.Generic.Dictionary`2[TKey,TValue].get_Count(this)")>]
    val GetCount : state -> term list -> term

    [<Implements("System.Boolean System.Collections.Generic.Dictionary`2[TKey,TValue].ContainsKey(this, TKey)")>]
    val IsContainsKey : IInterpreter -> cilState -> term list -> cilState list

    [<Implements("TValue System.Collections.Generic.Dictionary`2[TKey,TValue].get_Item(this, TKey)")>]
    val GetItem : IInterpreter -> cilState -> term list -> cilState list

    [<Implements("System.Void System.Collections.Generic.Dictionary`2[TKey,TValue].set_Item(this, TKey, TValue)")>]
    val SetItem : IInterpreter -> cilState -> term list -> cilState list

    [<Implements("System.Void System.Collections.Generic.Dictionary`2[TKey,TValue].Add(this, TKey, TValue)")>]
    val AddElement : IInterpreter -> cilState -> term list -> cilState list
