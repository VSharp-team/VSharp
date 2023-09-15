namespace VSharp.System

open global.System
open VSharp
open VSharp.Core
open VSharp.Interpreter.IL

module internal Buffer =

    val CommonMemmove : cilState -> term -> term option -> term -> term option -> term -> cilState list

    [<Implements("System.Void System.Buffer.Memmove(T&, T&, System.UIntPtr)")>]
    val GenericMemmove : IInterpreter -> cilState -> term list -> cilState list

    [<Implements("System.Void System.Buffer.Memmove(System.Byte&, System.Byte&, System.UIntPtr)")>]
    val ByteMemmove : IInterpreter -> cilState -> term list -> cilState list

    [<Implements("System.Void System.Buffer.MemoryCopy(System.Void*, System.Void*, System.Int64, System.Int64)")>]
    val MemoryCopy : IInterpreter -> cilState -> term list -> cilState list
