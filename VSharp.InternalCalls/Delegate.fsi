namespace VSharp.System

open global.System
open VSharp
open VSharp.Core
open VSharp.Interpreter.IL

module internal Delegate =

    [<Implements("System.Delegate System.Delegate.Combine(System.Delegate, System.Delegate)")>]
    val DelegateCombine : IInterpreter -> cilState -> term list -> cilState list

    [<Implements("System.Delegate System.Delegate.Remove(System.Delegate, System.Delegate)")>]
    val DelegateRemove : IInterpreter -> cilState -> term list -> cilState list
