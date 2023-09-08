namespace VSharp.System

open global.System
open VSharp
open VSharp.Core
open VSharp.Interpreter.IL

module internal Diagnostics =

    [<Implements("System.Void System.Diagnostics.Debug.Assert(System.Boolean)")>]
    val DebugAssert : IInterpreter -> cilState -> term list -> cilState list
