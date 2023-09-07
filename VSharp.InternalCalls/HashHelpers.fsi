namespace VSharp.System

open global.System
open VSharp
open VSharp.Core
open VSharp.Interpreter.IL

module internal HashHelpers =

    [<Implements("System.UInt32 System.Collections.HashHelpers.FastMod(System.UInt32, System.UInt32, System.UInt64)")>]
    val FastMod : IInterpreter -> cilState -> term list -> cilState list
