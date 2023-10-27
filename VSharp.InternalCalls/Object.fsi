namespace VSharp.System

open global.System
open VSharp
open VSharp.Core
open VSharp.Interpreter.IL

module internal Object =

    [<Implements("System.Object System.Object.MemberwiseClone(this)")>]
    val MemberwiseClone : IInterpreter -> cilState -> term list -> cilState list
