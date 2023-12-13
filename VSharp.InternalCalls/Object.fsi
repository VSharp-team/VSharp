namespace VSharp.System

open global.System
open VSharp
open VSharp.Core
open VSharp.Interpreter.IL
open VSharp.Interpreter.IL.CilState

module internal Object =

    [<Implements("System.Object System.Object.MemberwiseClone(this)")>]
    val MemberwiseClone : IInterpreter -> cilState -> term list -> cilState list
