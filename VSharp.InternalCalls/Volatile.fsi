namespace VSharp.System

open global.System
open VSharp
open VSharp.Core
open VSharp.Interpreter.IL

// ------------------------------ mscorlib.System.Threading.Volatile --------------------------------

module internal Volatile =

    [<Implements("T System.Threading.Volatile.Read(T&)")>]
    val Read : IInterpreter -> cilState -> term list -> cilState list

    [<Implements("System.Void System.Threading.Volatile.Write(T&, T)")>]
    val Write : IInterpreter -> cilState -> term list -> cilState list
