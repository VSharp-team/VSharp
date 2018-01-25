namespace VSharp.System

open global.System
open VSharp
open VSharp.Core
open VSharp.Interpreter

// ------------------------------- mscorelib.System.Array -------------------------------

module internal String =

    [<Implements("System.Void System.String..ctor(this, System.Char[])")>]
    val ctorOfCharArray : State -> Term list -> StatementResult * State
