namespace VSharp.System

open global.System
open VSharp
open VSharp.Core

// ------------------------------- mscorlib.System.String -------------------------------

module internal String =

    [<Implements("System.Void System.String..ctor(this, System.Char[])")>]
    val CtorOfCharArray : state -> term list -> term * state

    [<Implements("System.Int32 System.String.get_Length(this)")>]
    val GetLength : state -> term list -> term * state
