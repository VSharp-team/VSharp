namespace VSharp.System

open global.System
open VSharp
open VSharp.Core

// ------------------------------- mscorlib.System.Array -------------------------------

module internal String =

    let ctorOfCharArray (state : state) (args : term list) =
        Return <| __notImplemented__(), state
