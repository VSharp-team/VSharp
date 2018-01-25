namespace VSharp.System

open global.System
open VSharp
open VSharp.Core

// ------------------------------- mscorelib.System.Array -------------------------------

module internal String =

    let ctorOfCharArray (state : State) (args : Term list) =
        Return <| __notImplemented__(), state
