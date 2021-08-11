namespace VSharp.System

open global.System
open VSharp
open VSharp.Core

module internal OpenSourceRider =

// ------------------------------- Logging -------------------------------

    // TODO: support logging
    let logCCtor (state : state) (_ : term list) : term * state =
        Nop, state

    let getLog (state : state) (_ : term list) : term * state =
        let logType = typeof<JetBrains.Diagnostics.Log>
        Types.FromDotNetType logType |> Memory.AllocateDefaultClass state
