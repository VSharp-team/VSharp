namespace VSharp.System

open global.System
open VSharp
open VSharp.Core

module internal OpenSourceRider =

// ------------------------------- Logging -------------------------------

    [<Implements("System.Void JetBrains.Diagnostics.Log..cctor()")>]
    val internal logCCtor : state -> term list -> term * state
