namespace VSharp.System

open global.System
open VSharp
open VSharp.Core

module internal OpenSourceRider =

// ------------------------------- Logging -------------------------------

    [<Implements("System.Void JetBrains.Diagnostics.Log..cctor()")>]
    val internal logCCtor : state -> term list -> term * state

    [<Implements("JetBrains.Diagnostics.ILog JetBrains.Diagnostics.Log.GetLog()")>]
    val internal getLog : state -> term list -> term * state
