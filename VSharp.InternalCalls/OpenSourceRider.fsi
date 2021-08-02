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

    [<Implements("System.Boolean JetBrains.Diagnostics.LogEx.IsTraceEnabled(JetBrains.Diagnostics.ILog)")>]
    val internal isTraceEnabled : state -> term list -> term * state

//    [<Implements("System.Void System.Collections.Comparer..ctor(this, System.Globalization.CultureInfo)")>]
//    val internal ctor : state -> term list -> (term * state) list
