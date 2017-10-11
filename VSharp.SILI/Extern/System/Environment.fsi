namespace VSharp.System

open global.System
open VSharp

// ------------------------------- mscorelib.System.Math -------------------------------

module Environment =

    [<Implements("System.String System.Environment.GetResourceFromDefault(System.String)")>]
    val internal GetResourceFromDefault : State.state -> Term list -> StatementResult * State.state
