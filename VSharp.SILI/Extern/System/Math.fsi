namespace VSharp.System

open global.System
open VSharp

// ------------------------------- mscorelib.System.Math -------------------------------

    module Math =

        [<Implements("System.Double System.Math.Log(System.Double)")>]
        val Log : State.state -> Term list -> StatementResult * State.state
