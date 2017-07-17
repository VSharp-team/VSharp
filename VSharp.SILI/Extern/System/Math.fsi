namespace VSharp.System

open global.System
open VSharp

// ------------------------------- mscorelib.System.Math -------------------------------

module Math =
    [<Implements("System.Int32 System.Math.Log(this)")>]
        val Log : (State.state -> Term list -> StatementResult * State.state)
