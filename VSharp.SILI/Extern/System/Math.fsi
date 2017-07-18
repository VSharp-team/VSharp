namespace VSharp.System

open global.System
open VSharp

// ------------------------------- mscorelib.System.Math -------------------------------

    module Math =

        [<Implements("System.Double System.Math.Sqrt(System.Double)")>]
        val Sqrt : State.state -> Term list -> StatementResult * State.state

        [<Implements("System.Double System.Math.Log(System.Double)")>]
        val Log : State.state -> Term list -> StatementResult * State.state

        [<Implements("System.Double System.Math.Log10(System.Double)")>]
        val Log10 : State.state -> Term list -> StatementResult * State.state

        [<Implements("System.Double System.Math.Exp(System.Double)")>]
        val Exp : State.state -> Term list -> StatementResult * State.state

        [<Implements("System.Double System.Math.Pow(System.Double, System.Double)")>]
        val Pow : State.state -> Term list -> StatementResult * State.state
