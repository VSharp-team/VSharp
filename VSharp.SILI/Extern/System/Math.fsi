namespace VSharp.System

open global.System
open VSharp

// ------------------------------- mscorelib.System.Math -------------------------------

module internal Math =

    [<Implements("System.Double System.Math.Acos(System.Double)")>]
    val Acos : State.state -> Term list -> StatementResult * State.state

    [<Implements("System.Double System.Math.Asin(System.Double)")>]
    val Asin : State.state -> Term list -> StatementResult * State.state

    [<Implements("System.Double System.Math.Atan(System.Double)")>]
    val Atan : State.state -> Term list -> StatementResult * State.state

    [<Implements("System.Double System.Math.Atan2(System.Double, System.Double)")>]
    val Atan2 : State.state -> Term list -> StatementResult * State.state

    [<Implements("System.Double System.Math.Ceiling(System.Double)")>]
    val Ceiling : State.state -> Term list -> StatementResult * State.state

    [<Implements("System.Double System.Math.Cos(System.Double)")>]
    val Cos : State.state -> Term list -> StatementResult * State.state

    [<Implements("System.Double System.Math.Cosh(System.Double)")>]
    val Cosh : State.state -> Term list -> StatementResult * State.state

    [<Implements("System.Double System.Math.Floor(System.Double)")>]
    val Floor : State.state -> Term list -> StatementResult * State.state

    [<Implements("System.Double System.Math.Sin(System.Double)")>]
    val Sin : State.state -> Term list -> StatementResult * State.state

    [<Implements("System.Double System.Math.Tan(System.Double)")>]
    val Tan : State.state -> Term list -> StatementResult * State.state

    [<Implements("System.Double System.Math.Sinh(System.Double)")>]
    val Sinh : State.state -> Term list -> StatementResult * State.state

    [<Implements("System.Double System.Math.Tanh(System.Double)")>]
    val Tanh : State.state -> Term list -> StatementResult * State.state

    [<Implements("System.Double System.Math.Round(System.Double)")>]
    val Round : State.state -> Term list -> StatementResult * State.state

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

    [<Implements("System.Double System.Math.Abs(System.Double)")>]
    val Abs : State.state -> Term list -> StatementResult * State.state

    [<Implements("System.Single System.Math.Abs(System.Single)")>]
    val AbsS : State.state -> Term list -> StatementResult * State.state
