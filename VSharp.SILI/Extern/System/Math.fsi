namespace VSharp.System

open global.System
open VSharp
open VSharp.Core
open VSharp.Interpreter

// ------------------------------- mscorelib.System.Math -------------------------------

module internal Math =

    [<Implements("System.Double System.Math.Acos(System.Double)")>]
    val Acos : State -> Term list -> StatementResult * State

    [<Implements("System.Double System.Math.Asin(System.Double)")>]
    val Asin : State -> Term list -> StatementResult * State

    [<Implements("System.Double System.Math.Atan(System.Double)")>]
    val Atan : State -> Term list -> StatementResult * State

    [<Implements("System.Double System.Math.Atan2(System.Double, System.Double)")>]
    val Atan2 : State -> Term list -> StatementResult * State

    [<Implements("System.Double System.Math.Ceiling(System.Double)")>]
    val Ceiling : State -> Term list -> StatementResult * State

    [<Implements("System.Double System.Math.Cos(System.Double)")>]
    val Cos : State -> Term list -> StatementResult * State

    [<Implements("System.Double System.Math.Cosh(System.Double)")>]
    val Cosh : State -> Term list -> StatementResult * State

    [<Implements("System.Double System.Math.Floor(System.Double)")>]
    val Floor : State -> Term list -> StatementResult * State

    [<Implements("System.Double System.Math.Sin(System.Double)")>]
    val Sin : State -> Term list -> StatementResult * State

    [<Implements("System.Double System.Math.Tan(System.Double)")>]
    val Tan : State -> Term list -> StatementResult * State

    [<Implements("System.Double System.Math.Sinh(System.Double)")>]
    val Sinh : State -> Term list -> StatementResult * State

    [<Implements("System.Double System.Math.Tanh(System.Double)")>]
    val Tanh : State -> Term list -> StatementResult * State

    [<Implements("System.Double System.Math.Round(System.Double)")>]
    val Round : State -> Term list -> StatementResult * State

    [<Implements("System.Double System.Math.Sqrt(System.Double)")>]
    val Sqrt : State -> Term list -> StatementResult * State

    [<Implements("System.Double System.Math.Log(System.Double)")>]
    val Log : State -> Term list -> StatementResult * State

    [<Implements("System.Double System.Math.Log10(System.Double)")>]
    val Log10 : State -> Term list -> StatementResult * State

    [<Implements("System.Double System.Math.Exp(System.Double)")>]
    val Exp : State -> Term list -> StatementResult * State

    [<Implements("System.Double System.Math.Pow(System.Double, System.Double)")>]
    val Pow : State -> Term list -> StatementResult * State

    [<Implements("System.Double System.Math.Abs(System.Double)")>]
    val Abs : State -> Term list -> StatementResult * State

    [<Implements("System.Single System.Math.Abs(System.Single)")>]
    val AbsS : State -> Term list -> StatementResult * State
