namespace VSharp.System

open global.System
open VSharp
open VSharp.Core
open VSharp.Interpreter

// ------------------------------- mscorelib.System.Math -------------------------------

module internal Math =

    [<Implements("System.Double System.Math.Acos(System.Double)")>]
    val Acos : state -> term list -> statementResult * state

    [<Implements("System.Double System.Math.Asin(System.Double)")>]
    val Asin : state -> term list -> statementResult * state

    [<Implements("System.Double System.Math.Atan(System.Double)")>]
    val Atan : state -> term list -> statementResult * state

    [<Implements("System.Double System.Math.Atan2(System.Double, System.Double)")>]
    val Atan2 : state -> term list -> statementResult * state

    [<Implements("System.Double System.Math.Ceiling(System.Double)")>]
    val Ceiling : state -> term list -> statementResult * state

    [<Implements("System.Double System.Math.Cos(System.Double)")>]
    val Cos : state -> term list -> statementResult * state

    [<Implements("System.Double System.Math.Cosh(System.Double)")>]
    val Cosh : state -> term list -> statementResult * state

    [<Implements("System.Double System.Math.Floor(System.Double)")>]
    val Floor : state -> term list -> statementResult * state

    [<Implements("System.Double System.Math.Sin(System.Double)")>]
    val Sin : state -> term list -> statementResult * state

    [<Implements("System.Double System.Math.Tan(System.Double)")>]
    val Tan : state -> term list -> statementResult * state

    [<Implements("System.Double System.Math.Sinh(System.Double)")>]
    val Sinh : state -> term list -> statementResult * state

    [<Implements("System.Double System.Math.Tanh(System.Double)")>]
    val Tanh : state -> term list -> statementResult * state

    [<Implements("System.Double System.Math.Round(System.Double)")>]
    val Round : state -> term list -> statementResult * state

    [<Implements("System.Double System.Math.Sqrt(System.Double)")>]
    val Sqrt : state -> term list -> statementResult * state

    [<Implements("System.Double System.Math.Log(System.Double)")>]
    val Log : state -> term list -> statementResult * state

    [<Implements("System.Double System.Math.Log10(System.Double)")>]
    val Log10 : state -> term list -> statementResult * state

    [<Implements("System.Double System.Math.Exp(System.Double)")>]
    val Exp : state -> term list -> statementResult * state

    [<Implements("System.Double System.Math.Pow(System.Double, System.Double)")>]
    val Pow : state -> term list -> statementResult * state

    [<Implements("System.Double System.Math.Abs(System.Double)")>]
    val Abs : state -> term list -> statementResult * state

    [<Implements("System.Single System.Math.Abs(System.Single)")>]
    val AbsS : state -> term list -> statementResult * state
