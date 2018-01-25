namespace VSharp.System

open global.System
open VSharp
open VSharp.Core
open VSharp.Interpreter

// ------------------------------- mscorelib.System.Array -------------------------------

module internal SystemArray =

    [<Implements("System.Int32 System.Array.GetLength(this, System.Int32)")>]
    val GetLength : State -> Term list -> StatementResult * State

    [<Implements("System.Int32 System.Array.GetRank(this)")>]
    val GetRank : State -> Term list -> StatementResult * State

    [<Implements("System.Int32 System.Array.get_Rank(this)")>]
    val get_Rank : State -> Term list -> StatementResult * State

    [<Implements("System.Int32 System.Array.get_Length(this)")>]
    val get_Length : State -> Term list -> StatementResult * State

    [<Implements("System.Int32 System.Array.GetLowerBound(this, System.Int32)")>]
    val GetLowerBound : State -> Term list -> StatementResult * State
