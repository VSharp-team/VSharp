namespace VSharp.System

open global.System
open VSharp
open VSharp.Core
open VSharp.Interpreter

// ------------------------------- mscorelib.System.Array -------------------------------

module internal SystemArray =

    [<Implements("System.Int32 System.Array.GetLength(this, System.Int32)")>]
    val GetLength : state -> term list -> statementResult * state

    [<Implements("System.Int32 System.Array.GetRank(this)")>]
    val GetRank : state -> term list -> statementResult * state

    [<Implements("System.Int32 System.Array.get_Rank(this)")>]
    val get_Rank : state -> term list -> statementResult * state

    [<Implements("System.Int32 System.Array.get_Length(this)")>]
    val get_Length : state -> term list -> statementResult * state

    [<Implements("System.Int32 System.Array.GetLowerBound(this, System.Int32)")>]
    val GetLowerBound : state -> term list -> statementResult * state
