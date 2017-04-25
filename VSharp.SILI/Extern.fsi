namespace VSharp

open System

// ------------------------------- mscorelib.System -------------------------------

module private System =

    module Array =

        [<Implements("System.Int32 System.Array.GetLength(this, System.Int32)")>]
        val GetLength : State.state -> Term list -> StatementResult * State.state

        [<Implements("System.Int32 System.Array.GetRank(this)")>]
        val GetRank : State.state -> Term list -> StatementResult * State.state
