namespace VSharp.System

open global.System
open VSharp

// ------------------------------- mscorelib.System.Array -------------------------------

    module Array =

        [<Implements("System.Int32 System.Array.GetLength(this, System.Int32)")>]
        val GetLength : State.state -> Term list -> StatementResult * State.state

        [<Implements("System.Int32 System.Array.GetRank(this)")>]
        val GetRank : State.state -> Term list -> StatementResult * State.state

        [<Implements("System.Int32 System.Array.get_Length(this)")>]
        val get_Length : State.state -> Term list -> StatementResult * State.state
