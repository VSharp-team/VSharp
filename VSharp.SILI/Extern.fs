namespace VSharp

open System

// ------------------------------- mscorelib.System -------------------------------

module private System =

    module Array =
        [<Implements("System.Int32 System.Array.GetLength(this, System.Int32)")>]
        let GetLength (state : State.state) (args : Term list) (k : StatementResult * State.state -> 'a) =
            __notImplemented__()
