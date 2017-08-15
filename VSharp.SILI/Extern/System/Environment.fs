namespace VSharp.System

open global.System
open VSharp

// ------------------------------- mscorelib.System.Environment -------------------------------

module Environment =

    let GetResourceFromDefault (state : State.state) (args : Term list) =
        Return (Terms.MakeConcreteString "Getting resource strings currently not supported!"), state
