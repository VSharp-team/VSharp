namespace VSharp.System

open global.System
open VSharp

// ------------------------------- mscorelib.System.Environment -------------------------------

module Environment =

    let internal GetResourceFromDefault (state : State.state) (args : Term list) =
        Return (MakeConcreteString "Getting resource strings currently not supported!"), state
