namespace VSharp.System

open global.System
open VSharp
open VSharp.Core

// ------------------------------- mscorlib.System.Environment -------------------------------

module Environment =

    let internal GetResourceFromDefault (state : state) (args : term list) =
        Return (Concrete "Getting resource strings currently not supported!" Types.String), state
