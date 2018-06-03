namespace VSharp.System

open global.System
open VSharp
open VSharp.Core

// ------------------------------- mscorlib.System.Environment -------------------------------

module Environment =

    let internal GetResourceFromDefault (state : state) (args : term list) =
        let reference, state = Memory.AllocateString "Getting resource strings currently not supported!" state
        Return reference, state
