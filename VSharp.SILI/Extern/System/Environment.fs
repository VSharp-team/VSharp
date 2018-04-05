namespace VSharp.System

open global.System
open VSharp
open VSharp.Core

// ------------------------------- mscorlib.System.Environment -------------------------------

module Environment =

    let internal GetResourceFromDefault (state : state) (args : term list) =
        let result = "Getting resource strings currently not supported!"
        let reference, state = Memory.AllocateString result.Length result state
        Return reference, state
