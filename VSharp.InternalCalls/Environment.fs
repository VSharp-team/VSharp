namespace VSharp.System

open global.System
open VSharp
open VSharp.Core

// ------------------------------- mscorlib.System.Environment -------------------------------

module Environment =

    let internal GetResourceFromDefault (state : state) (_ : term list) =
        let reference, state = Memory.AllocateString "Getting resource strings currently not supported!" state
        reference, state

    let GetCurrentManagedThreadId  (state : state) (_ : term list) =
        MakeNumber 0, state
