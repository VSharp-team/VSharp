namespace VSharp.System

open global.System
open VSharp.Core

// ------------------------------- mscorlib.System.Environment -------------------------------

module Environment =

    let internal GetResourceFromDefault (state : state) (_ : term list) =
        Memory.AllocateString "Getting resource strings currently not supported!" state

    let GetCurrentManagedThreadId  (_ : state) (_ : term list) =
        MakeNumber 0

    let internal WriteLine (_ : state) (args : term list) =
        assert(List.length args = 1)
        Nop
