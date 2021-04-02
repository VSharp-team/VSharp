namespace VSharp.System

open global.System
open VSharp
open VSharp.Core

// ------------------------------ System.IntPtr --------------------------------

module IntPtr =

    let internal ctor (state : state) (args : term list) : (term * state) list =
        assert(List.length args = 2)
        let this, term = List.item 0 args, List.item 1 args
        let ptr = MakeIntPtr term state
        Memory.WriteSafe state this ptr |> List.map (withFst Nop)
