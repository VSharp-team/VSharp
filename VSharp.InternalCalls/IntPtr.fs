namespace VSharp.System

open global.System
open VSharp
open VSharp.Core

// ------------------------------ System.IntPtr --------------------------------

module IntPtr =

    let private ctor (state : state) this term : (term * state) list =
        let ptr = MakeIntPtr term state
        Memory.WriteSafe state this ptr |> List.map (withFst Nop)

    let internal ctorFromInt (state : state) (args : term list) : (term * state) list =
        assert(List.length args = 2)
        let this, intTerm = List.item 0 args, List.item 1 args
        ctor state this intTerm

    let internal ctorFromPtr (state : state) (args : term list) : (term * state) list =
        assert(List.length args = 2)
        let this, ptrTerm = List.item 0 args, List.item 1 args
        ctor state this ptrTerm

    let internal ctorFromLong (state : state) (args : term list) : (term * state) list =
        assert(List.length args = 2)
        let this, intTerm = List.item 0 args, List.item 1 args
        ctor state this intTerm
