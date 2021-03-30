namespace VSharp.System

open global.System
open VSharp
open VSharp.Core

// ------------------------------ System.IntPtr --------------------------------

module IntPtr =

    let internal ctor (state : state) (args : term list) : (term * state) list =
        match args with
        | this :: [term] ->
            let ptr = MakeIntPtr term state
            Memory.WriteSafe state this ptr |> List.map (withFst Nop)
        | _ -> __notImplemented__()
