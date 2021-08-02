namespace VSharp.System

open global.System
open VSharp.Core

module internal PlatformHelper =

    let get_ProcessorCount (state : state) (_ : term list) : term * state =
        MakeNumber 1, state
