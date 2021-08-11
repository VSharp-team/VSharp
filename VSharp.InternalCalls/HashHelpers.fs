namespace VSharp.System

open global.System
open VSharp
open VSharp.Core
open Arithmetics

module HashHelpers =

    // TODO: check, why original FastMod returns -1
    let FastMod (state : state) (args : term list) : term * state =
        assert(List.length args = 3)
        let hashCode, length = args.[0], args.[1]
        hashCode %%% length, state
