namespace VSharp.System

open global.System
open VSharp
open VSharp.Core

module internal Interlocked =

    let compareExchange (state : state) (args : term list) : (term * state) list =
        assert(List.length args = 4)
        let location, value, compared = args.[1], args.[2], args.[3]
        let currentValue = Memory.ReadSafe state location
        let state' =
            StatedConditionalExecutionAppendResults state
                (fun state k -> k (currentValue === compared, state))
                (fun state k -> k (Memory.WriteSafe state location value))
                (fun state k -> k (List.singleton state))
                id
        List.map (withFst currentValue) state'
