namespace VSharp.System

open global.System
open VSharp
open VSharp.Core

module internal Interlocked =

    let exchange state location value =
        let currentValue = Memory.Read state location
        let state' = Memory.Write state location value
        List.map (withFst currentValue) state'

    let compareExchange state location value compared =
        let currentValue = Memory.Read state location
        let state' =
            StatedConditionalExecutionAppendResults state
                (fun state k -> k (currentValue === compared, state))
                (fun state k -> k (Memory.Write state location value))
                (fun state k -> k (List.singleton state))
                id
        List.map (withFst currentValue) state'

    let genericCompareExchange (state : state) (args : term list) : (term * state) list =
        assert(List.length args = 4)
        let location, value, compared = args[1], args[2], args[3]
        compareExchange state location value compared

    let intCompareExchange (state : state) (args : term list) : (term * state) list =
        assert(List.length args = 3)
        let location, value, compared = args[0], args[1], args[2]
        compareExchange state location value compared

    let intPtrCompareExchange (state : state) (args : term list) : (term * state) list =
        assert(List.length args = 3)
        let location, value, compared = args[0], args[1], args[2]
        compareExchange state location value compared

    let genericExchange (state : state) (args : term list) : (term * state) list =
        assert(List.length args = 3)
        let location, value = args[1], args[2]
        exchange state location value

    let intExchange (state : state) (args : term list) : (term * state) list =
        assert(List.length args = 2)
        let location, value = args[0], args[1]
        exchange state location value

    let intExchangeAdd (state : state) (args : term list) : (term * state) list =
        assert(List.length args = 2)
        let location, value = args[0], args[1]
        exchange state location (Arithmetics.Add (Memory.Read state location) value)

    let longExchangeAdd (state : state) (args : term list) : (term * state) list =
        assert(List.length args = 2)
        let location, value = args[0], args[1]
        exchange state location (Arithmetics.Add (Memory.Read state location) value)

    let memoryBarrier (state : state) (args : term list) : term =
        Nop()
