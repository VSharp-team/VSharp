namespace VSharp.System

open global.System
open VSharp
open VSharp.Core
open VSharp.Interpreter.IL
open VSharp.Interpreter.IL.CilStateOperations

module internal Interlocked =

    let exchange (interpreter : IInterpreter) cilState location value =
        let exchange cilState k =
            let currentValue = read cilState location
            let cilStates = write cilState location value
            for cilState in cilStates do
                push currentValue cilState
            k cilStates
        interpreter.NpeOrInvoke cilState location exchange id

    let commonCompareExchange (interpreter : IInterpreter) cilState location value compared =
        let compareExchange cilState k =
            let currentValue = read cilState location
            let cilStates =
                StatedConditionalExecutionCIL cilState
                    (fun cilState k -> k (currentValue === compared, cilState))
                    (fun cilState k -> k (write cilState location value))
                    (fun cilState k -> k (List.singleton cilState))
                    id
            for cilState in cilStates do
                push currentValue cilState
            k cilStates
        interpreter.NpeOrInvoke cilState location compareExchange id

    let genericCompareExchange (interpreter : IInterpreter) cilState (args : term list) =
        assert(List.length args = 4)
        let location, value, compared = args[1], args[2], args[3]
        commonCompareExchange interpreter cilState location value compared

    let compareExchange (interpreter : IInterpreter) cilState (args : term list) =
        assert(List.length args = 3)
        let location, value, compared = args[0], args[1], args[2]
        commonCompareExchange interpreter cilState location value compared

    let genericExchange (interpreter : IInterpreter) cilState (args : term list) =
        assert(List.length args = 3)
        let location, value = args[1], args[2]
        exchange interpreter cilState location value

    let intPtrExchange (interpreter : IInterpreter) cilState (args : term list) =
        assert(List.length args = 2)
        let location, value = args[0], args[1]
        exchange interpreter cilState location value

    let intExchange (interpreter : IInterpreter) cilState (args : term list) =
        assert(List.length args = 2)
        let location, value = args[0], args[1]
        exchange interpreter cilState location value

    let commonExchangeAdd (interpreter : IInterpreter) cilState (args : term list) =
        assert(List.length args = 2)
        let location, value = args[0], args[1]
        let value = Arithmetics.Add (read cilState location) value
        exchange interpreter cilState location value

    let intExchangeAdd (interpreter : IInterpreter) cilState (args : term list) =
        commonExchangeAdd interpreter cilState args

    let longExchangeAdd (interpreter : IInterpreter) cilState (args : term list) =
        commonExchangeAdd interpreter cilState args

    let memoryBarrier (_ : state) (_ : term list) : term =
        Nop()
