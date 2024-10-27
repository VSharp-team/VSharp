namespace VSharp.System

open global.System

open VSharp.Core
open VSharp.Interpreter.IL
open VSharp.Interpreter.IL.CilState

module internal Interlocked =

    let exchange (interpreter : IInterpreter) (cilState : cilState) location value =
        let exchange (cilState : cilState) k =
            let currentValue = cilState.Read location
            cilState.Write location value
            cilState.Push currentValue
            List.singleton cilState |> k
        interpreter.NpeOrInvoke cilState location exchange id

    let commonCompareExchange (interpreter : IInterpreter) (cilState : cilState) location value compared =
        let compareExchange (cilState : cilState) k =
            let currentValue = cilState.Read location
            let cilStates =
                cilState.StatedConditionalExecutionCIL
                    (fun cilState k -> k (currentValue === compared, cilState))
                    (fun cilState k -> k (cilState.Write location value; List.singleton cilState))
                    (fun cilState k -> k (List.singleton cilState))
                    id
            for cilState in cilStates do
                cilState.Push currentValue
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

    let commonExchangeAdd (interpreter : IInterpreter) (cilState : cilState) (args : term list) =
        assert(List.length args = 2)
        let location, value = args[0], args[1]
        let value = Arithmetics.Add (cilState.Read location) value
        exchange interpreter cilState location value

    let intExchangeAdd (interpreter : IInterpreter) cilState (args : term list) =
        commonExchangeAdd interpreter cilState args

    let longExchangeAdd (interpreter : IInterpreter) cilState (args : term list) =
        commonExchangeAdd interpreter cilState args

    let memoryBarrier (_ : state) (_ : term list) : term =
        Nop()
