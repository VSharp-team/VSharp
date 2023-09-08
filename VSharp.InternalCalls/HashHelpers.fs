namespace VSharp.System

open global.System
open VSharp
open VSharp.Core
open VSharp.Interpreter.IL
open VSharp.Interpreter.IL.CilStateOperations

module internal HashHelpers =

    let FastMod (i : IInterpreter) cilState (args : term list) =
        assert(List.length args = 3)
        let left, right = args[0], args[1]
        let validCase cilState k =
            let leftType = TypeOf left
            let rightType = TypeOf right
            let result =
                if TypeUtils.isUnsigned leftType || TypeUtils.isUnsigned rightType then
                    Arithmetics.RemUn left right
                else Arithmetics.Rem left right
            push result cilState
            k [cilState]
        StatedConditionalExecutionCIL cilState
            (fun state k -> k (right === MakeNumber 0, state))
            (i.Raise i.DivideByZeroException)
            validCase
            id
