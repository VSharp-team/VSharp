namespace VSharp.System

open global.System
open VSharp
open VSharp.Core
open VSharp.Interpreter.IL
open VSharp.Interpreter.IL.CilStateOperations

// ------------------------------ mscorlib.System.Threading.Volatile --------------------------------

module Volatile =

    let Read (_ : IInterpreter) (cilState : cilState) (args : term list) =
        assert(List.length args = 2)
        let ref = args[1]
        let value = read cilState ref
        push value cilState
        List.singleton cilState

    let Write (_ : IInterpreter) (cilState : cilState) (args : term list) =
        assert(List.length args = 3)
        let ref, value = args[1], args[2]
        write cilState ref value
