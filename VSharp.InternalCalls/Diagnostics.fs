namespace VSharp.System

open global.System
open VSharp
open VSharp.Core
open VSharp.Interpreter.IL
open VSharp.Interpreter.IL.CilStateOperations

module internal Diagnostics =

    let DebugAssert (_ : IInterpreter) cilState (args : term list) =
        assert(List.length args = 1)
        let condition = List.head args
        StatedConditionalExecutionCIL cilState
            (fun state k -> k (condition, state))
            (fun cilState k -> (); k [cilState])
            (fun cilState k ->
                ErrorReporter.ReportFatalError cilState "Debug.Assert failed"
                k [])
            id
