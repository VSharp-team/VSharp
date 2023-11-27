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

    let Fail (_ : IInterpreter) cilState (args : term list) =
        assert(List.length args = 1)
        ErrorReporter.ReportFatalError cilState "System.Diagnostics.Debug.Fail called"
        [cilState]

    let FailWithDetailedMessage (_ : IInterpreter) cilState (args : term list) =
        assert(List.length args = 2)
        ErrorReporter.ReportFatalError cilState "System.Diagnostics.Debug.Fail called"
        [cilState]
