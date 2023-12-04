namespace VSharp.System

open global.System
open VSharp.Core
open VSharp.Interpreter.IL
open VSharp.Interpreter.IL.CilStateOperations

// ------------------------------ mscorlib.System.Threading.Thread --------------------------------

module Thread =

    let internal GetFastDomainInternal (_ : state) (_ : term list) = NullRef typeof<AppDomain>

    let internal GetDomainInternal state args = GetFastDomainInternal state args

    let internal SpinWaitInternal (_ : state) (args : term list) =
        assert(List.length args = 1)
        Nop()

    let internal SpinOnce (_ : state) (args : term list) =
        assert(List.length args = 1)
        Nop()

    let internal Yield (_ : state) (args : term list) =
        assert(List.length args = 0)
        MakeBool true

    let internal SleepInternal (_ : state) (args : term list) =
        assert(List.length args = 1)
        Nop()

    let MonitorReliableEnter (interpreter : IInterpreter) (cilState : cilState) (args : term list) =
        assert(List.length args = 2)
        let obj, resultRef = args[0], args[1]
        let success cilState k =
            write cilState resultRef (True()) |> k
        BranchOnNullCIL cilState obj
            (interpreter.Raise interpreter.ArgumentNullException)
            success
            id

    let MonitorEnter (interpreter : IInterpreter) (cilState : cilState) (args : term list) =
        assert(List.length args = 1)
        let obj = List.head args
        BranchOnNullCIL cilState obj
            (interpreter.Raise interpreter.ArgumentNullException)
            (fun cilState k -> List.singleton cilState |> k)
            id

    let MonitorPulseAll (_ : IInterpreter) (cilState : cilState) (args : term list) =
        assert(List.length args = 1)
        List.singleton cilState

    let Initialize (_ : IInterpreter) (cilState : cilState) (args : term list) =
        assert(List.length args = 1)
        List.singleton cilState

    let WaitOne (_ : state) (_ : term list) =
        MakeBool true

    let WaitOneNoCheck (_ : state) (_ : term list) =
        MakeBool true

    let MonitorWait (interpreter : IInterpreter) (cilState : cilState) (args : term list) =
        assert(List.length args = 1)
        let obj = List.head args
        let nonNullCase cilState k =
            push (MakeBool true) cilState
            List.singleton cilState |> k
        BranchOnNullCIL cilState obj
            (interpreter.Raise interpreter.ArgumentNullException)
            nonNullCase
            id
