namespace VSharp.System

open global.System
open VSharp
open VSharp.Core

// ------------------------------- mscorlib.System.Array -------------------------------

module internal String =

    let CtorOfCharArray state args =
        assert (List.length args = 2)
        let this, array = List.item 0 args, List.item 1 args
        let string, state = Memory.StringCtorOfCharArray state this array
        let state = Memory.Mutate state this string |> snd
        ControlFlow.ThrowOrReturn this, state
