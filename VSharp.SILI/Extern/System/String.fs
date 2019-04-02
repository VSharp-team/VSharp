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

    let GetLength (state : state) (args : term list) =
        assert(List.length args = 1)
        let length, state = Memory.StringLength state (List.head args)
        ControlFlow.ThrowOrReturn length, state

    let GetHashCode (state : state) (args : term list) =
        assert(List.length args = 1)
        let hash, state = Memory.StringHashCode state (List.head args)
        ControlFlow.ThrowOrReturn hash, state

    let IsStringInterned state args =
        assert(List.length args = 2)
        let string, state = Memory.IsInternedString state (List.item 1 args)
        ControlFlow.ThrowOrReturn string, state

    let InternString state args =
        assert(List.length args = 2)
        let string, state = Memory.InternString state (List.item 1 args)
        ControlFlow.ThrowOrReturn string, state

    let InternalIsInterned state args =
        assert(List.length args = 1)
        let string, state = Memory.IsInternedString state (List.head args)
        ControlFlow.ThrowOrReturn string, state

    let InternalIntern state args =
        assert(List.length args = 1)
        let string, state = Memory.InternString state (List.head args)
        ControlFlow.ThrowOrReturn string, state
