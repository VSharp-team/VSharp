namespace VSharp.System

open global.System
open VSharp
open VSharp.Core

// ------------------------------- mscorlib.System.Array -------------------------------

module internal String =

    let CtorOfCharArray state args =
        assert (List.length args = 2)
        let this, arrayRef = List.item 0 args, List.item 1 args
        BranchStatementsOnNull state arrayRef
            (fun state k -> k (Nop, state))
            (fun state k ->
                let states = Memory.StringCtorOfCharArray state arrayRef this
                match states with
                | [state] -> k (Nop, state)
                | _ -> __notImplemented__())
            id

    let GetLength (state : state) (args : term list) =
        assert(List.length args = 1)
        let length = Memory.StringLength state (List.head args)
        length, state

    let GetChars (state : state) (args : term list) =
        assert (List.length args = 2)
        let this, index = List.item 0 args, List.item 1 args
        Memory.ReadStringChar state this index, state
