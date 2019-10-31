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
        this, state

    let GetLength (state : state) (args : term list) =
        assert(List.length args = 1)
        let length, state = Memory.StringLength state (List.head args)
        length, state
