namespace VSharp.System

open global.System
open VSharp
open VSharp.Core

// ------------------------------- mscorlib.System.Array -------------------------------

module internal SystemArray =

    let GetRank state args =
        assert(List.length args = 1)
        let array = Memory.Dereference state (List.head args)
        let result = GuardedApplyExpression array (fun term ->
            match term.term with
            | Array(d, _, _, _, _, _) -> d
            | term -> internalfailf "expected array, but %O got!" term)
        result, state

    let get_Rank state args =
        GetRank state args

    let get_Length state args =
        assert(List.length args = 1)
        let array = Memory.Dereference state (List.head args)
        Memory.ArrayLength array, state
