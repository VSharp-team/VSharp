namespace VSharp.System

open global.System
open VSharp
open VSharp.Core

// ------------------------------- mscorlib.System.Array -------------------------------

module internal SystemArray =

    let GetRank (state : state) args =
        assert(List.length args = 1)
        List.head args |> Memory.ArrayRank state, state

    let get_Rank state args =
        GetRank state args

    let get_Length state args =
        assert(List.length args = 1)
        let getLengthFromRank arrayRef =
            let rank = Terms.MostConcreteTypeOfHeapRef state arrayRef |> Types.RankOf
            assert (rank >= 1)
            let lengths = List.init rank (MakeNumber >> Memory.ArrayLengthByDimension state arrayRef)
            match lengths with
            | [l] -> l
            | l::ls -> List.fold Arithmetics.Mul l ls
            | _ -> __unreachable__()
        GuardedApplyExpression (List.head args) getLengthFromRank, state
