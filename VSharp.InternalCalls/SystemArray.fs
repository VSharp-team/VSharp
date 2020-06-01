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

    let rec private length state arrayRef =
        GuardedApplyExpression arrayRef (fun arrayRef ->
            match arrayRef.term with
            | HeapRef(_, typ) ->
                let rank = Types.RankOf typ
                assert (rank >= 1)
                let lengths = List.init rank (MakeNumber >> Memory.ArrayLengthByDimension state arrayRef)
                match lengths with
                | [l] -> l
                | l::ls -> List.fold Arithmetics.Mul l ls
                | _ -> __unreachable__()
            | _ -> internalfailf "getting array length: expected array heap reference, but got %O" arrayRef)

    let get_Length state args =
        assert(List.length args = 1)
        length state (List.head args), state
