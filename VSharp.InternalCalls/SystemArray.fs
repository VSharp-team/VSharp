namespace VSharp.System

open global.System
open VSharp
open VSharp.Core

// ------------------------------- mscorlib.System.Array -------------------------------

module internal SystemArray =

    let GetRank (state : state) args =
        assert(List.length args = 1)
        List.head args |> Memory.ArrayRank state

    let get_Rank state args =
        GetRank state args

    let get_Length state args =
        assert(List.length args = 1)
        let getLengthFromRank arrayRef =
            let rank = Terms.MostConcreteTypeOfRef state arrayRef |> Types.RankOf
            assert (rank >= 1)
            let lengths = List.init rank (MakeNumber >> Memory.ArrayLengthByDimension state arrayRef)
            match lengths with
            | [l] -> l
            | l::ls -> List.fold Arithmetics.Mul l ls
            | _ -> __unreachable__()
        GuardedApplyExpression (List.head args) getLengthFromRank

    let ContainsChar (state : state) args =
        assert(List.length args = 3)
        let this, char = args.[0], args.[2]
        match this.term with
        | HeapRef({term = ConcreteHeapAddress _}, _) ->
            let checkOneElement acc i =
                let index = Concrete i Types.IndexType
                let elem = Memory.ReadArrayIndex state this [index] None
                acc ||| (elem === char)
            let length = Memory.ArrayLengthByDimension state this (MakeNumber 0)
            match length.term with
            | Concrete(obj, _) ->
                let length = obj :?> int
                let indices = List.init length id
                List.fold checkOneElement (False()) indices
            | _ -> __unreachable__()
        | _ -> __insufficientInformation__ "Contains works only for concrete address arrays"

    let GetCount (state : state) (args : term list) =
        assert(List.length args = 2)
        let this = List.head args
        get_Length state [this]

    let GetItem (state : state) (args : term list) =
        assert(List.length args = 3)
        let this, index = args.[0], args.[2]
        Memory.ReadArrayIndex state this [index] None
