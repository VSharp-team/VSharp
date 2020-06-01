namespace VSharp

type vectorTime = uint32 list

module VectorTime =
    let zero = [0u]

    let rec lessOrEqual (t1 : vectorTime) (t2 : vectorTime) =
        match t1, t2 with
        | [], _ -> true
        | _::_, [] -> false
        | v1::t1, v2::t2 -> v1 < v2 || (v1 = v2 && lessOrEqual t1 t2)

    let max t1 t2 = if lessOrEqual t1 t2 then t2 else t1
    let min t1 t2 = if lessOrEqual t1 t2 then t1 else t2

    let compose (t1 : vectorTime) (t2 : vectorTime) =
        List.append t1 t2

    let print (t : vectorTime) = t |> List.map toString |> join "."