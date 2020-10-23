namespace VSharp

type vectorTime = uint32 list

module VectorTime =
    let zero = [0u]
    let infty = [System.UInt32.MaxValue]

    let rec lessOrEqual (t1 : vectorTime) (t2 : vectorTime) =
        match t1, t2 with
        | [], _ -> true
        | _::_, [] -> false
        | v1::t1, v2::t2 -> v1 < v2 || (v1 = v2 && lessOrEqual t1 t2)

    let advance (t : vectorTime) =
        List.mapLast (fun x -> if x = System.UInt32.MaxValue then internalfailf "Advancing infinite time!" else x + 1u) t

    let max t1 t2 = if lessOrEqual t1 t2 then t2 else t1
    let min t1 t2 = if lessOrEqual t1 t2 then t1 else t2

    let compose (t1 : vectorTime) (t2 : vectorTime) =
        List.append t1 t2

    let print (t : vectorTime) = t |> List.map toString |> join "."
