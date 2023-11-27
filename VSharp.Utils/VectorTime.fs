namespace VSharp

open System.Runtime.CompilerServices

type vectorTime = int32 list

module VectorTime =

    let zero = [0]
    let infty = [System.Int32.MaxValue]
    let minfty = [System.Int32.MinValue]

    let hash (time : vectorTime) =
        if time = zero then RuntimeHelpers.GetHashCode null
        else time.GetHashCode()

    let extractFromSingleton (time: vectorTime) =
        assert(List.length time = 1)
        List.head time

    let singleton t : vectorTime =
        [t]

    let rec compare (t1 : vectorTime) (t2 : vectorTime) =
        List.compareWith (fun (v1 : int32) (v2 : int32) -> v1.CompareTo(v2)) t1 t2

    let equals (t1 : vectorTime) (t2 : vectorTime) =
        0 = compare t1 t2

    let less (t1 : vectorTime) (t2 : vectorTime) =
        -1 = compare t1 t2

    let greater (t1 : vectorTime) (t2 : vectorTime) =
        1 = compare t1 t2

    let lessOrEqual (t1 : vectorTime) (t2 : vectorTime) =
        let res = compare t1 t2
        res = -1 || res = 0

    let greaterOrEqual (t1 : vectorTime) (t2 : vectorTime) =
        let res = compare t1 t2
        res = 1 || res = 0

    let isDescending (t : vectorTime) =
        true
//        t |> Seq.pairwise |> Seq.forall (fun (a, b) -> a >= b)

    let advance (t : vectorTime) = List.mapLast (fun x -> x + 1) t
//        assert(isDescending t)
//        let inc x = if x = System.UInt32.MaxValue then internalfailf "Advancing infinite time!" else x + 1u
//        let getNextTime x acc =
//            match acc with
//            | _ when List.isEmpty acc -> x |> inc |> List.singleton
//            | [a] when a > x -> x |> inc |> List.singleton
//            | _ -> x :: acc
//        List.foldBack getNextTime t []

    let max t1 t2 = if lessOrEqual t1 t2 then t2 else t1
    let min t1 t2 = if lessOrEqual t1 t2 then t1 else t2

    let isEmpty = List.isEmpty

    let compose (t1 : vectorTime) (t2 : vectorTime) =
        List.append t1 t2

    let print (t : vectorTime) = t |> List.map toString |> join "."
