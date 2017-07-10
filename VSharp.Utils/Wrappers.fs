namespace VSharp

[<AutoOpen>]
module public Wrappers =
    let public __notImplemented__() = raise (new System.NotImplementedException())
    let public internalfail message = "Internal error: " + message |> failwith
    let public toString x = x.ToString()
    let public format f objs = System.String.Format(f, objs)
    let public format1 f (obj : obj) = System.String.Format(f, obj)
    let public format2 f (obj1 : obj) (obj2 : obj) = System.String.Format(f, obj1, obj2)
    let public format3 f (obj1 : obj) (obj2 : obj) (obj3 : obj) = System.String.Format(f, obj1, obj2, obj3)
    let public id1 x _ = x
    let public id2 _ x = x
    let public always x = (fun _ -> x)
    let public curry f x y = f (x, y)
    let public uncurry f (x, y) = f x y
    let public join s (ss : seq<string>) = System.String.Join(s, ss)
    let public cons x xs = x :: xs
    let public withFst x = fun y -> (x, y)
    let public withSnd y = fun x -> (x, y)
    let public mapAdd (map : Map<'a, 'b>) key value = map.Add(key, value)

    let public getDictValueOrUpdate (dict : System.Collections.Generic.IDictionary<'a, 'b>) key fallback =
        if dict.ContainsKey(key) then dict.[key]
        else
            let newVal = fallback() in
            dict.Add(key, newVal)
            newVal

    let public tryGetDictValue (dict : System.Collections.Generic.IDictionary<'a, 'b>) key defaultValue =
        if dict.ContainsKey(key) then dict.[key]
        else defaultValue

    let mapFoldMap mapping state table =
        let mapFolder (map, state) key value =
            let newValue, newState = mapping key state value in
            (Map.add key newValue map, newState)
        Map.fold mapFolder (Map.empty, state) table

    let mappedPartition f xs =
        let rec mappedPartitionAcc f left right = function
            | [] -> (List.rev left, List.rev right)
            | x::xs ->
                match f x with
                | Some m -> mappedPartitionAcc f (m::left) right xs
                | None -> mappedPartitionAcc f left (x::right) xs
        mappedPartitionAcc f [] [] xs

    let rec map2 f xs1 xs2 =
        match xs1, xs2 with
        | SeqEmpty, [] -> []
        | SeqEmpty, x2::xs2' -> f None (Some x2) :: map2 f xs1 xs2'
        | SeqNode(x1, xs1'), [] -> f (Some x1) None :: map2 f xs1' xs2
        | SeqNode(x1, xs1'), x2::xs2' -> f (Some x1) (Some x2) :: map2 f xs1' xs2'
