namespace VSharp

open System.Collections.Generic

module public Seq =
    let foldi f st xs =
        let i = ref (-1)
        Seq.fold (fun s t ->
            i := !i + 1
            f s !i t) st xs

    let public cons x xs = seq {
        yield x
        yield! xs
    }

    let public (|Cons|Empty|) s =
        if Seq.isEmpty s then Empty
        else Cons (Seq.head s, Seq.tail s)

module public List =
    let rec private mappedPartitionAcc f left right = function
        | [] -> (List.rev left, List.rev right)
        | x::xs ->
            match f x with
            | Some m -> mappedPartitionAcc f (m::left) right xs
            | None -> mappedPartitionAcc f left (x::right) xs

    let public mappedPartition f xs =
        mappedPartitionAcc f [] [] xs

    let rec public map2Different f xs1 xs2 =
        match xs1, xs2 with
        | Seq.Empty, [] -> []
        | Seq.Empty, x2::xs2' -> f None (Some x2) :: map2Different f xs1 xs2'
        | Seq.Cons(x1, xs1'), [] -> f (Some x1) None :: map2Different f xs1' xs2
        | Seq.Cons(x1, xs1'), x2::xs2' -> f (Some x1) (Some x2) :: map2Different f xs1' xs2'

    let public append3 xs ys zs = List.append xs (List.append ys zs)

    let rec public filterMap2 mapper xs ys =
        match xs, ys with
        | [], [] -> []
        | x::xs, y::ys ->
            let z = mapper x y
            optCons (filterMap2 mapper xs ys) z
        | _ -> internalfail "filterMap2 expects lists of equal lengths"

    let public unique = function
        | [] -> internalfail "unexpected non-empty list"
        | x::xs ->
            assert(List.forall ((=)x) xs)
            x

    let rec public cartesian = function
        | [xs] -> Seq.map List.singleton xs
        | xs::xss ->
            seq {
                for x in xs do
                    for xs' in cartesian xss do
                        yield x::xs'
            }
        | [] -> Seq.empty

    let public cartesianMap mapper = cartesian >> Seq.map mapper

    let lastAndRest xs =
        let rec lastAndRest x xs k =
            match xs with
            | [] -> k x []
            | y::ys -> lastAndRest y ys (fun c cs -> k c (x::cs))
        match xs with
        | [] -> internalfail "List.last of empty list!"
        | x::xs -> lastAndRest x xs makePair

    let rec mapLast f = function
        | [x] -> [f x]
        | x::xs -> x::(mapLast f xs)
        | [] -> []

module public Map =
    let public add2 (map : Map<'a, 'b>) key value = map.Add(key, value)

    let foldMap mapping state table =
        let mapFolder (map, state) key value =
            let newValue, newState = mapping key state value
            (Map.add key newValue map, newState)
        Map.fold mapFolder (Map.empty, state) table

    let filterMap mapping map =
        Map.fold
            (fun m k v ->
                match mapping k v with
                | Some v' -> Map.add k v' m
                | None -> m)
            Map.empty map

    let findOrDefaultWith k map defThunk =
        let optionValue = Map.tryFind k map
        Option.defaultWith defThunk optionValue

module public Dict =
    let public getValueOrUpdate (dict : IDictionary<'a, 'b>) key fallback =
        if dict.ContainsKey(key) then dict.[key]
        else
            let newVal = fallback()
            dict.Add(key, newVal)
            newVal

    let public tryGetValue (dict : IDictionary<'a, 'b>) key defaultValue =
        if dict.ContainsKey(key) then dict.[key]
        else defaultValue

    let public tryGetValue2 (dict : IDictionary<'a, 'b>) key defaultValue =
        if dict.ContainsKey(key) then dict.[key]
        else defaultValue()

    let public ofSeq<'a, 'b when 'a : equality> (s : seq<'a * 'b>) : IDictionary<'a, 'b> =
        let result = new Dictionary<'a, 'b>()
        Seq.iter result.Add s
        result :> IDictionary<'a, 'b>

    let public equals (dict1 : IDictionary<'a,'b>) (dict2 : IDictionary<'a, 'b>) =
        dict1.Keys.Count = dict2.Keys.Count &&
        dict1.Keys |> Seq.forall (fun k -> dict2.ContainsKey(k) && obj.Equals(dict2.[k], dict1.[k]));

    let public toString format separator keyMapper valueMapper sorter (d : IDictionary<'a, 'b>) =
        d
        |> Seq.sortBy (fun kv -> sorter kv.Key)
        |> Seq.map (fun kv -> sprintf format (keyMapper kv.Key) (valueMapper kv.Value))
        |> join separator


type 'a stack = 'a list

module public Stack =

    let bottomAndRest = List.lastAndRest

    let pop = function
        | [] -> failwith "Attempt to pop an empty stack"
        | head :: tl -> head, tl

    let push stack element = element::stack

    let empty = List.empty

    let singleton = List.singleton

    let isEmpty = List.isEmpty

    let middle idx stack = List.item ((List.length stack) - idx - 1) stack

    let size = List.length

    let tryFindBottom = List.tryFindBack

    let exists = List.exists

    let map = List.map

    let union = List.append

    let fold = List.fold
