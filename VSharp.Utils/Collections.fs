namespace VSharp

open System.Collections.Generic

module public Seq =
    let foldi f st xs =
        let i = ref (-1)
        Seq.fold (fun s t ->
            i := !i + 1
            f s !i t) st xs

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

    let public minus xs ys =
        let l1 = List.length xs
        let l2 = List.length ys
        let result, tail = List.splitAt (l2 - l1) xs
        assert(tail = ys)
        result

    let rec public changeLast f xs =
        let cons x = function
            | [] -> [f x]
            | xs -> x :: xs
        List.foldBack cons xs []

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

    let public ofSeq<'a, 'b when 'a : equality> (s : seq<'a * 'b>) : IDictionary<'a, 'b> =
        let result = new Dictionary<'a, 'b>()
        Seq.iter result.Add s
        result :> IDictionary<'a, 'b>

    let public equals (dict1 : IDictionary<'a,'b>) (dict2 : IDictionary<'a, 'b>) =
        dict1.Keys.Count = dict2.Keys.Count &&
        dict1.Keys |> Seq.forall (fun k -> dict2.ContainsKey(k) && obj.Equals(dict2.[k], dict1.[k]));

type 'a stack = 'a list

module public Stack =

    let peek = function
        | [] -> failwith "Attempt to peak head of an empty stack"
        | hd::tl -> hd

    let pop = function
        | [] -> failwith "Attempt to pop an empty stack"
        | hd::tl -> tl

    let push stack element = element::stack

    let updateHead stack newHd = push (pop stack) newHd

    let updateMiddle stack idx newVal =
        let rec updateMiddleRec xs idx acc =
            match xs with
            | [] -> acc
            | x::xs' when idx = 0 -> updateMiddleRec xs' (idx - 1) (newVal::acc)
            | x::xs' -> updateMiddleRec xs' (idx - 1) (x::acc)
        updateMiddleRec stack ((List.length stack) - idx - 1) [] |> List.rev

    let empty = List.empty

    let singleton = List.singleton

    let isEmpty = List.isEmpty

    let middle idx stack = List.item ((List.length stack) - idx - 1) stack

    let size = List.length

    let tryFindBottom = List.tryFindBack

type 'a nonEmptyList = 'a * 'a list

module public NonEmptyList =
    let ofList : 'a list -> 'a nonEmptyList = function
        | x::xs -> (x, xs)
        | _ -> internalfail "constructing non-empty list from empty list"

    let toList (x, xs) = x::xs
