namespace VSharp

module public Seq =
    let filterMap mapper xs =
        seq { for x in xs do
                match mapper x with
                | Some y -> yield y
                | None -> () }

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

    let rec public filterMap mapper = function
        | [] -> []
        | x::xs ->
            match mapper x with
            | Some y -> y::(filterMap mapper xs)
            | None -> (filterMap mapper xs)

    let rec public filterMap2 mapper xs ys =
        match xs, ys with
        | [], [] -> []
        | x::xs, y::ys ->
            match mapper x y with
            | Some z -> z::(filterMap2 mapper xs ys)
            | None -> (filterMap2 mapper xs ys)
        | _ -> internalfail "filterMap2 expects lists of equal lengths"

module public Map =
    let public add2 (map : Map<'a, 'b>) key value = map.Add(key, value)

    let foldMap mapping state table =
        let mapFolder (map, state) key value =
            let newValue, newState = mapping key state value in
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
    let public getValueOrUpdate (dict : System.Collections.Generic.IDictionary<'a, 'b>) key fallback =
        if dict.ContainsKey(key) then dict.[key]
        else
            let newVal = fallback() in
            dict.Add(key, newVal)
            newVal

    let public tryGetValue (dict : System.Collections.Generic.IDictionary<'a, 'b>) key defaultValue =
        if dict.ContainsKey(key) then dict.[key]
        else defaultValue

module public Stack =
    type 'a stack = 'a list

    let peak = function
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

type 'a NonEmptyList = 'a * 'a list

module public NonEmptyList =
    let ofList : 'a list -> 'a NonEmptyList = function
        | x::xs -> (x, xs)
        | _ -> internalfail "constructing non-empty list from empty list"

    let toList (x, xs) = x::xs
