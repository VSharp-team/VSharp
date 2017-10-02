namespace VSharp

open FSharpx.Collections

type Timestamp = uint32
type MemoryCell<'a> when 'a : equality = 'a * Timestamp * Timestamp  // Value * Creation timestamp * Modification timestamp
type Heap<'a, 'b> when 'a : equality and 'b : equality = PersistentHashMap<'a, MemoryCell<'b>>

module public Heap =

    let public empty<'a, 'b when 'a : equality and 'b : equality> : Heap<'a, 'b> = PersistentHashMap.empty<'a, MemoryCell<'b>>

    let public contains key (h : Heap<'a, 'b>) = PersistentHashMap.containsKey key h
    let public find key (h : Heap<'a, 'b>) = h.[key]
    let public add key value (h : Heap<'a, 'b>) = PersistentHashMap.add key value h

    let public size (h : Heap<'a, 'b>) = h.Length

    let public map mapper (h : Heap<'a, 'b>) : Heap<'a, 'c> =
        h |> PersistentHashMap.toSeq |> Seq.map (fun (k, v) -> k, mapper k v) |> PersistentHashMap.ofSeq
    let public fold folder state (h : Heap<'a, 'b>) =
        h |> PersistentHashMap.toSeq |> Seq.fold (fun state (k, v) -> folder state k v) state
    let public mapFold folder state (h : Heap<'a, 'b>) =
        h |> PersistentHashMap.toSeq |> Seq.mapFold (fun state (k, v) -> folder state k v) state |> fun (r, s) -> PersistentHashMap.ofSeq r, s

    let public ofSeq = PersistentHashMap.ofSeq
    let public toSeq (h : Heap<'a, 'b>) = PersistentHashMap.toSeq h

    let public locations (h : Heap<'a, 'b>) = h |> toSeq |> Seq.map fst
    let public values (h : Heap<'a, 'b>) = h |> toSeq |> Seq.map (snd >> fst3)

    let public partition predicate (h : Heap<'a, 'b>) =
        h |> toSeq |> Seq.map (fun (k, v) -> (k, fst3 v)) |> List.ofSeq |> List.partition predicate

    let public merge<'a, 'b, 'c when 'a : equality and 'b : equality> (guards : 'c list) (heaps : Heap<'a, 'b> list) resolve : Heap<'a, 'b> =
        let keys = new System.Collections.Generic.HashSet<'a>() in
        List.iter (locations >> keys.UnionWith) heaps
        let mergeOneKey k =
            let vals = List.filterMap2 (fun g s -> if contains k s then Some(g, s.[k]) else None) guards heaps in
            (k, resolve vals)
        in
        keys |> Seq.map mergeOneKey |> ofSeq

    let public merge2 (h1 : Heap<'a, 'b>) (h2 : Heap<'a, 'b>) resolve =
        let resolveIfShould map key value =
            if contains key map then
                let oldValue = map.[key] in
                let newValue = value in
                if oldValue = newValue then map
                else
                    add key (resolve oldValue newValue) map
            else
                add key value map
        in
        fold resolveIfShould h1 h2

    let public toString format separator keyMapper valueMapper sorter (h : Heap<'a, 'b>) =
        let elements =
            h |> PersistentHashMap.toSeq
            |> Seq.map (fun (k, (v, _, _)) -> k, v)
            |> Seq.sortBy sorter
            |> Seq.map (fun (k, v) -> sprintf format (keyMapper k) (valueMapper v)) in
        elements |> join separator

    let public dump (h : Heap<'a, 'b>) keyToString = toString "%s ==> %O" "\n" keyToString id Prelude.toString h
