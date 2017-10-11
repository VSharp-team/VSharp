namespace VSharp

open System.Collections
open System.Collections.Generic
open FSharpx.Collections

type Timestamp = uint32
type MemoryCell<'a> when 'a : equality = 'a * Timestamp * Timestamp  // Value * Creation timestamp * Modification timestamp

[<CustomEquality;NoComparison>]
type public Heap<'a, 'b> when 'a : equality and 'b : equality =
    {heap : PersistentHashMap<'a, MemoryCell<'b>>}
    static member Empty() = {heap = PersistentHashMap<'a, MemoryCell<'b>>.Empty()}
    member x.Length = x.heap.Length
    member x.ContainsKey(key) = x.heap.ContainsKey(key)
    member x.Add(pair) = {heap = x.heap.Add(pair)}
    member x.Remove(key) = x.heap.Remove(key)
    member x.Item
        with get key = x.heap.[key]
    static member ofSeq(items) = {heap = PersistentHashMap<'a, MemoryCell<'b>>.ofSeq(items)}
    member x.Iterator() = x.heap.Iterator()

    interface IEnumerable<'a*MemoryCell<'b>> with
        member x.GetEnumerator () =
          x.Iterator().GetEnumerator()

    interface IEnumerable with
        member x.GetEnumerator () =
          x.Iterator().GetEnumerator() :> IEnumerator

    override x.GetHashCode() = x.heap :> seq<'a * MemoryCell<'b>> |> List.ofSeq |> fun l -> l.GetHashCode()

    override x.Equals(o : obj) =
        match o with
        | :? Heap<'a, 'b> as h -> x.GetHashCode() = h.GetHashCode()
        | _ -> false

module public Heap =

    let public empty<'a, 'b when 'a : equality and 'b : equality> : Heap<'a, 'b> = Heap<'a, 'b>.Empty()

    let public ofSeq  = Heap<'a, 'b>.ofSeq
    let public toSeq (h : Heap<'a, 'b>) = h :> seq<'a * MemoryCell<'b>>

    let public contains key (h : Heap<'a, 'b>) = h.ContainsKey key
    let public find key (h : Heap<'a, 'b>) = h.[key]
    let public add key value (h : Heap<'a, 'b>) = h.Add(key, value)

    let public size (h : Heap<'a, 'b>) = h.Length

    let public map mapper (h : Heap<'a, 'b>) : Heap<'a, 'c> =
        h |> toSeq |> Seq.map (fun (k, v) -> k, mapper k v) |> ofSeq
    let public fold folder state (h : Heap<'a, 'b>) =
        h |> toSeq |> Seq.fold (fun state (k, v) -> folder state k v) state
    let public mapFold folder state (h : Heap<'a, 'b>) =
        h |> toSeq |> Seq.mapFold (fun state (k, v) -> folder state k v) state |> fun (r, s) -> ofSeq r, s

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
            h
            |> toSeq
            |> Seq.map (fun (k, (v, _, _)) -> k, v)
            |> Seq.sortBy sorter
            |> Seq.map (fun (k, v) -> sprintf format (keyMapper k) (valueMapper v)) in
        elements |> join separator

    let public dump (h : Heap<'a, 'b>) keyToString = toString "%s ==> %O" "\n" keyToString id Prelude.toString h
