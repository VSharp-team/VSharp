namespace VSharp

open System.Collections
open System.Collections.Generic
open FSharpx.Collections

[<CustomEquality;NoComparison>]
type public heap<'a, 'b> when 'a : equality and 'b : equality =
    {heap : PersistentHashMap<'a, 'b memoryCell>}
    static member Empty() = {heap = PersistentHashMap<'a, 'b memoryCell>.Empty()}
    member x.Length = x.heap.Length
    member x.ContainsKey(key) = x.heap.ContainsKey(key)
    member x.Add(pair) = {heap = x.heap.Add(pair)}
    member x.Remove(key) = x.heap.Remove(key)
    member x.Item
        with get key = x.heap.[key]
    static member ofSeq(items) = {heap = PersistentHashMap<'a, 'b memoryCell>.ofSeq(items)}
    member x.Iterator() = x.heap.Iterator()

    interface IEnumerable<'a * 'b memoryCell> with
        member x.GetEnumerator () =
          x.Iterator().GetEnumerator()

    interface IEnumerable with
        member x.GetEnumerator () =
          x.Iterator().GetEnumerator() :> IEnumerator

    override x.GetHashCode() = x.heap :> seq<'a * 'b memoryCell> |> List.ofSeq |> fun l -> l.GetHashCode()

    override x.Equals(o : obj) =
        match o with
        | :? heap<'a, 'b> as h -> x.GetHashCode() = h.GetHashCode()
        | _ -> false

module public Heap =

    let public empty<'a, 'b when 'a : equality and 'b : equality> : heap<'a, 'b> = heap<'a, 'b>.Empty()
    let public isEmpty h = PersistentHashMap.length h.heap = 0

    let public ofSeq  = heap<'a, 'b>.ofSeq
    let public toSeq (h : heap<'a, 'b>) = h :> seq<'a * 'b memoryCell>

    let public contains key (h : heap<'a, 'b>) = h.ContainsKey key
    let public find key (h : heap<'a, 'b>) = h.[key]
    let public add key value (h : heap<'a, 'b>) = h.Add(key, value)

    let public size (h : heap<'a, 'b>) = h.Length

    let public map mapper (h : heap<'a, 'b>) : heap<'a, 'c> =
        h |> toSeq |> Seq.map (fun (k, v) -> mapper k v) |> ofSeq
    let public map' mapper (h : heap<'a, 'b>) : heap<'a, 'c> =
        h |> toSeq |> Seq.map (fun (k, v) -> k, mapper k v) |> ofSeq
    let public fold folder state (h : heap<'a, 'b>) =
        h |> toSeq |> Seq.fold (fun state (k, v) -> folder state k v) state
    let public mapFold folder state (h : heap<'a, 'b>) =
        h |> toSeq |> Seq.mapFold (fun state (k, v) -> folder state k v) state |> fun (r, s) -> ofSeq r, s

    let public locations (h : heap<'a, 'b>) = h |> toSeq |> Seq.map fst
    let public values (h : heap<'a, 'b>) = h |> toSeq |> Seq.map (fun (k, v) -> v.value)

    let public partition predicate (h : heap<'a, 'b>) =
        h |> toSeq |> Seq.map (fun (k, v) -> (k, v.value)) |> List.ofSeq |> List.partition predicate

    let public merge<'a, 'b, 'c when 'a : equality and 'b : equality> (guards : 'c list) (heaps : heap<'a, 'b> list) resolve : heap<'a, 'b> =
        let keys = new System.Collections.Generic.HashSet<'a>()
        List.iter (locations >> keys.UnionWith) heaps
        let mergeOneKey k =
            let vals = List.filterMap2 (fun g s -> if contains k s then Some(g, s.[k]) else None) guards heaps
            (k, resolve vals)
        keys |> Seq.map mergeOneKey |> ofSeq

    let public unify state (h1 : heap<'a, 'b>) (h2 : heap<'a, 'b>) unifier =
        let unifyIfShould state key value =
            if contains key h1 then
                let oldValue = h1.[key]
                let newValue = value
                if oldValue = newValue then state
                else
                    unifier state key (Some oldValue) (Some newValue)
            else
                unifier state key None (Some value)
        fold unifyIfShould state h2
        // TODO: handle values in h1 that are not contained in h2

    let public merge2 (h1 : heap<'a, 'b>) (h2 : heap<'a, 'b>) resolve =
        unify h1 h1 h2 (fun s k v1 v2 ->
            match v1, v2 with
            | Some v1, Some v2 -> add k (resolve v1 v2) s
            | None, Some v2 -> add k v2 s
            | _ -> __notImplemented__())

    let public toString format separator keyMapper valueMapper sorter (h : heap<'a, 'b>) =
        let elements =
            h
            |> toSeq
            |> Seq.map (fun (k, v) -> k, v.value)
            |> Seq.sortBy sorter
            |> Seq.map (fun (k, v) -> sprintf format (keyMapper k) (valueMapper v))
        elements |> join separator

    let public dump (h : heap<'a, 'b>) keyToString = toString "%s ==> %O" "\n" keyToString id Prelude.toString h
