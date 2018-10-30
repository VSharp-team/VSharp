namespace VSharp

open System.Collections
open System.Collections.Generic
open FSharpx.Collections

[<CustomEquality;NoComparison>]
type public heapKey<'a, 'fql when 'a : equality> =
    {key : 'a; FQL : 'fql option}
    override x.GetHashCode() =
        x.key.GetHashCode()
    override x.Equals(o : obj) =
        match o with
        | :? heapKey<'a, 'fql> as y -> x.key.Equals(y.key)
        | _ -> false

[<CustomEquality;NoComparison>]
type public heap<'a, 'b, 'fql> when 'a : equality and 'b : equality =
    {heap : PersistentHashMap<heapKey<'a, 'fql>, 'b memoryCell>}
    static member Empty() = {heap = PersistentHashMap<heapKey<'a, 'fql>, 'b memoryCell>.Empty()}
    member x.Length = x.heap.Length
    member x.ContainsKey(key) = x.heap.ContainsKey({key = key; FQL = None})
    member x.ContainsKey(key) = x.heap.ContainsKey(key)
    member x.Add(pair) = {heap = x.heap.Add(pair)}
    member x.Remove(key) = x.heap.Remove(key)
    member x.Item
        with get key = x.heap.[{key = key; FQL = None}]
    member x.Item
        with get key = x.heap.[key]
    static member ofSeq(items) = {heap = PersistentHashMap<heapKey<'a, 'fql>, 'b memoryCell>.ofSeq(items)}
    member x.Iterator() = x.heap.Iterator()

    interface IEnumerable<heapKey<'a, 'fql> * 'b memoryCell> with
        member x.GetEnumerator () =
          x.Iterator().GetEnumerator()

    interface IEnumerable with
        member x.GetEnumerator () =
          x.Iterator().GetEnumerator() :> IEnumerator

    override x.GetHashCode() = x.heap :> seq<heapKey<'a, 'fql> * 'b memoryCell> |> List.ofSeq |> fun l -> l.GetHashCode()

    override x.Equals(o : obj) =
        match o with
        | :? heap<'a, 'b, 'fql> as h -> x.GetHashCode() = h.GetHashCode()
        | _ -> false

module public Heap =
    open System.Runtime.Remoting.Contexts

    let private getKey key = key.key

    let public empty<'a, 'b, 'fql when 'a : equality and 'b : equality> : heap<'a, 'b, 'fql> = heap<'a, 'b, 'fql>.Empty()
    let public isEmpty h = PersistentHashMap.length h.heap = 0

    let public ofSeq  = heap<'a, 'b, 'fql>.ofSeq
    let public toSeq (h : heap<'a, 'b, 'fql>) = h :> seq<heapKey<'a, 'fql> * 'b memoryCell>

    let public contains (key : 'a) (h : heap<'a, 'b, 'fql>) = h.ContainsKey key
    let private containsHeapKey (heapKey : heapKey<'a, 'fql>) (h : heap<'a, 'b, 'fql>) = h.ContainsKey heapKey
    let public find (key : 'a) (h : heap<'a, 'b, 'fql>) = h.[key]
    let public add key value (h : heap<'a, 'b, 'fql>) = h.Add(key, value)

    let public size (h : heap<'a, 'b, 'fql>) = h.Length

    let private mapKeyValue mapper (key, value) =
        let k, v = mapper key.key value
        ({key with key = k}, v)

    let public map mapper (h : heap<'a, 'b, 'fql>) : heap<'a, 'c, 'fql> =
        h |> toSeq |> Seq.map (mapKeyValue mapper) |> ofSeq
    let public map' mapper (h : heap<'a, 'b, 'fql>) : heap<'a, 'c, 'fql> =
        h |> toSeq |> Seq.map (fun (k, v) -> k, mapper k.key v) |> ofSeq
    let public foldFQL folder state (h : heap<'a, 'b, 'fql>) =
        h |> toSeq |> Seq.fold (fun state (k, v) -> folder state k v) state
    let public fold folder state (h : heap<'a, 'b, 'fql>) =
        h |> toSeq |> Seq.fold (fun state (k, v) -> folder state k.key v) state

    let private mapFoldKeyValue folder state (key, value) =
        let (k, v), state = folder state key.key value
        ({key with key = k}, v), state

    let public mapFold folder state (h : heap<'a, 'b, 'fql>) =
        h |> toSeq |> Seq.mapFold (mapFoldKeyValue folder) state |> mapfst ofSeq

    let private fqlLocations (h : heap<'a, 'b, 'fql>) = h |> toSeq |> Seq.map fst
    let public locations (h : heap<'a, 'b, 'fql>) = h |> toSeq |> Seq.map (getKey << fst)
    let public values (h : heap<'a, 'b, 'fql>) = h |> toSeq |> Seq.map (fun (_, v) -> v.value)

    let public partition predicate (h : heap<'a, 'b, 'fql>) =
        h |> toSeq |> Seq.map (fun (k, v) -> (k.key, v.value)) |> List.ofSeq |> List.partition predicate

    let public merge<'a, 'b, 'c, 'fql when 'a : equality and 'b : equality> (guards : 'c list) (heaps : heap<'a, 'b, 'fql> list) resolve : heap<'a, 'b, 'fql> =
        let keys = new System.Collections.Generic.HashSet<heapKey<'a, 'fql>>()
        List.iter (fqlLocations >> keys.UnionWith) heaps
        let mergeOneKey k =
            let vals = List.filterMap2 (fun g s -> if containsHeapKey k s then Some(g, s.[k]) else None) guards heaps
            (k, resolve vals)
        keys |> Seq.map mergeOneKey |> ofSeq

    let public unify acc (h1 : heap<'a, 'b, 'fql>) (h2 : heap<'a, 'b, 'fql>) unifier instantiate1 instantiate2 =
        let keysSet = HashSet(fqlLocations h1)
        keysSet.UnionWith(fqlLocations h2)
        let unifyIfShould acc key =
            match containsHeapKey key h1, containsHeapKey key h2 with
            | true, true  -> unifier acc key h1.[key] h2.[key]
            | true, false -> instantiate1 acc key h1.[key]
            | false, true -> instantiate2 acc key h2.[key]
            | _ -> __unreachable__()
        Seq.fold unifyIfShould acc keysSet

    let public merge2 (h1 : heap<'a, 'b, 'fql>) (h2 : heap<'a, 'b, 'fql>) resolve =
        unify h1 h1 h2 (fun s k v1 v2 -> add k (resolve v1 v2) s) (fun s k v1 -> add k v1 s) (fun s k v2 -> add k v2 s)

    let public toString format separator keyMapper valueMapper sorter (h : heap<'a, 'b, 'fql>) =
        let elements =
            h
            |> toSeq
            |> Seq.map (fun (k, v) -> k.key, v.value)
            |> Seq.sortBy sorter
            |> Seq.map (fun (k, v) -> sprintf format (keyMapper k) (valueMapper v))
        elements |> join separator

    let public dump (h : heap<'a, 'b, 'fql>) keyToString = toString "%s ==> %O" "\n" keyToString id Prelude.toString h
