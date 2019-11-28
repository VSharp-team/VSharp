namespace VSharp

open System.Collections
open System.Collections.Generic
open FSharpx.Collections

[<CustomEquality;NoComparison>]
type public heap<'key, 'term, 'fql, 'typ> when 'key : equality and 'term : equality and 'fql : equality =
    {heap : PersistentHashMap<memoryCell<'key, 'fql, 'typ>, 'term>}
    static member Empty() = {heap = PersistentHashMap<memoryCell<'a, 'fql, 'typ>, 'b>.Empty()}
    member x.Length = x.heap.Length
    member x.ContainsKey(key) = x.heap.ContainsKey(key)
    member x.Add(pair) = {heap = x.heap.Add(pair)}
    member x.Remove(key) = x.heap.Remove(key)
    member x.Item
        with get key = x.heap.[key]
    static member ofSeq(items) = {heap = PersistentHashMap<memoryCell<'key, 'fql, 'typ>, 'term>.ofSeq(items)}
    member x.Iterator() = x.heap.Iterator()

    interface IEnumerable<memoryCell<'key, 'fql, 'typ> * 'term> with
        member x.GetEnumerator () =
          x.Iterator().GetEnumerator()

    interface IEnumerable with
        member x.GetEnumerator () =
          x.Iterator().GetEnumerator() :> IEnumerator

    override x.GetHashCode() = x.heap :> seq<memoryCell<'key, 'fql, 'typ> * 'term> |> List.ofSeq |> fun l -> l.GetHashCode() // TODO: use set instead of list

    override x.Equals(o : obj) =
        match o with
        | :? heap<'key, 'term, 'fql, 'typ> as h -> x.GetHashCode() = h.GetHashCode()
        | _ -> false

module public Heap =

    let public getKey key = key.key

    let public empty<'a, 'b, 'fql, 'typ when 'a : equality and 'b : equality and 'fql : equality> : heap<'a, 'b, 'fql, 'typ> = heap<'a, 'b, 'fql, 'typ>.Empty()
    let public isEmpty h = PersistentHashMap.length h.heap = 0

    let public ofSeq = heap<'a, 'b, 'fql, 'typ>.ofSeq
    let public toSeq (h : heap<'a, 'b, 'fql, 'typ>) = h :> seq<memoryCell<'a, 'fql, 'typ> * 'b>

    let public contains (key : memoryCell<'a, 'fql, 'typ>) (h : heap<'a, 'b, 'fql, 'typ>) = h.ContainsKey key
    let public find (key : memoryCell<'a, 'fql, 'typ>) (h : heap<'a, 'b, 'fql, 'typ>) = h.[key]
    let public add key value (h : heap<'a, 'b, 'fql, 'typ>) = h.Add(key, value)

    let public size (h : heap<'a, 'b, 'fql, 'typ>) = h.Length

    let public map mapper (h : heap<'a, 'b, 'fql, 'typ>) : heap<'a, 'c, 'fql, 'typ> =
        h |> toSeq |> Seq.map mapper |> ofSeq
    let public fold folder state (h : heap<'a, 'b, 'fql, 'typ>) =
        h |> toSeq |> Seq.fold (fun state (k, v) -> folder state k v) state
    let public forall predicate (h : heap<'a, 'b, 'fql, 'typ>) =
        h |> toSeq |> Seq.forall predicate

    let public mapFold folder state (h : heap<'a, 'b, 'fql, 'typ>) =
        h |> toSeq |> Seq.mapFold folder state |> mapfst ofSeq

    let private fqlLocations (h : heap<'a, 'b, 'fql, 'typ>) = h |> toSeq |> Seq.map fst
    let public locations (h : heap<'a, 'b, 'fql, 'typ>) = h |> toSeq |> Seq.map (getKey << fst)
    let public values (h : heap<'a, 'b, 'fql, 'typ>) = h |> toSeq |> Seq.map (fun (_, v) -> v)

    let public partition predicate (h : heap<'a, 'b, 'fql, 'typ>) =
        h |> toSeq |> Seq.map (fun (k, v) -> (k.key, v)) |> List.ofSeq |> List.partition predicate

    let public unify acc guards (heaps : heap<'a, 'b, 'fql, 'typ> list) unifier =
        let keys = new System.Collections.Generic.HashSet<memoryCell<'a, 'fql, 'typ>>()

        List.iter (fqlLocations >> keys.UnionWith) heaps
        let unifyOneKey acc k =
            let hgvs = List.mapi2 (fun i g h -> if contains k h then (i, g, Some(h.[k])) else (i, g, None)) guards heaps
            unifier acc k hgvs
        Seq.fold unifyOneKey acc keys

    let public merge guards (heaps : heap<'a, 'b, 'fql, 'typ> list) resolve =
        unify heaps.Head guards heaps (fun acc k hgvs -> add k (resolve k hgvs) acc)

    let public unify2 acc (h1 : heap<'a, 'b, 'fql, 'typ>) (h2 : heap<'a, 'b, 'fql, 'typ>) unifier =
        let keysSet = HashSet(fqlLocations h1)
        keysSet.UnionWith(fqlLocations h2)
        let unifyIfShould acc key =
            match contains key h1, contains key h2 with
            | true, true  -> unifier acc key (Some h1.[key]) (Some h2.[key])
            | true, false -> unifier acc key (Some h1.[key]) None
            | false, true -> unifier acc key None (Some h2.[key])
            | _ -> __unreachable__()
        Seq.fold unifyIfShould acc keysSet

    let public merge2 (h1 : heap<'a, 'b, 'fql, 'typ>) (h2 : heap<'a, 'b, 'fql, 'typ>) resolve =
        unify2 h1 h1 h2 (fun s k v1 v2 -> add k (resolve k v1 v2) s)

    let public toString format separator keyMapper valueMapper sorter (h : heap<'a, 'b, 'fql, 'typ>) =
        h
        |> toSeq
        |> Seq.sortBy (fst >> getKey >> sorter)
        |> Seq.map (fun (k, v) -> sprintf format (keyMapper k.key) (valueMapper k.typ v))
        |> join separator

    let public dump (h : heap<'a, 'b, 'fql, 'typ>) keyToString valueToString sorter =
        toString "%s ==> %O" "\n" keyToString valueToString sorter h
