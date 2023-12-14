namespace VSharp

open System.Text
open FSharpx.Collections
open VSharp

// TODO: migrate on System.Collections.Immutable?
// TODO: We do not really need equality of 'value, but PersistentHashMap requires it
[<CustomEquality;NoComparison>]
type public pdict<'key, 'value> when 'key : equality and 'value : equality =
    private {impl : PersistentHashMap<'key, 'value>; mutable hash : int option}
    static member Empty() = {impl = PersistentHashMap<'key, 'value>.Empty(); hash = None}
    member x.Item
        with get key = x.impl[key]

    override x.GetHashCode() =
        let createHash() =
            let hash = x.impl :> seq<'key * 'value> |> Seq.fold (fun acc x -> System.HashCode.Combine(acc, x)) (System.HashCode().ToHashCode())
            x.hash <- Some hash
            hash
        Option.defaultWith createHash x.hash

    override x.Equals(o : obj) =
        match o with
        | :? pdict<'key, 'value> as h ->
            x.GetHashCode() = h.GetHashCode() && Seq.forall2 (=) x.impl h.impl
        | _ -> false

    override x.ToString() =
        let mutable sb = StringBuilder()
        let dict = Seq.sortBy (fst >> toString) (x.impl :> seq<'key * 'value>)
        for k, v in dict do
            sb <- sb.AppendLine($"{k} => {v}")
        sb.ToString()

module public PersistentDict =

    let public empty<'a, 'b when 'a : equality and 'b : equality> : pdict<'a, 'b> = pdict.Empty()
    let public isEmpty d = PersistentHashMap.length d.impl = 0

    let public ofSeq s = {impl = PersistentHashMap<'a, 'b>.ofSeq s; hash = None}
    let public toSeq (d : pdict<'a, 'b>) = d.impl :> seq<'a * 'b>

    let public contains (key : 'a) (d : pdict<'a, 'b>) = d.impl.ContainsKey key
    let public find (d : pdict<'a, 'b>) (key : 'a) = d.impl.[key]

    // [NOTE] if PersistentDict already contains key, 'add' will replace it with new value
    let public add (key : 'a) (value : 'b) (d : pdict<'a, 'b>) = {impl = d.impl.Add(key, value); hash = None}
    let public remove key (d : pdict<'a, 'b>) = {impl = d.impl.Remove key; hash = None}
    let public tryFind (d : pdict<'a, 'b>) key =
        // TODO: speed it up by scanning only once! Perhaps we should migrate to System.Collections.Immutable to support this
        if d.impl.ContainsKey key then d.impl.[key] |> Some
        else None
    let public update (d : pdict<'a, 'b>) key defaultValue (mapper : 'b -> 'b) =
        match tryFind d key with
        | Some value -> add key (mapper value) d
        | None -> add key (mapper defaultValue) d

    let public size (d : pdict<'a, 'b>) = d.impl.Length

    let public map (keyMapper : 'a -> 'a) (valueMapper : 'b -> 'c) (d : pdict<'a, 'b>) : pdict<'a, 'c> =
        d |> toSeq |> Seq.map (fun (k, v) -> (keyMapper k, valueMapper v)) |> ofSeq
    let public iter (action : 'a * 'b -> unit) (d : pdict<'a, 'b>) : unit =
        d |> toSeq |> Seq.iter action
    let public fold folder state (d : pdict<'a, 'b>) =
        d |> toSeq |> Seq.fold (fun state (k, v) -> folder state k v) state
    let public forall predicate (d : pdict<'a, 'b>) =
        d |> toSeq |> Seq.forall predicate

    let public mapFold folder state (d : pdict<'a, 'b>) =
        d |> toSeq |> Seq.mapFold folder state |> mapfst ofSeq

    let public keys (d : pdict<'a, 'b>) = d |> toSeq |> Seq.map fst
    let public values (d : pdict<'a, 'b>) = d |> toSeq |> Seq.map (fun (_, v) -> v)

    let public partition predicate (d : pdict<'a, 'b>) =
        let mutable sat = empty
        let mutable unsat = empty
        for (k, v) in toSeq d do
            if predicate k v then sat <- add k v sat
            else unsat <- add k v unsat
        sat, unsat

    let public groupBy mapper (d : pdict<'a, 'b>) =
        d |> toSeq |> Seq.groupBy mapper |> ofSeq

    // WARNING: Assumes that all dictionaries have the same set of keys, but does not validate it!
    let public unify acc guards (dicts : pdict<'a, 'b> list) unifier =
        assert (not <| dicts.IsEmpty)
        let unifyOneKey acc k =
            let hgvs = List.map2 (fun g h -> (g, find h k)) guards dicts
            unifier acc k hgvs
        Seq.fold unifyOneKey acc (keys dicts.Head)

    // WARNING: Assumes that d1 and d2 have the same set of keys, but does not validate it!
    let public merge guards (dicts : pdict<'a, 'b> list) resolve =
        unify dicts.Head guards dicts (fun acc k hgvs -> add k (resolve hgvs) acc)

    // WARNING: Assumes that d1 and d2 have the same set of keys, but does not validate it!
    let public unify2 acc (d1 : pdict<'a, 'b>) (d2 : pdict<'a, 'b>) unifier =
        let unifyIfShould acc key = unifier acc key d1.[key] d2.[key]
        Seq.fold unifyIfShould acc (keys d1)

    // WARNING: Assumes that d1 and d2 have the same set of keys, but does not validate it!
    let public merge2 (d1 : pdict<'a, 'b>) (d2 : pdict<'a, 'b>) resolve =
        unify2 d1 d1 d2 (fun s k v1 v2 -> add k (resolve k v1 v2) s)

    let private commonToString format separator sort keyMapper valueMapper (d : pdict<'a, 'b>) =
        let sorted = toSeq d |> sort
        let mutable sb = StringBuilder()
        for k, v in sorted do
            let string : string = sprintf format (keyMapper k) (valueMapper v)
            sb <- sb.Append(string + separator)
        sb.ToString()

    let public toString format separator sorter keyMapper valueMapper (d : pdict<'a, 'b>) =
        commonToString format separator (Seq.sortBy (fst >> sorter)) keyMapper valueMapper d

    let public dump (d : pdict<'a, 'b>) sort keyToString valueToString =
        commonToString "%s ==> %O" "\n" sort keyToString valueToString d

type pset<'a when 'a : equality> = pdict<'a, int>

module PersistentSet =

    let public empty<'a when 'a : equality> : pset<'a> = PersistentDict.empty<'a, int>
    let public isEmpty (d : pset<'a>) = PersistentDict.isEmpty d

    let public ofSeq (seq : seq<'a>) = PersistentDict.ofSeq (Seq.map (withSnd 0) seq)
    let public toSeq (d : pset<'a>) = PersistentDict.keys d

    let public contains (key : 'a) (d : pset<'a>) = PersistentDict.contains key d

    // [NOTE] if PersistentDict already contains key, 'add' will replace it with new value
    let public add (d : pset<'a>) (key : 'a) : pset<'a> = PersistentDict.add key 0 d
    let public remove (d : pset<'a>) (key : 'a) : pset<'a> = PersistentDict.remove key d

    let public cardinality (d : pset<'a>) = PersistentDict.size d

    let public fold folder state (d : pset<'a>) =
        d |> toSeq |> Seq.fold folder state
    let public forall predicate (d : pset<'a>) =
        d |> toSeq |> Seq.forall predicate
    let public map (mapper : 'a -> 'a) (d : pset<'a>) : pset<'a> =
        PersistentDict.map mapper id d
    let public iter (action : 'a -> unit) (d : pset<'a>) : unit =
        PersistentDict.iter (fst >> action) d

    let subtract (s1 : pset<'a>) (s2 : pset<'a>) =
        Seq.fold remove s1 (toSeq s2)
    let union (s1 : pset<'a>) (s2 : pset<'a>) =
        Seq.fold add s1 (toSeq s2)
    let intersect (s1 : pset<'a>) (s2 : pset<'a>) =
        Seq.fold (fun acc x -> if contains x s1 then add acc x else acc) empty (toSeq s2)
