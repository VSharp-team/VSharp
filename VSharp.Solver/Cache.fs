namespace VSharp.Solver

open System.Collections.Generic

open VSharp
open VSharp.Core.SolverInteraction
open VSharp.Core
open VSharp.Prelude

module Cache =
    let (|SeqEmpty|SeqCons|) (xs: 'a seq) =
        match Seq.tryHead xs with
        | Some x -> SeqCons(x, Seq.tail xs)
        | None -> SeqEmpty

    let rec transpose (xs: 'a option seq) : 'a seq option =
        match xs with
        | SeqCons(Some x, xs) -> transpose xs |> Option.map (Seq.cons x)
        | SeqCons(None, _) -> None
        | SeqEmpty -> Some Seq.empty

    let rec coreParts (core: term) : term list =
        match core with
        | Conjunction(xs) -> List.collect coreParts xs
        | _ -> List.singleton core

    let iteTerms (ite: iteType) : term seq =
        seq {
            yield! Seq.collect (fun (t1, t2) -> [ t1; t2 ]) ite.branches
            yield ite.elseValue
        }

    let sliceTerms (h: term) (slice: list<term * term * term>) : term seq =
        seq {
            yield h
            yield! Seq.collect (fun (t1, t2, t3) -> [ t1; t2; t3 ]) slice
        }

    let dictValuesNotClash (dict: Dictionary<'k, 'v>) : bool =
        dict.Count = (dict.Values |> Set |> Set.count)

    let addRenaming (namings: Dictionary<'k, 'v>) (k: 'k) (v: 'v) : bool =
        if namings.ContainsKey(k) then
            namings[k] = v
        else
            namings.Add(k, v)
            true

    let rec isSourcePure (src: ISymbolicConstantSource) : bool =
        match src with
        | StackReading _ -> true
        | GetHashCodeSource _ -> true
        | :? functionResultConstantSource -> true
        | StructFieldSource(src, _) -> isSourcePure src
        | HeapAddressSource src -> isSourcePure src
        | PointerOffsetSource src -> isSourcePure src
        | PointerAddressSource src -> isSourcePure src
        | _ -> false

    let rec tryRenameImpl (namings: Dictionary<string, string>) (lhs: term) (rhs: term) : bool =
        match lhs.term, rhs.term with
        | Nop, Nop -> true
        | Concrete(o1, _), Concrete(o2, _) when o1.Equals(o2) -> true
        | Constant(v1, s1, ty1), Constant(v2, s2, ty2) when
            (isSourcePure s1 && isSourcePure s2) || (s1.Equals(s2) && ty1.Equals(ty2))
            ->
            addRenaming namings v1.v v2.v
        | Expression(op1, ts1, ty1), Expression(op2, ts2, ty2) when
            op1.Equals(op2) && ty1.Equals(ty2) && List.length ts1 = List.length ts2
            ->
            tryRenameSeq namings ts1 ts2
        | Struct(fs1, ty1), Struct(fs2, ty2) when
            ty1.Equals(ty2) && Set(PersistentDict.keys fs1) = Set(PersistentDict.keys fs2)
            ->
            let ts = fs1 |> PersistentDict.toSeq |> Seq.map (fun (k, v) -> (v, fs2[k]))
            let ts1 = Seq.map fst ts
            let ts2 = Seq.map snd ts
            tryRenameSeq namings ts1 ts2
        | HeapRef(addr1, ty1), HeapRef(addr2, ty2) when addr1.Equals(addr2) && ty1.Equals(ty2) -> true
        | Ref addr1, Ref addr2 when addr1.Equals(addr2) -> true
        | Ptr(base1, ty1, t1), Ptr(base2, ty2, t2) when base1 = base2 && ty1.Equals(ty2) -> tryRenameImpl namings t1 t2
        | Slice(h1, slice1), Slice(h2, slice2) when List.length slice1 = List.length slice2 ->
            tryRenameSeq namings (sliceTerms h1 slice1) (sliceTerms h2 slice2)
        | Ite ite1, Ite ite2 when List.length ite1.branches = List.length ite2.branches ->
            tryRenameSeq namings (iteTerms ite1) (iteTerms ite2)
        | _ -> false

    and tryRenameSeq (namings: Dictionary<string, string>) (lhs: term seq) (rhs: term seq) : bool =
        match lhs, rhs with
        | SeqCons(x, xs), SeqCons(y, ys) -> tryRenameImpl namings x y && tryRenameSeq namings xs ys
        | SeqCons(_), SeqEmpty -> false
        | SeqEmpty, SeqCons(_) -> false
        | SeqEmpty, SeqEmpty -> true

    let tryRename (namings: Dictionary<string, string>) (lhs: term) (rhs: term) =
        tryRenameImpl namings lhs rhs && dictValuesNotClash namings

    let public findRenaming (lhs: term) (rhs: term) : Map<string, string> option =
        let namings = Dictionary()

        if tryRename namings lhs rhs then
            namings |> Seq.map (fun (KeyValue(k, v)) -> (k, v)) |> Map |> Some
        else
            None

    let rec alphaHashCode (t: term) : int =
        match t.term with
        | Constant(_, src, t) when isSourcePure src -> t.GetHashCode()
        | Expression(op, args, ty) ->
            [ op.GetHashCode()
              (List.map alphaHashCode args).GetHashCode()
              ty.GetHashCode() ]
                .GetHashCode()
        | Struct(fields, t) ->
            (fields
             |> PersistentDict.toSeq
             |> Seq.map (fun (id, field) -> [ id.GetHashCode(); alphaHashCode field ].GetHashCode())
             |> Seq.cons (t.GetHashCode())
             |> Seq.toList)
                .GetHashCode()
        | Ptr(b, ty, t) -> [ b.GetHashCode(); ty.GetHashCode(); alphaHashCode t ].GetHashCode()
        | Slice(h, vals) -> (sliceTerms h vals |> Seq.map alphaHashCode |> Seq.toList).GetHashCode()
        | Ite(ite) -> (iteTerms ite |> Seq.map alphaHashCode |> Seq.toList).GetHashCode()
        // TODO: probably branch for address is needed
        | _ -> t.GetHashCode()


    let (=~=) (lhs: term) (rhs: term) =
        tryRename (Dictionary<string, string>()) lhs rhs

    [<CustomEquality; NoComparison>]
    type alphaTerm =
        { value: term
          hc: int }

        static member from(t: term) : alphaTerm = { value = t; hc = alphaHashCode t }

        override self.Equals(o: obj) =
            match o with
            | :? alphaTerm as other -> self.hc = other.hc && self.value =~= other.value
            | _ -> false

        override self.GetHashCode() = self.hc

    let bigAnd (xs: term seq) : term =
        match xs with
        | SeqCons(x, xs) ->
            Seq.fold (fun acc x -> Expression (Operator OperationType.LogicalAnd) [ acc; x ] typeof<bool>) x xs
        | SeqEmpty -> failwith "cannot construct `and` from empty expression"

    [<CustomEquality; NoComparison>]
    type unsatCore =
        { all: alphaTerm
          parts: term array }

        static member fromParts(parts: term array) : unsatCore =
            { all = alphaTerm.from (bigAnd parts)
              parts = parts }

        static member size(core: unsatCore) : int = core.parts.Length

        override self.Equals(o: obj) =
            match o with
            | :? unsatCore as other -> self.all = other.all
            | _ -> false

        override self.GetHashCode() = self.all.GetHashCode()

    let isSubset (core: unsatCore) (q: alphaTerm list) : bool =
        core.parts
        |> Array.map alphaTerm.from
        |> Array.map (fun part -> List.tryFind ((=) part) q)
        |> transpose
        |> Option.map List.ofSeq
        |> Option.filter (fun candidates ->
            core.all = alphaTerm.from (candidates |> Seq.map (fun t -> t.value) |> bigAnd))
        |> Option.isSome

    [<Struct>]
    type partEntry = { core: int; idxInCore: int }

    let unsatCores: List<unsatCore> = List()
    let partsMapping: Dictionary<alphaTerm, List<partEntry>> = Dictionary()

    // ======================= Public API =======================

    let public update (q: term) (outcome: smtResult) : unit =
        match outcome with
        // TODO: Decoding of bool expression can fail, so current workaround is to return empty core
        | SmtUnsat { core = core } when not (Array.isEmpty core) ->
            let core = unsatCore.fromParts core
            let idx = unsatCores.Count
            unsatCores.Add(core)

            for i, part in Seq.indexed core.parts do
                let part = alphaTerm.from part
                let entry = { core = idx; idxInCore = i }

                if partsMapping.ContainsKey(part) then
                    partsMapping[part].Add(entry)
                else
                    partsMapping.Add(part, List [ entry ])

            ()
        | _ -> ()

    let rec foreach (f: 'a -> 'b option) (seq: 'a seq) : 'b option =
        match seq with
        | SeqCons(x, xs) ->
            match f x with
            | Some x -> Some x
            | None -> foreach f xs
        | SeqEmpty -> None

    [<Struct>]
    type coreMatch = { partInCore: int; partInQuery: int }

    // TODO: probably should substitute constants's names
    let public lookup (q: term) : smtResult option =
        let qParts = q |> coreParts |> List.map alphaTerm.from |> Array.ofList
        let coreToEntriesInQuery: Dictionary<int, List<coreMatch>> = Dictionary()

        let candidate =
            qParts
            |> Seq.indexed
            |> foreach (fun (i, part) ->
                let entries =
                    if partsMapping.ContainsKey(part) then
                        partsMapping[part]
                    else
                        List()

                entries
                |> foreach (fun entry ->
                    let coreIdx = entry.core

                    let coreEntry =
                        { partInCore = entry.idxInCore
                          partInQuery = i }

                    if coreToEntriesInQuery.ContainsKey(coreIdx) then
                        coreToEntriesInQuery[coreIdx].Add(coreEntry)
                    else
                        coreToEntriesInQuery.Add(coreIdx, List [ coreEntry ])

                    Some coreIdx
                    |> Option.filter (fun idx -> coreToEntriesInQuery[idx].Count = unsatCores[idx].parts.Length)
                    |> Option.map (fun idx ->
                        let subQuery =
                            coreToEntriesInQuery[idx]
                            |> Seq.sortBy (fun entry -> entry.partInCore)
                            |> Seq.map (fun entry -> Array.get qParts entry.partInQuery)
                            |> Seq.map (fun t -> t.value)
                            |> bigAnd

                        (subQuery, unsatCores[idx]))
                    |> Option.filter (fun (subQuery, core) -> alphaTerm.from subQuery = core.all)
                    |> Option.map (fun _ -> unsatCores[coreIdx])))

        candidate |> Option.map (fun core -> SmtUnsat { core = core.parts })
