namespace VSharp.Solver

open System.Collections.Generic

open VSharp
open VSharp.Core.SolverInteraction
open VSharp.Core
open VSharp.Prelude

module Cache =
    let transpose (xs: 'a option seq) : 'a seq option =
        Seq.fold (fun acc x -> Option.map2 Seq.cons x acc) (Some Seq.empty) xs

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

    let (|SeqEmpty|SeqCons|) (xs: 'a seq) =
        match Seq.tryHead xs with
        | Some x -> SeqCons(x, Seq.tail xs)
        | None -> SeqEmpty

    let rec tryRenameImpl (namings: Dictionary<string, string>) (lhs: term) (rhs: term) : bool =
        match lhs.term, rhs.term with
        | Nop, Nop -> true
        | Concrete(o1, _), Concrete(o2, _) when o1.Equals(o2) -> true
        | Constant(v1, s1, ty1), Constant(v2, s2, ty2) when s1.Equals(s2) && ty1.Equals(ty2) ->
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

    let (=~=) (lhs: term) (rhs: term) =
        lhs.GetHashCode() = rhs.GetHashCode()
        && tryRename (Dictionary<string, string>()) lhs rhs

    let bigAnd (xs: term seq) : term =
        match xs with
        | SeqCons(x, xs) ->
            Seq.fold (fun acc x -> Expression (Operator OperationType.LogicalAnd) [ acc; x ] typeof<bool>) x xs
        | SeqEmpty -> failwith "cannot construct `and` from empty expression"

    let isSubset (core: term array) (q: term list) : bool =
        core
        |> Array.map (fun part -> List.tryFind ((=~=) part) q)
        |> transpose
        |> Option.map List.ofSeq
        |> Option.filter (fun candidates -> bigAnd core =~= bigAnd candidates)
        |> Option.isSome

    [<CustomEquality; NoComparison>]
    type alphaTerm =
        { inner: term }

        static member from(t: term) : alphaTerm = { inner = t }

        override self.Equals(o: obj) =
            match o with
            | :? alphaTerm as other -> self.inner =~= other.inner
            | _ -> false

        override self.GetHashCode() = self.inner.GetHashCode()

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

    let smtResults: Dictionary<alphaTerm, smtResult> = Dictionary()
    let unsatCores: HashSet<unsatCore> = HashSet()

    // ======================= Public API =======================

    let public update (q: term) (outcome: smtResult) : unit =
        match outcome with
        // TODO: Decoding of bool expression can fail, so current workaround is to return empty core
        | SmtUnsat { core = core } when not (Array.isEmpty core) -> unsatCores.Add(unsatCore.fromParts core) |> ignore
        | _ -> ()

        smtResults.TryAdd(alphaTerm.from q, outcome) |> ignore

    // TODO: probably should substitute constants's names
    let public lookup (q: term) : smtResult option =
        let aq = { inner = q }

        if smtResults.ContainsKey(aq) then
            Some smtResults[aq]
        else
            let qParts = coreParts q

            unsatCores
            |> Seq.filter (fun core -> unsatCore.size core < List.length qParts)
            |> Seq.tryFind (fun core -> isSubset core.parts qParts)
            |> Option.map (fun core -> SmtUnsat { core = core.parts })
