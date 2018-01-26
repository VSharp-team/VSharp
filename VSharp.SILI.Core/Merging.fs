namespace VSharp.Core

open VSharp

module internal Merging =

    type private MergeType =
        | StructMerge
        | ArrayMerge
        | BoolMerge
        | DefaultMerge

    let private mergeTypeOf term =
        match term.term with
        | Struct _ -> StructMerge
        | Array _ -> ArrayMerge
        | _ when isBool term -> BoolMerge
        | _ -> DefaultMerge

    let guardOf term =
        match term.term with
        | GuardedValues(gs, _) -> disjunction term.metadata gs
        | _ -> makeTrue term.metadata

    let rec private boolMerge = function
        | [] -> []
        | [(g, v)] as gvs ->
            match g with
            | True -> gvs
            | _ -> [propagateGuard g v]
        | [(g1, v1); (g2, v2)] -> [(g1 ||| g2, (g1 &&& v1) ||| (g2 &&& v2))]
        | (g, v)::gvs ->
            let guard = List.fold (|||) g (List.map fst gvs)
            let value = List.fold (fun acc (g, v) -> acc ||| (g &&& v)) (g &&& v) gvs
            [(guard, value)]

    and private structMerge = function
        | [] -> []
        | [(g, v)] as gvs ->
            match g with
            | True -> gvs
            | _ -> [propagateGuard g v]
        | (x :: _) as gvs ->
            let t = x |> snd |> typeOf
            assert(gvs |> Seq.map (snd >> typeOf) |> Seq.forall ((=) t))
            let gs, vs = List.unzip gvs
            let extractFields = term >> function
                | Struct(fs, _) -> fs
                | t -> internalfailf "Expected struct, got %O" t
            let fss = vs |> List.map extractFields
            let merged = Heap.merge gs fss mergeCells
            [(True, Struct (fst x).metadata merged t)]

    and private arrayMerge = function
        | [] -> []
        | [(g, v)] as gvs ->
            match g with
            | True -> gvs
            | _ -> [propagateGuard g v]
        | (x :: _) as gvs ->
            let t = x |> snd |> typeOf
            assert(gvs |> Seq.map (snd >> typeOf) |> Seq.forall ((=) t))
            let gs, vs = List.unzip gvs
            let extractArrayInfo = term >> function
                | Array(dim, len, lower, init, contents, lengths, _) -> (dim, len, lower, init, contents, lengths)
                | t -> internalfailf "Expected array, got %O" t
            let ds, lens, lows, inits, contents, lengths =
                vs
                |> Seq.map extractArrayInfo
                |> fun info -> Seq.foldBack (fun (d, l, lw, i, c, ls) (da, la, lwa, ia, ca, lsa) -> (d::da, l::la, lw::lwa, i::ia, c::ca, ls::lsa)) info ([], [], [], [], [], [])
            let d = List.unique ds
            let l = List.unique lens
            let mergedLower = Heap.merge gs lows mergeCells
            let mergedContents = Heap.merge gs contents mergeCells
            let mergedLengths = Heap.merge gs lengths mergeCells
            let mergedInit = inits |> Seq.map2 (fun ng init -> Seq.map (fun (g, v) -> (ng &&& g, v)) init) gs |> Seq.concat |> List.ofSeq |> mergeSame
            [(True, Array Metadata.empty d l mergedLower mergedInit mergedContents mergedLengths t)]

    and private simplify (|Unguard|_|) gvs =
        let rec loop gvs out =
            match gvs with
            | [] -> out
            | ((True, _) as gv)::_ -> [gv]
            | (False, _)::gvs' -> loop gvs' out
            | (g, Unguard us)::gvs' ->
                let guarded = us |> List.map (fun (g', v) -> (g &&& g', v))
                loop gvs' (List.append (simplify (|Unguard|_|) guarded) out)
            | gv::gvs' -> loop gvs' (gv::out)
        loop gvs []

    and mergeSame<'a when 'a : equality> : (term * 'a) list -> (term * 'a) list = function
        | [] -> []
        | [_] as xs -> xs
        | [(g1, v1); (g2, v2)] as gvs -> if v1 = v2 then [(g1 ||| g2, v1)] else gvs
        | gvs ->
            let rec loop gvs out =
                match gvs with
                | [] -> out
                | (g, v)::gvs' ->
                    let eq, rest = List.partition (snd >> (=) v) gvs'
                    let joined = List.fold (|||) g (List.map fst eq)
                    match joined with
                    | True -> [(joined, v)]
                    | False -> loop rest out
                    | _ -> loop rest ((joined, v)::out)
            loop gvs []

    and private typedMerge gvs t =
        match t with
        | BoolMerge -> boolMerge gvs
        | StructMerge -> structMerge gvs
        | ArrayMerge -> arrayMerge gvs
        | DefaultMerge -> gvs

    and propagateGuard g v =
        match v.term with
        | Struct(contents, t) ->
            let contents' = Heap.map' (fun _ cell -> { cell with value = merge [(g, cell.value)] }) contents
            (Terms.True, Struct v.metadata contents' t)
        | Array(dimension, len, lower, init, contents, lengths, t) ->
            let contents' = Heap.map' (fun _ cell -> { cell with value = merge [(g, cell.value)] }) contents
            let lower' = Heap.map' (fun _ cell -> { cell with value = merge [(g, cell.value)] }) lower
            let lengths' = Heap.map' (fun _ cell -> { cell with value = merge [(g, cell.value)] }) lengths
            let init' = List.map (fun (gi, i) -> gi &&& g, i) init
            (Terms.True, Array v.metadata dimension len lower' init' contents' lengths' t)
        | _ -> (g, v)

    and private compress = function
        | [] -> []
        | [(g, v)] as gvs ->
            match g with
            | True -> gvs
            | _ -> [propagateGuard g v]
        | [(_, v1); (_, v2)] as gvs when mergeTypeOf v1 = mergeTypeOf v2 -> typedMerge (mergeSame gvs) (mergeTypeOf v1)
        | [_; _] as gvs -> gvs
        | gvs ->
            gvs
            |> mergeSame
            |> List.groupBy (snd >> mergeTypeOf)
            |> List.map (fun (t, gvs) -> typedMerge gvs t)
            |> List.concat

    and merge gvs =
        match compress (simplify (|UnionT|_|) gvs) with
        | [(True, v)] -> v
        | [(g, v)] when Terms.isBool v -> g &&& v
        | gvs' -> Union Metadata.empty gvs'

    and mergeCells gcs =
        let foldCell (acc1, acc2, acc3) (g, cell) = ((g, cell.value)::acc1, min acc2 cell.created, max acc3 cell.modified)
        let gvs, c, m = gcs |> List.fold foldCell ([], System.UInt32.MaxValue, System.UInt32.MinValue)
        { value = merge gvs; created = c; modified = m }

    let merge2Terms g h u v =
        let g = guardOf u &&& g
        let h = guardOf v &&& h
        match g, h with
        | _, _ when u = v -> u
        | True, _
        | _, False
        | False, _
        | _, True -> v
        | ErrorT _, _ -> g
        | _, ErrorT _ -> h
        | _ -> merge [(g, u); (h, v)]

    let merge2Cells g h ({value = u;created = cu;modified = mu} as ucell : term memoryCell) ({value = v;created = cv;modified = mv} as vcell : term memoryCell) =
        let g = guardOf u &&& g
        let h = guardOf v &&& h
        match g, h with
        | _, _ when u = v -> { value = u; created = min cu cv; modified = min mu mv }
        | True, _
        | _, False -> ucell
        | False, _
        | _, True -> vcell
        | ErrorT _, _ -> { ucell with value = g }
        | _, ErrorT _ -> { vcell with value = h }
        | _ -> mergeCells [(g, ucell); (h, vcell)]

    let mergeGeneralizedHeaps guards heaps =
        // TODO: get rid of extra zips/unzips
        let (|MergedHeap|_|) = function | Merged gvs -> Some gvs | _ -> None
        let guards, heaps = List.zip guards heaps |> simplify (|MergedHeap|_|) |> List.unzip
        // TODO: non-restricted heaps should be merged in a different way
        let defined, undefined =
            heaps
                |> List.zip guards
                |> List.mappedPartition (function | (g, Defined(r, s)) -> Some(g, r, s) | _ -> None)
        if defined.IsEmpty then
            undefined |> mergeSame |> Merged
        else
            let definedGuards, restricted, definedHeaps = List.unzip3 defined
            let restricted = List.unique restricted
            let definedHeap = Heap.merge definedGuards definedHeaps mergeCells |> State.Defined restricted
            if undefined.IsEmpty then definedHeap
            else
                let definedGuard = disjunction Metadata.empty definedGuards
                (definedGuard, definedHeap)::undefined |> mergeSame |> Merged

    let private merge2GeneralizedHeaps g1 g2 h1 h2 resolve =
        match h1, h2 with
        | Defined(r1, h1), Defined(r2, h2) ->
            assert(r1 = r2)
            Heap.merge2 h1 h2 resolve |> State.Defined r1
        | _ -> mergeGeneralizedHeaps [g1; g2] [h1; h2]

    let merge2States condition1 condition2 (state1 : state) (state2 : state) =
        match condition1, condition2 with
        | True, _ -> state1
        | False, _ -> state2
        | _, True -> state2
        | _, False -> state1
        | _ ->
            assert(state1.pc = state2.pc)
            assert(state1.frames = state2.frames)
            let resolve = merge2Cells condition1 condition2
            let mergedStack = Utils.MappedStack.merge2 state1.stack state2.stack resolve (State.stackLazyInstantiator state1)
            let mergedHeap = merge2GeneralizedHeaps condition1 condition2 state1.heap state2.heap resolve
            let mergedStatics = merge2GeneralizedHeaps condition1 condition2 state1.statics state2.statics resolve
            { state1 with stack = mergedStack; heap = mergedHeap; statics = mergedStatics }

    let mergeStates conditions states : state =
        assert(List.length states > 0)
        let first : state = List.head states
        let frames = first.frames
        let path = first.pc
        assert(states |> List.forall (fun s -> s.frames = frames))
        assert(states |> List.forall (fun s -> s.pc = path))
        let mergedStack = Utils.MappedStack.merge conditions (List.map State.stackOf states) mergeCells (State.stackLazyInstantiator first)
        let mergedHeap = mergeGeneralizedHeaps conditions (List.map State.heapOf states)
        let mergedStatics = mergeGeneralizedHeaps conditions (List.map State.staticsOf states)
        { stack = mergedStack; heap = mergedHeap; statics = mergedStatics; frames = frames; pc = path }

    let genericSimplify gvs =
        let rec loop gvs out =
            match gvs with
            | [] -> out
            | ((True, _) as gv)::_ -> [gv]
            | (False, _)::gvs' -> loop gvs' out
            | gv::gvs' -> loop gvs' (gv::out)
        loop gvs [] |> mergeSame

    let commonGuardedMapk mapper gvs merge k =
        let gs, vs = List.unzip gvs
        Cps.List.mapk mapper vs (List.zip gs >> merge >> k)
    let guardedMapk mapper gvs k = commonGuardedMapk mapper gvs merge k
    let guardedMap mapper gvs = guardedMapk (Cps.ret mapper) gvs id

    let commonGuardedStateMapk mapper gvs state merge k =
        let gs, vs = List.unzip gvs
        Cps.List.mapk (mapper state) vs (fun vsst ->
        let vs, states = List.unzip vsst
        k (vs |> List.zip gs |> merge, mergeStates gs states))
    let guardedStateMapk mapper gvs state k = commonGuardedStateMapk mapper gvs state merge k
    let guardedStateMap mapper gvs state = guardedStateMapk (Cps.ret2 mapper) gvs state id

    let commonGuardedErroredMapk mapper errorMapper gvs state merge k =
        let ges, gvs = List.partition (snd >> isError) gvs
        let egs, es = List.unzip ges
        let vgs, vs = List.unzip gvs
        let eg = disjunction Metadata.empty egs
        Cps.List.mapk (mapper state) vs (fun vsst ->
        let vs', states = List.unzip vsst
        let ges' = es |> List.map errorMapper |> List.zip egs
        let gvs' = List.zip vgs vs' |> List.append ges' |> merge
        let state' = mergeStates (eg :: vgs) (state :: states)
        k (gvs', state'))
    let guardedErroredMapk mapper errorMapper gvses state k = commonGuardedErroredMapk mapper errorMapper gvses state merge k
    let guardedErroredMap mapper errorMapper gvses state = guardedErroredMapk (Cps.ret2 mapper) errorMapper gvses state id

    let unguard = function
        | {term = Union gvs} -> gvs
        | t -> [(True, t)]

    let erroredUnguard term =
        let ges, gvs = term |> unguard |> List.partition (snd >> isError)
        ges, merge gvs

    let productUnion f t1 t2 =
        match t1.term, t2.term with
        | Union gvs1, Union gvs2 ->
            gvs1 |> List.map (fun (g1, v1) ->
            gvs2 |> List.map (fun (g2, v2) ->
            (g1 &&& g2, f v1 v2)))
            |> List.concat |> merge
        | Union gvs1, _ ->
            gvs1 |> List.map (fun (g1, v1) -> (g1, f v1 t2)) |> merge
        | _, Union gvs2 ->
            gvs2 |> List.map (fun (g2, v2) -> (g2, f t1 v2)) |> merge
        | _ -> f t1 t2

    let rec private guardedCartesianProductRec mapper ctor gacc xsacc = function
        | [] -> [(gacc, ctor xsacc)]
        | x::xs ->
            mapper x
            |> List.map (fun (g, v) ->
                let g' = gacc &&& g
                if isError v then [(g', v)]
                else
                    guardedCartesianProductRec mapper ctor g' (List.append xsacc [v]) xs)
            |> List.concat |> genericSimplify


    let guardedCartesianProduct mapper ctor terms =
        guardedCartesianProductRec mapper ctor True [] terms

    let guardedApply f gvs =
        gvs |> List.map (fun (g, v) -> (g, if isError v then v else f v))

    let map f t =
        match t.term with
        | Union gvs -> guardedApply f gvs |> merge
        | Error e -> e
        | _ -> f t

    let statedMap f state term =
        match term.term with
        | Union gvs -> guardedStateMap f gvs state
        | Error e -> e, state
        | _ -> f state term

    let statedMapk f state term k =
        match term.term with
        | Union gvs -> guardedStateMapk f gvs state k
        | Error e -> k (e, state)
        | _ -> f state term k
