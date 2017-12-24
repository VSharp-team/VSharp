namespace VSharp

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
        | _ when IsBool term -> BoolMerge
        | _ -> DefaultMerge

    let internal guardOf term =
        match term.term with
        | Terms.GuardedValues(gs, _) -> disjunction term.metadata gs
        | _ -> Terms.MakeTrue term.metadata

    let rec private boolMerge = function
        | [] -> []
        | [(g, v)] as gvs ->
            match g with
            | True -> gvs
            | _ -> [propagateGuard g v]
        | [(g1, v1); (g2, v2)] -> [(g1 ||| g2, (g1 &&& v1) ||| (g2 &&& v2))]
        | (g, v)::gvs ->
            let guard = List.fold (|||) g (List.map fst gvs) in
            let value = List.fold (fun acc (g, v) -> acc ||| (g &&& v)) (g &&& v) gvs in
            [(guard, value)]

    and private structMerge = function
        | [] -> []
        | [(g, v)] as gvs ->
            match g with
            | True -> gvs
            | _ -> [propagateGuard g v]
        | (x :: _) as gvs ->
            let t = x |> snd |> TypeOf in
            assert(gvs |> Seq.map (snd >> TypeOf) |> Seq.forall ((=) t))
            let gs, vs = List.unzip gvs in
            let extractFields = term >> function
                | Struct(fs, _) -> fs
                | t -> "Expected struct, got " + (toString t) |> internalfail
            in
            let fss = vs |> List.map extractFields in
            let merged = Heap.merge gs fss mergeCells in
            [(True, Struct (fst x).metadata merged t)]

    and private arrayMerge = function
        | [] -> []
        | [(g, v)] as gvs ->
            match g with
            | True -> gvs
            | _ -> [propagateGuard g v]
        | (x :: _) as gvs ->
            let t = x |> snd |> TypeOf in
            assert(gvs |> Seq.map (snd >> TypeOf) |> Seq.forall ((=) t))
            let gs, vs = List.unzip gvs in
            let extractArrayInfo = term >> function
                | Array(dim, len, lower, init, contents, lengths, _) -> (dim, len, lower, init, contents, lengths)
                | t -> "Expected array, got " + (toString t) |> internalfail
            in
            let ds, lens, lows, inits, contents, lengths =
                vs
                |> Seq.map extractArrayInfo
                |> fun info -> Seq.foldBack (fun (d, l, lw, i, c, ls) (da, la, lwa, ia, ca, lsa) -> (d::da, l::la, lw::lwa, i::ia, c::ca, ls::lsa)) info ([], [], [], [], [], [])
            in
            let d = List.unique ds
            let l = List.unique lens
            let mergedLower = Heap.merge gs lows mergeCells in
            let mergedContents = Heap.merge gs contents mergeCells in
            let mergedLengths = Heap.merge gs lengths mergeCells in
            let mergedInit = inits |> Seq.map2 (fun ng init -> Seq.map (fun (g, v) -> (ng &&& g, v)) init) gs |> Seq.concat |> List.ofSeq |> mergeSame in
            [(True, Array Metadata.empty d l mergedLower mergedInit mergedContents mergedLengths t)]

    and private simplify (|Unguard|_|) gvs =
        let rec loop gvs out =
            match gvs with
            | [] -> out
            | ((True, v) as gv)::gvs' -> [gv]
            | (False, v)::gvs' -> loop gvs' out
            | (g, Unguard us)::gvs' ->
                let guarded = us |> List.map (fun (g', v) -> (g &&& g', v)) in
                loop gvs' (List.append (simplify (|Unguard|_|) guarded) out)
            | gv::gvs' -> loop gvs' (gv::out)
        loop gvs []

    and internal mergeSame<'a when 'a : equality> : (Term * 'a) list -> (Term * 'a) list = function
        | [] -> []
        | [_] as xs -> xs
        | [(g1, v1); (g2, v2)] as gvs -> if v1 = v2 then [(g1 ||| g2, v1)] else gvs
        | gvs ->
            let rec loop gvs out =
                match gvs with
                | [] -> out
                | (g, v)::gvs' ->
                    let eq, rest = List.partition (snd >> (=) v) gvs' in
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
            let contents' = Heap.map (fun _ (v, c, m) -> (merge [(g, v)], c, m)) contents in
            (Terms.True, Struct v.metadata contents' t)
        | Array(dimension, len, lower, constant, contents, lengths, t) ->
            let contents' = Heap.map (fun _ (v, c, m) -> (merge [(g, v)], c, m)) contents in
            let lower' = Heap.map (fun _ (v, c, m) -> (merge [(g, v)], c, m)) lower in
            let lengths' = Heap.map (fun _ (v, c, m) -> (merge [(g, v)], c, m)) lengths in
            (Terms.True, Array v.metadata dimension len lower' constant contents' lengths' t)
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

    and internal merge gvs =
        match compress (simplify (|UnionT|_|) gvs) with
        | [(True, v)] -> v
        | [(g, v)] when Terms.IsBool v -> g &&& v
        | gvs' -> Union Metadata.empty gvs'

    and internal mergeCells gcs =
        let foldCell (acc1, acc2, acc3) (g, (v, c, m)) = ((g, v)::acc1, min acc2 c, max acc3 m) in
        let gvs, c, m = gcs |> List.fold foldCell ([], System.UInt32.MaxValue, System.UInt32.MinValue) in
        (merge gvs, c, m)

    let internal merge2Terms g h u v =
        let g = guardOf u &&& g in
        let h = guardOf v &&& h in
        match g, h with
        | _, _ when u = v -> u
        | True, _
        | _, False
        | False, _
        | _, True -> v
        | ErrorT _, _ -> g
        | _, ErrorT _ -> h
        | _ -> merge [(g, u); (h, v)]

    let internal merge2Cells g h ((u, cu, mu) as ucell : MemoryCell<Term>) ((v, cv, mv) as vcell : MemoryCell<Term>) =
        let g = guardOf u &&& g in
        let h = guardOf v &&& h in
        match g, h with
        | _, _ when u = v -> (u, min cu cv, min mu mv)
        | True, _
        | _, False -> ucell
        | False, _
        | _, True -> vcell
        | ErrorT _, _ -> (g, cu, mu)
        | _, ErrorT _ -> (h, cv, mv)
        | _ -> mergeCells [(g, ucell); (h, vcell)]

    let internal mergeGeneralizedHeaps guards heaps =
        // TODO: get rid of extra zips/unzips
        let (|MergedHeap|_|) = function | State.Merged gvs -> Some gvs | _ -> None
        let guards, heaps = List.zip guards heaps |> simplify (|MergedHeap|_|) |> List.unzip
        // TODO: non-restricted heaps should be merged in a different way
        let defined, undefined =
            heaps
                |> List.zip guards
                |> List.mappedPartition (function | (g, State.Defined(r, s)) -> Some(g, r, s) | _ -> None)
        in
        let definedGuards, restricted, definedHeaps = List.unzip3 defined in
        let restricted = List.unique restricted
        let definedHeap = Heap.merge definedGuards definedHeaps mergeCells |> State.Defined restricted
        if undefined.IsEmpty then definedHeap
        else
            let definedGuard = disjunction Metadata.empty definedGuards in
            let defined = definedGuards |> List.map (withSnd definedHeap) in
            (definedGuard, definedHeap)::undefined |> mergeSame |> State.Merged

    let private merge2GeneralizedHeaps g1 g2 h1 h2 resolve =
        match h1, h2 with
        | State.Defined(r1, h1), State.Defined(r2, h2) ->
            assert(r1 = r2)
            Heap.merge2 h1 h2 resolve |> State.Defined r1
        | _ -> mergeGeneralizedHeaps [g1; g2] [h1; h2]

    let internal merge2States condition1 condition2 (state1 : State.state) (state2 : State.state) =
        match condition1, condition2 with
        | True, _ -> state1
        | False, _ -> state2
        | _, True -> state2
        | _, False -> state1
        | _ ->
            assert(state1.pc = state2.pc)
            assert(state1.frames = state2.frames)
            let resolve = merge2Cells condition1 condition2 in
            let mergedStack = Utils.MappedStack.merge2 state1.stack state2.stack resolve (State.stackLazyInstantiator state1) in
            let mergedHeap = merge2GeneralizedHeaps condition1 condition2 state1.heap state2.heap resolve in
            let mergedStatics = merge2GeneralizedHeaps condition1 condition2 state1.statics state2.statics resolve in
            { state1 with stack = mergedStack; heap = mergedHeap; statics = mergedStatics }

    let internal mergeStates conditions states : State.state =
        assert(List.length states > 0)
        let first : State.state = List.head states in
        let frames = first.frames in
        let path = first.pc in
        assert(states |> List.forall (fun s -> s.frames = frames))
        assert(states |> List.forall (fun s -> s.pc = path))
        let mergedStack = Utils.MappedStack.merge conditions (List.map State.stackOf states) mergeCells (State.stackLazyInstantiator first) in
        let mergedHeap = mergeGeneralizedHeaps conditions (List.map State.heapOf states) in
        let mergedStatics = mergeGeneralizedHeaps conditions (List.map State.staticsOf states) in
        { stack = mergedStack; heap = mergedHeap; statics = mergedStatics; frames = frames; pc = path }

    let internal commonGuardedMapk mapper gvs merge k =
        let gs, vs = List.unzip gvs in
        Cps.List.mapk mapper vs (List.zip gs >> merge >> k)

    let internal guardedMapk mapper gvs k = commonGuardedMapk mapper gvs merge k

    let internal guardedMap mapper gvs = guardedMapk (Cps.ret mapper) gvs id

    let internal commonGuardedStateMapk mapper gvs state merge k =
        let gs, vs = List.unzip gvs in
        Cps.List.mapk (mapper state) vs (fun vsst ->
        let vs, states = List.unzip vsst in
        k (vs |> List.zip gs |> merge, mergeStates gs states))

    let internal guardedStateMapk mapper gvs state k = commonGuardedStateMapk mapper gvs state merge k

    let internal guardedStateMap mapper gvs state = guardedStateMapk (Cps.ret2 mapper) gvs state id

    let internal commonGuardedErroredMapk mapper errorMapper gvs state merge k =
        let ges, gvs = List.partition (snd >> IsError) gvs in
        let egs, es = List.unzip ges in
        let vgs, vs = List.unzip gvs in
        let eg = disjunction Metadata.empty egs in
        Cps.List.mapk (mapper state) vs (fun vsst ->
        let vs', states = List.unzip vsst in
        let ges' = es |> List.map errorMapper |> List.zip egs in
        let gvs' = List.zip vgs vs' |> List.append ges' |> merge in
        let state' = mergeStates (eg :: vgs) (state :: states) in
        k (gvs', state'))

    let internal guardedErroredMapk mapper errorMapper gvses state k = commonGuardedErroredMapk mapper errorMapper gvses state merge k

    let internal guardedErroredMap mapper errorMapper gvses state = guardedErroredMapk (Cps.ret2 mapper) errorMapper gvses state id

    let internal unguard = function
        | {term = Union gvs} -> gvs
        | t -> [(True, t)]

    let internal erroredUnguard term =
        let ges, gvs = term |> unguard |> List.partition (snd >> IsError) in
        ges, merge gvs

    let internal genericSimplify gvs =
        let rec loop gvs out =
            match gvs with
            | [] -> out
            | ((True, v) as gv)::gvs' -> [gv]
            | (False, v)::gvs' -> loop gvs' out
            | gv::gvs' -> loop gvs' (gv::out)
        loop gvs [] |> mergeSame

    let internal productUnion f t1 t2 =
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
                let g' = gacc &&& g in
                if IsError v then [(g', v)]
                else
                    guardedCartesianProductRec mapper ctor g' (List.append xsacc [v]) xs)
            |> List.concat |> genericSimplify


    let internal guardedCartesianProduct mapper ctor terms =
        guardedCartesianProductRec mapper ctor True [] terms

    let internal guardedApply f gvs =
        gvs |> List.map (fun (g, v) -> (g, if IsError v then v else f v))

    let internal map f t =
        match t.term with
        | Union gvs -> guardedApply f gvs |> merge
        | Error e -> e
        | _ -> f t
