namespace VSharp.Core

open VSharp

module internal Merging =

    type private MergeType =
        | StructMerge of termType
        | ClassMerge
        | ArrayMerge
        | BoolMerge
        | DefaultMerge

    let private mergeTypeOf term =
        match term.term with
        | Struct(_, t) -> StructMerge t
        | Class _ -> ClassMerge
        | Array _ -> ArrayMerge
        | _ when isBool term -> BoolMerge
        | _ -> DefaultMerge

    let guardOf term =
        match term.term with
        | GuardedValues(gs, _) -> disjunction term.metadata gs
        | _ -> makeTrue term.metadata

    let private readHeap = State.readHeap Metadata.empty
    let private readStatics = State.readStatics Metadata.empty
    let private readTerm = State.readTerm Metadata.empty

    let rec private boolMerge gvs =
        let guard = List.fold (|||) False (List.map fst gvs)
        let value = List.fold (fun acc (g, v) -> acc ||| (g &&& v)) False gvs
        [(guard, value)]

    and keysResolver<'a, 'b, 'c when 'c : equality> r (read : bool -> 'a -> 'b -> termType -> term) keyMapper getter (k : 'c memoryCell) hgvs =
        let key = keyMapper k
        let typ = baseTypeOfKey k
        let instIfShould = function
            | _, g, Some v -> (g, v)
            | i, g, _ -> (g, read r (getter i) key typ)
        merge <| List.map instIfShould hgvs

    and keysResolver2<'a, 'b, 'c, 'd when 'c : equality> r h1 h2 (read : bool -> 'a -> 'b -> termType -> term) keyMapper resolve (k : 'c memoryCell) v1 v2 : 'd =
        let read h =
            let key = keyMapper k
            let typ = baseTypeOfKey k
            read r h key typ
        match v1, v2 with
        | Some v1, Some v2 -> resolve v1 v2
        | Some v, _ -> resolve v (read h2)
        | _, Some v -> resolve (read h1) v
        | _, _ -> __unreachable__()

    and private blockMerge typ mtd gvs =
        let gs, vs = List.unzip gvs
        let fss = vs |> List.map fieldsOf
        let getter i = List.item i vs
        let mergedFields = keysResolver<term, fql, string> false readTerm getFQLOfKey getter |> Heap.merge gs fss
        [(Propositional.disjunction Metadata.empty gs, Block mtd mergedFields typ)]

    and private arrayMerge mtd gvs =
        let gs, vs = List.unzip gvs
        let extractArrayInfo = term >> function
            | Array(dim, len, lower, init, contents, lengths) -> (dim, len, lower, init, contents, lengths)
            | t -> internalfailf "Expected array, got %O" t
        let ds, lens, lows, inits, contents, lengths =
            vs
            |> Seq.map extractArrayInfo
            |> fun info -> Seq.foldBack (fun (d, l, lw, i, c, ls) (da, la, lwa, ia, ca, lsa) -> (d::da, l::la, lw::lwa, i::ia, c::ca, ls::lsa)) info ([], [], [], [], [], [])
        let d = List.unique ds
        let l = merge <| List.zip gs lens
        let getter i = List.item i vs
        let resolveKeys = keysResolver<term, fql, term> false readTerm getFQLOfKey getter
        let mergedLower =  Heap.merge gs lows resolveKeys
        let mergedContents = Heap.merge gs contents resolveKeys
        let mergedLengths = Heap.merge gs lengths resolveKeys
        let mergedInit = inits |> Seq.map2 (fun ng init -> Seq.map (fun (g, v) -> (ng &&& g, v)) init) gs |> Seq.concat |> List.ofSeq |> mergeSame
        [(Propositional.disjunction Metadata.empty gs, Array mtd d l mergedLower mergedInit mergedContents mergedLengths)]

    and private commonTypedMerge typedMerge = function
        | [] -> []
        | [(g, v)] as gvs ->
            match g with
            | True -> gvs
            | _ -> [propagateGuard g v]
        | (_, v) :: _ as gvs -> typedMerge v.metadata gvs

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

    and private typedMerge = function
        | BoolMerge -> commonTypedMerge (always boolMerge)
        | StructMerge typ -> commonTypedMerge (blockMerge (Some typ))
        | ClassMerge -> commonTypedMerge (blockMerge None)
        | ArrayMerge -> commonTypedMerge arrayMerge
        | DefaultMerge -> id

    and propagateGuard g v =
        match v.term with
        | Block(contents, typ) ->
            let contents' = Heap.map (fun (k, v) -> (k, merge [(g, v)])) contents
            (Terms.True, Block v.metadata contents' typ)
        | Array(dimension, len, lower, init, contents, lengths) ->
            let contents' = Heap.map (fun (k, v) -> (k, merge [(g, v)])) contents
            let lower' = Heap.map (fun (k, v) -> (k, merge [(g, v)])) lower
            let lengths' = Heap.map (fun (k, v) -> (k, merge [(g, v)])) lengths
            let init' = List.map (fun (gi, i) -> gi &&& g, i) init
            (Terms.True, Array v.metadata dimension len lower' init' contents' lengths')
        | _ -> (g, v)

    and private compress = function
        | [] -> []
        | [(_, v)] -> [True, v]
        | [(_, v1); (_, v2)] as gvs when mergeTypeOf v1 = mergeTypeOf v2 -> typedMerge (mergeTypeOf v1) (mergeSame gvs)
        | [_; _] as gvs -> gvs
        | gvs ->
            gvs
            |> mergeSame
            |> List.groupBy (snd >> mergeTypeOf)
            |> List.collect (fun (t, gvs) -> if List.length gvs >= 2 then typedMerge t gvs else gvs)

    and merge (gvs : (term * term) list) : term =
        match compress (simplify (|UnionT|_|) gvs) with
        | [(True, v)] -> v
        | [(g, v)] when Terms.isBool v -> g &&& v
        | gvs' -> Union Metadata.empty gvs'

    let merge2Terms g h u v =
        let g = guardOf u &&& g
        let h = guardOf v &&& h
        match g, h with
        | _, _ when u = v -> u
        | True, _
        | _, False -> u
        | False, _
        | _, True -> v
        | ErrorT _, _ -> g
        | _, ErrorT _ -> h
        | _ -> merge [(g, u); (h, v)]

// ---------------------------------------- Merging heaps ----------------------------------------

    let mergeDefinedHeaps restricted read guards heaps =
        let getter i = List.item i heaps
        Heap.merge guards heaps (keysResolver restricted read Heap.getKey getter)

    let mergeGeneralizedHeaps<'a when 'a : equality> read guards (heaps : list<'a generalizedHeap>) =
        let (|MergedHeap|_|) = function Merged gvs -> Some gvs | _ -> None
        let guardsAndHeaps = List.zip guards heaps |> simplify (|MergedHeap|_|)
        let Merged = function
            | [(True, x)] -> x
            | xs -> Merged xs
        // TODO: non-restricted heaps should be merged in a different way
        let defined, undefined =
            guardsAndHeaps
            |> List.mappedPartition (function (g, Defined(r, s)) -> Some(g, r, s) | _ -> None)
        if defined.IsEmpty then
            undefined |> mergeSame |> Merged
        else
            let definedGuards, restricted, definedHeaps = List.unzip3 defined
            let restricted = List.unique restricted
            let definedHeap = mergeDefinedHeaps restricted read definedGuards definedHeaps |> State.Defined restricted
            if undefined.IsEmpty then definedHeap
            else
                let definedGuard = disjunction Metadata.empty definedGuards
                (definedGuard, definedHeap)::undefined |> mergeSame |> Merged

    let private merge2GeneralizedHeaps g1 g2 h1 h2 read resolve =
        match h1, h2 with
        | Defined(r1, h1), Defined(r2, h2) ->
            assert(r1 = r2)
            Heap.merge2 h1 h2 (keysResolver2 r1 h1 h2 read Heap.getKey resolve) |> State.Defined r1
        | _ -> mergeGeneralizedHeaps read [g1; g2] [h1; h2]

    let private addToPathConditionIfNeed cond pc = if isTrue cond then pc else cond :: pc

    let merge2States condition1 condition2 state1 state2 =
        assert(state1.pc = state2.pc)
        assert(state1.frames = state2.frames)
        let mergedConditions = condition1 ||| condition2
        let mergedPC = addToPathConditionIfNeed mergedConditions state1.pc
        match condition1, condition2 with
        | True, _
        | _, False -> {state1 with pc = mergedPC}
        | _, True
        | False, _ -> {state2 with pc = mergedPC}
        | _ ->
            let resolve = merge2Terms condition1 condition2
            let mergedStack = Utils.MappedStack.merge2 state1.stack state2.stack resolve (State.stackLazyInstantiator state1)
            let mergedHeap = merge2GeneralizedHeaps condition1 condition2 state1.heap state2.heap readHeap resolve
            let mergedStatics = merge2GeneralizedHeaps condition1 condition2 state1.statics state2.statics readStatics resolve
            { state1 with stack = mergedStack; heap = mergedHeap; statics = mergedStatics; pc = mergedPC }

    let mergeStates conditions states =
        assert(List.length states > 0)
        let first = List.head states
        let frames = first.frames
        let path = first.pc
        let tv = first.typeVariables
        assert(states |> List.forall (fun s -> s.frames = frames))
        assert(states |> List.forall (fun s -> s.pc = path))
        assert(states |> List.forall (fun s -> s.typeVariables = tv))
        let mergedStack = Utils.MappedStack.merge conditions (List.map State.stackOf states) merge (State.stackLazyInstantiator first)
        let mergedHeap = mergeGeneralizedHeaps readHeap conditions (List.map State.heapOf states)
        let mergedStatics = mergeGeneralizedHeaps readStatics conditions (List.map State.staticsOf states)
        let mergedConditions = disjunction Metadata.empty conditions
        let mergedPC = if isTrue mergedConditions then path else mergedConditions :: path
        { stack = mergedStack; heap = mergedHeap; statics = mergedStatics; frames = frames; pc = mergedPC; typeVariables = tv }

    let unguard = function
        | {term = Union gvs} -> gvs
        | t -> [(True, t)]

    let conditionUnderState condition state =
        Propositional.conjunction condition.metadata (condition :: State.pathConditionOf state)

// ------------------------------------ Mapping non-term sequences ------------------------------------

    let guardedMapWithoutMerge f gvs =
        List.map (fun (g, v) -> (g, f v)) gvs

    let commonGuardedMapk mapper gvs merge k =
        let foldFunc gvs (g, v) k =
            mapper v (fun v' -> k ((g, v') :: gvs))
        Cps.List.foldlk foldFunc [] gvs (merge >> k)
    let guardedMap mapper gvs = commonGuardedMapk (Cps.ret mapper) gvs merge id

// ---------------------- Applying functions to terms and mapping term sequences ----------------------

    let commonGuardedErroredMapk mapper errorMapper gvs merge k =
        let foldFunc gvs (g, v) k =
            if isError v then k ((g, errorMapper v) :: gvs)
            else mapper v (fun t -> k ((g, t) :: gvs))
        Cps.List.foldlk foldFunc [] gvs (merge >> k)

    let commonGuardedErroredApplyk f errorHandler term merge k =
        match term.term with
        | Error _ -> errorHandler term |> k
        | Union gvs -> commonGuardedErroredMapk f errorHandler gvs merge k
        | _ -> f term k
    let commonGuardedErroredApply f errorHandler term merge = commonGuardedErroredApplyk (Cps.ret f) errorHandler term merge id

    let guardedErroredApplyk f term k = commonGuardedErroredApplyk f id term merge k
    let guardedErroredApply f term = guardedErroredApplyk (Cps.ret f) term id

    let commonGuardedErroredStatedMapk mapper errorMapper gvs state merge mergeStates k =
        let foldFunc (gvs, egs, vgs, states) (g, v) k =
            let pc = conditionUnderState g state
            match pc with
            | False -> k (gvs, egs, vgs, states)
            | _ when isError v -> k ((g, errorMapper v) :: gvs, g :: egs, vgs, states)
            |_ -> mapper (State.withPathCondition state g) v (fun (t, s) -> k ((g, t) :: gvs, egs, g :: vgs, State.popPathCondition s :: states))
        Cps.List.foldlk foldFunc ([], [], [], []) gvs (fun (gvs, egs, vgs, states) ->
        let eg = disjunction Metadata.empty egs
        let state' = mergeStates (eg :: vgs) (state :: states)
        k (merge gvs, state'))

    let commonGuardedErroredStatedApplyk f errorHandler state term merge mergeStates k =
        match term.term with
        | Error _ -> k (errorHandler term, state)
        | Union gvs -> commonGuardedErroredStatedMapk f errorHandler gvs state merge mergeStates k
        | _ -> f state term k
    let guardedErroredStatedApplyk f state term k = commonGuardedErroredStatedApplyk f id state term merge mergeStates k
    let guardedErroredStatedApply f state term = guardedErroredStatedApplyk (Cps.ret2 f) state term id

// ----------------------------------------------------------------------------------------------------

    let erroredUnguard term =
        let ges, gvs = term |> unguard |> List.partition (snd >> isError)
        ges, merge gvs

    let genericSimplify gvs : (term * 'a) list =
        let rec loop gvs out =
            match gvs with
            | [] -> out
            | ((True, _) as gv)::_ -> [gv]
            | (False, _)::gvs' -> loop gvs' out
            | gv::gvs' -> loop gvs' (gv::out)
        loop gvs [] |> mergeSame

    let rec private genericGuardedCartesianProductRec mapper gacc xsacc = function
        | x::xs ->
            mapper x
            |> List.collect (fun (g, v) ->
                genericGuardedCartesianProductRec mapper (gacc &&& g) (List.append xsacc [v]) xs)
            |> genericSimplify
        | [] -> [(gacc, xsacc)]
    let genericGuardedCartesianProduct mapper xs =
        genericGuardedCartesianProductRec mapper True [] xs

    let rec private guardedCartesianProductRec mapper ctor gacc xsacc = function
        | x::xs ->
            mapper x
            |> List.collect (fun (g, v) ->
                let g' = gacc &&& g
                if isError v then [(g', v)]
                else
                    guardedCartesianProductRec mapper ctor g' (List.append xsacc [v]) xs)
            |> genericSimplify
        | [] -> [(gacc, ctor xsacc)]

    let guardedCartesianProduct mapper terms ctor =
        guardedCartesianProductRec mapper ctor True [] terms
