namespace VSharp.Core

open VSharp

module Substitution =

    let private substituteRefTopLevel addressSubst typeSubst = function
        | RefTopLevelHeap(addr, bt, st) -> addressSubst addr bt st
        | RefTopLevelStatics typ -> [True, typ |> typeSubst |> RefTopLevelStatics]
        | RefNullAddress
        | RefTopLevelStack _ as tl -> [True, tl]

    let rec substituteHeap<'a when 'a : equality> keySubst subst addressSubst typeSubst (heap : 'a heap) : 'a heap =
        Heap.map (fun (k, v) -> substituteHeapKey keySubst subst addressSubst typeSubst k, substitute subst addressSubst typeSubst v) heap

    and substituteHeapKey<'a when 'a : equality> (keySubst : 'a -> 'a) (subst : term -> term) (addressSubst : term -> termType -> termType -> (term * refTopLevelAddress) list) (typeSubst : termType -> termType) (key : 'a memoryCell) : 'a memoryCell =
        let key' = keySubst key.key // TODO: key substitution works twice (in key.key and in key.FQL)
        let FQL' = Option.map (substituteHeapFQL subst addressSubst typeSubst) key.FQL
        match FQL' with
        | None -> {key = key'; FQL = None; typ = typeSubst key.typ}
        | Some [True, fql] -> {key = key'; FQL = Some fql; typ = baseTypeOfFQL fql} // we should never have union in heap key after substitution, so it shouldn't happen in fql
        | _ -> internalfail "substitution of heap key has failed"

    and substitute subst addressSubst typeSubst term =
        match term.term with
        | Ref(topLevel, path) ->
            substituteRef subst addressSubst typeSubst topLevel path (Ref term.metadata) |> Merging.merge |> subst
        | Ptr(topLevel, path, typ, shift) ->
            let ctor =
                match shift with
                | None -> fun tl path -> Ptr term.metadata tl path typ
                | Some shift ->
                    fun tl path ->
                        shift
                        |> substitute subst addressSubst typeSubst
                        |> Merging.guardedApply (IndentedPtr term.metadata tl path typ)
            substituteRef subst addressSubst typeSubst topLevel path ctor |> Merging.merge
        | Expression(op, args, t) ->
            let t = typeSubst t
            substituteMany subst addressSubst typeSubst args (fun args' ->
            if args = args' then term
            else
                match op with
                | Operator(op) -> Operators.simplifyOperation term.metadata op t args' id
                | Cast(_, targetType) ->
                    assert(List.length args' = 1)
                    let arg = List.head args'
                    TypeCasting.cast term.metadata arg targetType id
                | Application _ -> __notImplemented__())
            |> Merging.merge
        | Union gvs ->
            let gvs' = gvs |> List.choose (fun (g, v) ->
                let ggs = substitute subst addressSubst typeSubst g |> Merging.unguardMerge
                if isFalse ggs then None else Some (ggs, substitute subst addressSubst typeSubst v))
            if gvs' = gvs then term else Merging.merge gvs'
        | Block(contents, typ) ->
            let contents' = substituteHeap id subst addressSubst typeSubst contents
            let typ' = Option.map typeSubst typ
            Block term.metadata contents' typ'
        | Array(dim, len, lower, inst, contents, lengths) ->
            let dim' = dim |> substitute subst addressSubst typeSubst |> Merging.unguardMerge
            let len' = len |> substitute subst addressSubst typeSubst |> Merging.unguardMerge
            let lower' = substituteHeap subst subst addressSubst typeSubst lower
            let contents' = substituteHeap subst subst addressSubst typeSubst contents
            let lengths' = substituteHeap subst subst addressSubst typeSubst lengths
            let getInstors (g, i) =
                let g' = g |> substitute subst addressSubst typeSubst |> Merging.unguardMerge
                Merging.genericSimplify [(g', i)]
            let inst' = List.collect getInstors inst
            Array term.metadata dim' len' lower' inst' contents' lengths'
        | _ -> subst term

    and private substituteMany subst addressSubst typeSubst terms ctor =
        Merging.guardedCartesianProduct (substitute subst addressSubst typeSubst >> Merging.unguard) terms ctor

    and private substituteAndMap subst addressSubst typeSubst mapper =
        substitute subst addressSubst typeSubst >> Merging.unguard >> Merging.guardedMapWithoutMerge mapper

    and private substituteSegment subst addressSubst typeSubst = function
        | BlockField(f, t) -> [True, BlockField(f, typeSubst t)]
        | ArrayIndex(i, t) ->
            let t' = typeSubst t
            substituteAndMap subst addressSubst typeSubst (fun i' -> ArrayIndex(i', t')) i
        | ArrayLowerBound i ->
            substituteAndMap subst addressSubst typeSubst ArrayLowerBound i
        | ArrayLength i ->
            substituteAndMap subst addressSubst typeSubst ArrayLength i

    and private substitutePath subst addressSubst typeSubst path =
        Merging.genericGuardedCartesianProduct (substituteSegment subst addressSubst typeSubst) path

    and private substituteRefFQL subst (addressSubst : term -> termType -> termType -> (term * refTopLevelAddress) list) typeSubst (topLevel, path) =
        let tls = substituteRefTopLevel addressSubst typeSubst topLevel
        let paths = substitutePath subst addressSubst typeSubst path
        let createFQL (g, tl) = List.map (fun (g', path) -> g &&& g', (tl, path)) paths
        List.collect createFQL tls

    and private substituteHeapFQL subst (addressSubst : term -> termType -> termType -> (term * refTopLevelAddress) list) typeSubst (topLevel, path) =
        substituteRefFQL subst addressSubst typeSubst (topLevel.ConvertToRefTopLevel(), path) |> List.map (fun (g, (tl, path)) -> (g, (tl.ConvertToHeapTopLevel(), path)))

    and private substituteRef subst addressSubst typeSubst topLevel path ctor =
        let FQL' = substituteRefFQL subst addressSubst typeSubst (topLevel, path)
        List.map (fun (g, (tl, path)) -> g, ctor tl path) FQL'
