namespace VSharp.Core

open VSharp

module Substitution =

    let substituteHeap keySubst valueSubst heap =
        Heap.mapFold (fun errs k v ->
            let ges, v' = Merging.erroredUnguard v
            ((keySubst k, valueSubst v'), List.append errs ges)) [] heap

    let rec substitute subst typeSubst term =
        match term.term with
        | Ref(topLevel, path) ->
            topLevel |> substituteRef subst typeSubst path (Ref term.metadata) |> Merging.merge |> subst
        | Ptr(topLevel, path, typ, shift) ->
            let ctor =
                match shift with
                | None -> fun tl path -> Ptr term.metadata tl path typ
                | Some shift ->
                    fun tl path ->
                        shift
                        |> substitute subst typeSubst
                        |> Merging.guardedErroredApply (IndentedPtr term.metadata tl path typ)
            topLevel |> substituteRef subst typeSubst path ctor |> Merging.merge
        | Error e ->
            e |> substitute subst typeSubst |> Merging.guardedErroredApply (fun e' ->
            if e' = e then term else Error term.metadata e')
        | Expression(op, args, t) ->
            let t = typeSubst t
            args |> substituteMany subst typeSubst (fun args' ->
            if args = args' then term
            else
                match op with
                | Operator(op, isChecked) -> Operators.simplifyOperation term.metadata op isChecked t args' id
                // TODO: this is temporary hack, support normal substitution cast expression
                | Cast _ -> Expression term.metadata op args' t
                | Application _ -> __notImplemented__())
            |> Merging.merge
        | Union gvs ->
            let gvs' = gvs |> List.collect (fun (g, v) ->
                let ges, ggs = substitute subst typeSubst g |> Merging.erroredUnguard
                if isFalse ggs then ges else (ggs, substitute subst typeSubst v)::ges)
            if gvs' = gvs then term else Merging.merge gvs'
        | Block(contents, typ) ->
            let contents', errs = substituteHeap id (substitute subst typeSubst) contents
            let guard = errs |> List.fold (fun d (g, _) -> d ||| g) False
            let typ' = Option.map typeSubst typ
            (!!guard, Block term.metadata contents' typ')::errs |> Merging.merge
        | Array(dim, len, lower, inst, contents, lengths) ->
            let dimerrs, dim' = dim |> substitute subst typeSubst |> Merging.erroredUnguard
            let lenerrs, len' = len |> substitute subst typeSubst |> Merging.erroredUnguard
            let lower', lowererrs = substituteHeap subst (substitute subst typeSubst) lower
            let contents', contentserrs = substituteHeap subst (substitute subst typeSubst) contents
            let lengths', lengthserrs = substituteHeap subst (substitute subst typeSubst) lengths
            let getErrorsAndInstors (ges, gis) (g, i) =
                let ges', g' = g |> substitute subst typeSubst |> Merging.erroredUnguard
                let gis' = Merging.genericSimplify [(g', i)]
                List.append ges ges', List.append gis gis'
            let insterrs, inst' = List.fold getErrorsAndInstors ([], []) inst
            let errs = List.concat [dimerrs; lenerrs; lowererrs; contentserrs; lengthserrs; insterrs]
            let guard = errs |> List.fold (fun d (g, _) -> d ||| g) False
            let result = Array term.metadata dim' len' lower' inst' contents' lengths'
            (!!guard, result)::errs |> Merging.merge
        | _ -> subst term

    and private substituteMany subst typeSubst ctor terms =
        terms |> Merging.guardedCartesianProduct (substitute subst typeSubst >> Merging.unguard) ctor

    and private substituteAndMap subst typeSubst mapper =
        substitute subst typeSubst >> Merging.unguard >> Merging.guardedMapWithoutMerge mapper

    and private substituteSegment subst typeSubst = function
        | BlockField(f, t) -> ([True, BlockField(f, typeSubst t)])
        | ArrayIndex(i, t) ->
            let t' = typeSubst t
            substituteAndMap subst typeSubst (fun i' -> ArrayIndex(i', t')) i
        | ArrayLowerBound i ->
            substituteAndMap subst typeSubst ArrayLowerBound i
        | ArrayLength i ->
            substituteAndMap subst typeSubst ArrayLength i

    and private substitutePath subst typeSubst ctor path =
        path |> Merging.genericGuardedCartesianProduct (substituteSegment subst typeSubst) ctor

    and substituteRef subst typeSubst path ctor topLevel = // TODO: add substitution of FQL
        path |> substitutePath subst typeSubst (fun path' ->
            match topLevel with
            | TopLevelHeap(addr, bt, st) ->
                let bt' = typeSubst bt
                let st' = typeSubst st
                addr
                |> substitute subst typeSubst
                |> Merging.guardedErroredApply (fun addr' -> ctor (TopLevelHeap(addr', bt', st')) path')
            | TopLevelStatics typ -> ctor (typ |> typeSubst |> TopLevelStatics) path'
            | NullAddress
            | TopLevelStack _ as tl -> ctor tl path')
