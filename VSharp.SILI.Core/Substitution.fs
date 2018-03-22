namespace VSharp.Core

open VSharp

module Substitution =
    let rec substitute subst term =
        match term.term with
        | HeapRef(path, t, at, v) ->
            path |> NonEmptyList.toList |> substitutePath subst (fun path' ->
            let path'' = NonEmptyList.ofList path'
            if path'' = path then term else HeapView term.metadata path'' t at v)
            |> Merging.merge
        | StackRef(key, path, v) ->
            path |> substitutePath subst (fun path' ->
            if path' = path then term else StackView term.metadata key path' v)
            |> Merging.merge
        | StaticRef(key, path, v) ->
            path |> substitutePath subst (fun path' ->
            if path' = path then term else StaticView term.metadata key path' v)
            |> Merging.merge
        | Error e ->
            e |> substitute subst |> Merging.unguard |> Merging.guardedApply (fun e' ->
            if e' = e then term else Error term.metadata e')
            |> Merging.merge
        | Expression(op, args, t) ->
            args |> substituteMany subst (fun args' ->
            if args = args' then term
            else
                match op with
                | Operator(op, isChecked) -> Operators.simplifyOperation term.metadata op isChecked t args' id
                | Application _
                | Cast _ -> __notImplemented__())
            |> Merging.merge
        | Union gvs ->
            let gvs' = gvs |> List.map (fun (g, v) ->
                let ges, ggs = substitute subst g |> Merging.erroredUnguard
                (ggs, substitute subst v)::ges) |> List.concat
            if gvs' = gvs then term else Merging.merge gvs'
        | Struct(contents, typ) ->
            let contents', errs = substituteHeap subst contents
            let guard = errs |> List.fold (fun d (g, _) -> d ||| g) False
            (!!guard, Struct term.metadata contents' typ)::errs |> Merging.merge
        | Array(dim, len, lower, inst, contents, lengths, typ) ->
            let dimerrs, dim' = dim |> substitute subst |> Merging.erroredUnguard
            let lenerrs, len' = len |> substitute subst |> Merging.erroredUnguard
            let lower', lowererrs = substituteHeap subst lower
            let contents', contentserrs = substituteHeap subst contents
            let lengths', lengthserrs = substituteHeap subst lengths
            let insterrs, inst' =
                inst
                |> List.map (fun (g, i) ->
                    let ges, g' = g |> substitute subst |> Merging.erroredUnguard
                    let ges, gis =
                        match i with
                        | DefaultInstantiator _ -> ges, [(g, i)]
                        | LazyInstantiator(term, typ) ->
                            let ges', gts' = term |> substitute subst |> Merging.unguard |> List.partition (snd >> isError)
                            List.append ges ges', List.map (fun (g, t) -> (g' &&& g, LazyInstantiator(t, typ))) gts'
                    ges, Merging.genericSimplify gis)
                |> List.unzip
            let insterrs, inst' = List.concat insterrs, List.concat inst'
            let errs = List.concat [dimerrs; lenerrs; lowererrs; contentserrs; lengthserrs; insterrs]
            let guard = errs |> List.fold (fun d (g, _) -> d ||| g) False
            let result = Terms.Array term.metadata dim' len' lower' inst' contents' lengths' typ
            (guard, result)::errs |> Merging.merge
        | _ -> subst term

    and substituteHeap subst heap =
        Heap.mapFold (fun errs k cell ->
            let ges, v' = Merging.erroredUnguard cell.value
            ((k, {cell with value = substitute subst v'}), List.append errs ges)) [] heap

    and substituteMany subst ctor terms =
        terms |> Merging.guardedCartesianProduct (substitute subst >> Merging.unguard) ctor

    and substitutePath subst ctor path =
        let addrs, ts = List.unzip path
        addrs |> Merging.guardedCartesianProduct (substitute subst >> Merging.unguard) (fun addrs -> List.zip addrs ts |> ctor)
