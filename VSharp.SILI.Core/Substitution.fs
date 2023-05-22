namespace VSharp.Core

open VSharp

module Substitution =

    let rec substituteAddress termSubst typeSubst timeSubst = function
        | PrimitiveStackLocation _ as sl -> sl
        | ClassField(addr, field) -> ClassField(termSubst addr, field)
        | ArrayIndex(addr, index, (elementType, dim, isVector)) ->
            ArrayIndex(termSubst addr, List.map termSubst index, (typeSubst elementType, dim, isVector))
        | StructField(addr, field) -> StructField(substituteAddress termSubst typeSubst timeSubst addr, field)
        | StaticField(typ, field) -> StaticField(typeSubst typ, field)
        | ArrayLength(addr, dim, (typ, d, isVector)) -> ArrayLength(termSubst addr, termSubst dim, (typeSubst typ, d, isVector))
        | BoxedLocation(addr, typ) -> BoxedLocation(timeSubst addr, typeSubst typ)
        | StackBufferIndex(key, index)  -> StackBufferIndex(key, termSubst index)
        | ArrayLowerBound(addr, dim, (typ, d, isVector)) -> ArrayLowerBound(termSubst addr, termSubst dim, (typeSubst typ, d, isVector))

    let substitutePointerBase termSubst typeSubst = function
        | HeapLocation(loc, typ) -> HeapLocation(termSubst loc, typeSubst typ)
        | StaticLocation loc -> StaticLocation (typeSubst loc)
        | StackLocation _ as sl -> sl

    // TODO: get rid of union unnesting to avoid the exponential blow-up!
    let rec substitute termSubst typeSubst timeSubst term =
        let recur = substitute termSubst typeSubst timeSubst
        match term.term with
        | Expression(op, args, t) ->
            let t' = typeSubst t
            substituteMany termSubst typeSubst timeSubst args (fun args' ->
            if args = args' then term
            else
                match op with
                | Operator op ->
                    let term = simplifyOperation op args' id
                    primitiveCast term t'
                | Cast(_, targetType) ->
                    assert(List.length args' = 1)
                    let arg = List.head args'
                    primitiveCast arg (typeSubst targetType)
                | Application f -> standardFunction args' f
                | Combine -> combine args' t')
            |> Merging.merge
        | Union gvs ->
            let gvs' = gvs |> List.choose (fun (g, v) ->
                let ggs = recur g |> Merging.unguardMerge
                if isFalse ggs then None else Some (ggs, recur v))
            if gvs' = gvs then term else Merging.merge gvs'
        | HeapRef(address, typ) ->
            let addr' = recur address
            let typ' = typeSubst typ
            HeapRef addr' typ'
        | Struct(contents, typ) ->
            let contents' = PersistentDict.map id recur contents
            let typ' = typeSubst typ
            Struct contents' typ'
        | ConcreteHeapAddress addr -> ConcreteHeapAddress (timeSubst addr)
        | Ref address -> substituteAddress recur typeSubst timeSubst address |> Ref
        | Ptr(address, typ, shift) -> Ptr (substitutePointerBase recur typeSubst address) (typeSubst typ) (recur shift)
        | Slice(term, s, e, pos) -> Slice (recur term) (recur s) (recur e) (recur pos)
        | _ -> termSubst term

    and private substituteMany termSubst typeSubst timeSubst terms ctor =
        Merging.guardedCartesianProduct (substitute termSubst typeSubst timeSubst >> Merging.unguard) terms ctor

    and private substituteAndMap subst addressSubst typeSubst mapper =
        substitute subst addressSubst typeSubst >> Merging.unguard >> Merging.guardedMapWithoutMerge mapper
