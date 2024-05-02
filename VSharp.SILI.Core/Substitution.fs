namespace VSharp.Core

open VSharp

module Substitution =

    let rec private substituteAddress termSubst typeSubst address k =
        match address with
        | PrimitiveStackLocation _ as sl -> k sl
        | ClassField(addr, field) ->
            termSubst addr (fun addr' ->
            ClassField(addr', field) |> k)
        | ArrayIndex(addr, index, ({elemType = elementType} as arrayType)) ->
            let arrayType = { arrayType with elemType = typeSubst elementType }
            termSubst addr (fun addr' ->
            Cps.List.mapk termSubst index (fun index' ->
            ArrayIndex(addr', index', arrayType) |> k))
        | StructField(addr, field) ->
            substituteAddress termSubst typeSubst addr (fun addr' ->
            StructField(addr', field) |> k)
        | StaticField(typ, field) -> StaticField(typeSubst typ, field) |> k
        | ArrayLength(addr, dim, ({elemType = elementType} as arrayType)) ->
            let arrayType = { arrayType with elemType = typeSubst elementType }
            termSubst addr (fun addr' ->
            termSubst dim (fun dim' ->
            ArrayLength(addr', dim', arrayType) |> k))
        | BoxedLocation(addr, typ) ->
            termSubst addr (fun addr' ->
            BoxedLocation(addr', typeSubst typ) |> k)
        | StackBufferIndex(key, index)  ->
            termSubst index (fun index' ->
            StackBufferIndex(key, index') |> k)
        | ArrayLowerBound(addr, dim, ({elemType = elementType} as arrayType)) ->
            let arrayType = { arrayType with elemType = typeSubst elementType }
            termSubst addr (fun addr' ->
            termSubst dim (fun dim' ->
            ArrayLowerBound(addr', dim', arrayType) |> k))

    let private substitutePointerBase termSubst typeSubst pointerBase k =
        match pointerBase with
        | HeapLocation(loc, typ) ->
            termSubst loc (fun loc' ->
            HeapLocation(loc', typeSubst typ) |> k)
        | StaticLocation loc -> StaticLocation (typeSubst loc) |> k
        | StackLocation _ as sl -> k sl

    // TODO: get rid of union unnesting to avoid the exponential blow-up!
    let rec private substituteK termSubst typeSubst timeSubst term k =
        let recur = substituteK termSubst typeSubst timeSubst
        match term.term with
        | Expression(op, args, t) ->
            let t' = typeSubst t
            let ctor args' =
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
                    | Combine -> combine args' t'
            substituteManyK termSubst typeSubst timeSubst args ctor (fun gvs ->
            match gvs with
            | [(_, v)] -> {elseValue = v; branches = []}
            | gvs -> iteType.FromGvs gvs
            |> Merging.merge |> k)
        | Ite iteType ->
            let tryAdd (g, v) k =
                recur g (fun g' ->
                let ggs = Merging.unguardMerge g'
                if isFalse ggs then k None
                else
                    recur v (fun v' ->
                    Some (ggs, v') |> k))
            Cps.List.choosek tryAdd iteType.branches (fun branches' ->
            recur iteType.elseValue (fun e' ->
            let iteType' = {branches = branches'; elseValue = e'}
            if iteType = iteType' then k term else Merging.merge iteType' |> k))
        | HeapRef(address, typ) ->
            recur address (fun addr' ->
            let typ' = typeSubst typ
            HeapRef addr' typ' |> k)
        | Struct(contents, typ) ->
            let contents = PersistentDict.toSeq contents
            let substContents (key, value) k =
                recur value (fun value' ->
                k (key, value'))
            Cps.Seq.mapk substContents contents (fun contents' ->
            let contents' = PersistentDict.ofSeq contents'
            let typ' = typeSubst typ
            Struct contents' typ' |> k)
        | ConcreteHeapAddress addr -> ConcreteHeapAddress (timeSubst addr) |> k
        | Ref address ->
            substituteAddress recur typeSubst address (fun address' ->
            Ref address' |> k)
        | Ptr(address, typ, shift) ->
            substitutePointerBase recur typeSubst address (fun address' ->
            recur shift (fun shift' ->
            Ptr address' (typeSubst typ) shift' |> k))
        | Slice(part, slices) ->
            let substSlices (s, e, pos, isWrite) k =
                recur s (fun s' ->
                recur e (fun e' ->
                recur pos (fun pos' ->
                k (s', e', pos', isWrite))))
            Cps.List.mapk substSlices slices (fun slices' ->
            recur part (fun part' ->
            createSlice part' slices' |> k))
        | _ -> termSubst term |> k

    and private substituteManyK termSubst typeSubst timeSubst terms ctor k =
        let subst term k =
            substituteK termSubst typeSubst timeSubst term (fun term' ->
            Merging.unguard term' |> k)
        Merging.guardedCartesianProductK subst terms ctor k

    and substitute termSubst typeSubst timeSubst term =
        substituteK termSubst typeSubst timeSubst term id
