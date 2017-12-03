namespace VSharp

open VSharp.Terms
open VSharp.Types.Constructor
open Hierarchy

module internal Common =

// ------------------------------- Simplification -------------------------------

    let internal simplifyPairwiseCombinations = Propositional.simplifyPairwiseCombinations

    let internal simplifyConcreteBinary simplify mtd isChecked t x y xval yval _ _ state =
        simplify (Metadata.combine3 mtd x.metadata y.metadata) isChecked state t xval yval

    let rec internal simplifyGenericUnary name state x matched concrete unmatched =
        match x.term with
        | Error _ -> matched (x, state)
        | Concrete(xval, typeofX) -> concrete x xval typeofX state |> matched
        | GuardedValues(guards, values) ->
            Cps.List.mapFoldk (fun state term matched -> simplifyGenericUnary name state term matched concrete unmatched) state values (fun (values', state) ->
                (Merging.merge (List.zip guards values'), state) |> matched)
        | _ -> unmatched x state matched

    let rec internal simplifyGenericBinary name state x y matched concrete unmatched repeat =
        match x.term, y.term with
        | Error _, _ -> matched (x, state)
        | _, Error _ -> matched (y, state)
        | Concrete(xval, typeOfX), Concrete(yval, typeOfY) -> concrete x y xval yval typeOfX typeOfY state |> matched
        | Union(gvsx), Union(gvsy) ->
            let compose (gx, vx) state (gy, vy) matched = repeat vx vy state (fun (xy, state) -> ((gx &&& gy, xy), state) |> matched) in
                let join state (gx, vx) k = Cps.List.mapFoldk (compose (gx, vx)) state gvsy k in
                    Cps.List.mapFoldk join state gvsx (fun (gvss, state) -> (Merging.merge (List.concat gvss), state) |> matched)
        | GuardedValues(guardsX, valuesX), _ ->
            Cps.List.mapFoldk (fun state x matched -> repeat x y state matched) state valuesX (fun (values', state) ->
            (Merging.merge (List.zip guardsX values'), state) |> matched)
        | _, GuardedValues(guardsY, valuesY) ->
            Cps.List.mapFoldk (fun state y matched -> repeat x y state matched) state valuesY (fun (values', state) ->
            (Merging.merge (List.zip guardsY values'), state) |> matched)
        | _ -> unmatched x y state matched

// ------------------------------- Type casting -------------------------------

    type private SymbolicTypeSource(t : TermType) =
        inherit SymbolicConstantSource()
        override x.SubTerms = Seq.empty

    let rec is metadata leftType rightType =
        let makeBoolConst name termType = Constant Metadata.empty name (SymbolicTypeSource termType) Bool
        in
        let concreteIs (dotNetTypeHierarchy : Hierarchy) rightTermType = function
            | ReferenceType(t, _, _)
            | StructureType(t, _ ,_) -> Terms.MakeBool (t.Equals dotNetTypeHierarchy) metadata
            | SubType(t, _, _, name) as termType when dotNetTypeHierarchy.Is t ->
                let b = makeBoolConst (dotNetTypeHierarchy.Name) rightTermType in
                implies (makeBoolConst name termType) b metadata
            | ArrayType(_, SymbolicDimension name) as termType ->
                let b = makeBoolConst (dotNetTypeHierarchy.Name) rightTermType in
                implies (makeBoolConst name termType) b metadata
            | SubType(t, _, _, _) when not <| dotNetTypeHierarchy.Is t -> Terms.MakeFalse metadata
            // TODO: squash all Terms.MakeFalse into default case and get rid of __notImplemented__()
            | PointerType _ -> Terms.MakeFalse metadata
            | _ -> __notImplemented__()
        in
        let subTypeIs (dotNetTypeHierarchy : Hierarchy) rightTermType rightName = function
            | ReferenceType(t, _, _) -> Terms.MakeBool (t.Is dotNetTypeHierarchy) metadata
            | StructureType _ -> Terms.MakeBool (Hierarchy(typedefof<System.ValueType>).Is dotNetTypeHierarchy) metadata
            | SubType(t, _, _, _) when t.Is dotNetTypeHierarchy -> Terms.MakeTrue metadata
            | SubType(t, _, _, name) as termType when dotNetTypeHierarchy.Is t ->
                implies (makeBoolConst name termType) (makeBoolConst rightName rightTermType) metadata
            | ArrayType _ -> Terms.MakeBool (dotNetTypeHierarchy.Equals typedefof<obj>) metadata
            | _ -> __notImplemented__()
        in
        match leftType, rightType with
        | TermType.Null, _
        | Void, _   | _, Void
        | Bottom, _ | _, Bottom -> Terms.MakeFalse metadata
        | PointerType left, PointerType right -> Terms.MakeTrue metadata
        | Func _, Func _ -> Terms.MakeTrue metadata
        | ArrayType(t1, c1), ArrayType(_, SymbolicDimension _) -> Terms.MakeTrue metadata
        | ArrayType(t1, ConcreteDimension c1), ArrayType(t2, ConcreteDimension c2) -> if c1 = c2 then is metadata t1 t2 else Terms.MakeFalse metadata
        | _, StructureType(t, _, _)
        | _, ReferenceType(t, _, _) -> concreteIs t rightType leftType
        | _, SubType(t, _, _, name) -> subTypeIs t rightType name leftType
        | _ -> Terms.MakeFalse metadata

// ------------------------------- Branching -------------------------------

    let internal simpleConditionalExecution conditionInvocation thenBranch elseBranch merge merge2 k =
        let execution condition k =
            thenBranch (fun thenResult ->
            elseBranch (fun elseResult ->
            k <| merge2 condition !!condition thenResult elseResult))
        in
        conditionInvocation (fun condition ->
        match condition with
        | Terms.True ->  thenBranch k
        | Terms.False -> elseBranch k
        | Terms.ErrorT e -> k condition
        | UnionT gvs -> Merging.commonGuardedMapk execution gvs merge k
        | _ -> execution condition k)

    let internal reduceConditionalExecution (state : State.state) conditionInvocation thenBranch elseBranch merge merge2 errorHandler k =
        let execution conditionState condition k =
            thenBranch (State.withPathCondition conditionState condition) (fun (thenResult, thenState) ->
            elseBranch (State.withPathCondition conditionState !!condition) (fun (elseResult, elseState) ->
            let result = merge2 condition !!condition thenResult elseResult in
            let state = Merging.merge2States condition !!condition (State.popPathCondition thenState) (State.popPathCondition elseState) in
            k (result, state)))
        in
        conditionInvocation state (fun (condition, conditionState) ->
        let thenCondition =
            Propositional.conjunction condition.metadata (condition :: State.pathConditionOf conditionState)
            |> Merging.unguard |> Merging.merge
        in
        let elseCondition =
            Propositional.conjunction condition.metadata (!!condition :: State.pathConditionOf conditionState)
            |> Merging.unguard |> Merging.merge
        in
        match thenCondition, elseCondition, condition with
        | False, _, _ -> elseBranch conditionState k
        | _, False, _ -> thenBranch conditionState k
        | _, _, (Terms.ErrorT _ as e) -> k (errorHandler e, conditionState)
        | _, _, UnionT gvs -> Merging.commonGuardedErroredMapk execution errorHandler gvs conditionState merge k
        | _ -> execution conditionState condition k)

// ------------------------------- Substitution -------------------------------

    let rec internal substitute subst term =
        match term.term with
        | HeapRef(path, t) ->
            path |> NonEmptyList.toList |> substitutePath subst (fun path' ->
            let path'' = NonEmptyList.ofList path' in
            if path'' = path then term else HeapRef term.metadata path'' t)
            |> Merging.merge
        | StackRef(key, path) ->
            path |> substitutePath subst (fun path' ->
            if path' = path then term else StackRef term.metadata key path')
            |> Merging.merge
        | StaticRef(key, path) ->
            path |> substitutePath subst (fun path' ->
            if path' = path then term else StaticRef term.metadata key path')
            |> Merging.merge
        | Error e ->
            e |> substitute subst |> Merging.unguard |> Merging.guardedApply (fun e' ->
            if e' = e then term else Error term.metadata e')
            |> Merging.merge
        | Expression(op, args, t) ->
            args |> substituteMany subst (fun args' ->
            if args = args' then term else Expression term.metadata op args' t)
            |> Merging.merge
        | Union gvs ->
            let gvs' = gvs |> List.map (fun (g, v) ->
                let ges, ggs = substitute subst g |> Merging.erroredUnguard in
                (ggs, substitute subst v)::ges) |> List.concat
            in
            if gvs' = gvs then term else Merging.merge gvs'
        | Struct(contents, typ) ->
            let contents', errs = substituteHeap subst contents in
            let guard = errs |> List.fold (fun d (g, _) -> d ||| g) False in
            (!!guard, Struct term.metadata contents' typ)::errs |> Merging.merge
        | Array(dim, len, lower, inst, contents, lengths, typ) ->
            let dimerrs, dim' = dim |> substitute subst |> Merging.erroredUnguard in
            let lenerrs, len' = len |> substitute subst |> Merging.erroredUnguard in
            let lower', lowererrs = substituteHeap subst lower in
            let contents', contentserrs = substituteHeap subst contents in
            let lengths', lengthserrs = substituteHeap subst lengths in
            let insterrs, inst' =
                inst
                |> List.map (fun (g, i) ->
                    let ges, g' = g |> substitute subst |> Merging.erroredUnguard in
                    let ges, gis =
                        match i with
                        | DefaultInstantiator _ -> ges, [(g, i)]
                        | LazyInstantiator(term, typ) ->
                            let ges', gts' = term |> substitute subst |> Merging.unguard |> List.partition (snd >> IsError) in
                            List.append ges ges', List.map (fun (g, t) -> (g' &&& g, LazyInstantiator(t, typ))) gts'
                    in ges, Merging.genericSimplify gis)
                |> List.unzip
            in
            let insterrs, inst' = List.concat insterrs, List.concat inst' in
            let errs = List.concat [dimerrs; lenerrs; lowererrs; contentserrs; lengthserrs; insterrs] in
            Array term.metadata dim' len' lower' inst' contents' lengths' typ
        | _ -> subst term

    and internal substituteHeap subst heap =
        Heap.mapFold (fun errs k (v, c, m) ->
            let ges, v' = Merging.erroredUnguard v in
            ((k, (substitute subst v', c, m)), List.append errs ges)) [] heap

    and internal substituteMany subst ctor terms =
        terms |> Merging.guardedCartesianProduct (substitute subst >> Merging.unguard) ctor

    and internal substitutePath subst ctor path =
        let addrs, ts = List.unzip path in
        addrs |> Merging.guardedCartesianProduct (substitute subst >> Merging.unguard) (fun addrs -> List.zip addrs ts |> ctor)
