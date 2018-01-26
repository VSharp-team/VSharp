namespace VSharp.Core

open VSharp
open VSharp.Core.Types.Constructor

module internal Common =
    open System

// ------------------------------- Simplification -------------------------------

    let simplifyPairwiseCombinations = Propositional.simplifyPairwiseCombinations

    let simplifyConcreteBinary simplify mtd isChecked t x y xval yval _ _ state =
        simplify (Metadata.combine3 mtd x.metadata y.metadata) isChecked state t xval yval

    let rec simplifyGenericUnary name state x matched concrete unmatched =
        match x.term with
        | Error _ -> matched (x, state)
        | Concrete(xval, typeofX) -> concrete x xval typeofX state |> matched
        | GuardedValues(guards, values) ->
            Cps.List.mapFoldk (fun state term matched -> simplifyGenericUnary name state term matched concrete unmatched) state values (fun (values', state) ->
                (Merging.merge (List.zip guards values'), state) |> matched)
        | _ -> unmatched x state matched

    let rec simplifyGenericBinary _ state x y matched concrete unmatched repeat =
        match x.term, y.term with
        | Error _, _ -> matched (x, state)
        | _, Error _ -> matched (y, state)
        | Concrete(xval, typeOfX), Concrete(yval, typeOfY) -> concrete x y xval yval typeOfX typeOfY state |> matched
        | Union(gvsx), Union(gvsy) ->
            let compose (gx, vx) state (gy, vy) matched = repeat vx vy state (fun (xy, state) -> ((gx &&& gy, xy), state) |> matched)
            let join state (gx, vx) k = Cps.List.mapFoldk (compose (gx, vx)) state gvsy k
            Cps.List.mapFoldk join state gvsx (fun (gvss, state) -> (Merging.merge (List.concat gvss), state) |> matched)
        | GuardedValues(guardsX, valuesX), _ ->
            Cps.List.mapFoldk (fun state x matched -> repeat x y state matched) state valuesX (fun (values', state) ->
            (Merging.merge (List.zip guardsX values'), state) |> matched)
        | _, GuardedValues(guardsY, valuesY) ->
            Cps.List.mapFoldk (fun state y matched -> repeat x y state matched) state valuesY (fun (values', state) ->
            (Merging.merge (List.zip guardsY values'), state) |> matched)
        | _ -> unmatched x y state matched

// ------------------------------- Type casting -------------------------------

    // TODO: support composition for this constant source
    [<StructuralEquality;NoComparison>]
    type private SymbolicSubtypeSource =
        {left : termType; right : termType}
        interface ISymbolicConstantSource with
            override x.SubTerms = Seq.empty

    let rec is metadata leftType rightType =
        let subtypeName lname rname = sprintf  "(%s <: %s)" lname rname
        let makeBoolConst lname rname leftTermType rightTermType =
            Constant metadata (subtypeName lname rname) ({left = leftTermType; right = rightTermType} : SymbolicSubtypeSource) Bool
        match leftType, rightType with
        | _ when leftType = rightType -> makeTrue metadata
        | termType.Null, _
        | Void, _   | _, Void
        | Bottom, _ | _, Bottom -> makeFalse metadata
        | Reference _, Reference _ -> makeTrue metadata
        | Pointer _, Pointer _ -> makeTrue metadata
        | Func _, Func _ -> makeTrue metadata
        | ArrayType _ as t1, (ArrayType(_, SymbolicDimension name) as t2) ->
            if name = "System.Array" then makeTrue metadata else makeBoolConst (t1.ToString()) (t2.ToString()) t1 t2
        | ArrayType(_, SymbolicDimension _) as t1, (ArrayType _ as t2)  when t1 <> t2 ->
            makeBoolConst (t1.ToString()) (t2.ToString()) t1 t2
        | ArrayType(t1, ConcreteDimension d1), ArrayType(t2, ConcreteDimension d2) ->
            if d1 = d2 then is metadata t1 t2 else makeFalse metadata
        | TypeVariable(Explicit (_, t)) as t1, t2 ->
            (is metadata t t2 ||| is metadata t2 t) &&& makeBoolConst (t1.ToString()) (t2.ToString()) t1 t2
        | t1, (TypeVariable(Explicit (_, t)) as t2) ->
            is metadata t1 t &&& makeBoolConst (t1.ToString()) (t2.ToString()) t1 t2
        | ConcreteType lt as t1, (ConcreteType rt as t2) ->
            if lt.IsGround && rt.IsGround
                then makeBool (lt.Is rt) metadata
                else if lt.Is rt then makeTrue metadata else makeBoolConst (t1.ToString()) (t2.ToString()) t1 t2
        | _ -> makeFalse metadata

    // TODO: support composition for this constant source
    [<StructuralEquality;NoComparison>]
    type private IsValueTypeConstantSource =
        {termType : termType}
        interface ISymbolicConstantSource with
            override x.SubTerms = Seq.empty

    let internal isValueType metadata termType =
        let makeBoolConst name = Constant metadata (sprintf "IsValueType(%s)" name) ({termType = termType} : IsValueTypeConstantSource) Bool
        match termType with
        | ConcreteType t when t.Inheritor.IsValueType -> makeTrue metadata
        | TypeVariable(Explicit(name, t)) ->
            if (Types.toDotNetType t).IsValueType
                then makeBoolConst name
                else makeFalse metadata
        | _ -> makeFalse metadata

// ------------------------------- Branching -------------------------------

    let simpleConditionalExecution conditionInvocation thenBranch elseBranch merge merge2 k =
        let execution condition k =
            thenBranch (fun thenResult ->
            elseBranch (fun elseResult ->
            k <| merge2 condition !!condition thenResult elseResult))
        conditionInvocation (fun condition ->
        match condition with
        | Terms.True ->  thenBranch k
        | Terms.False -> elseBranch k
        | Terms.ErrorT _ -> k condition
        | UnionT gvs -> Merging.commonGuardedMapk execution gvs merge k
        | _ -> execution condition k)

    let reduceConditionalExecution (state : state) conditionInvocation thenBranch elseBranch merge merge2 errorHandler k =
        let execution conditionState condition k =
            thenBranch (State.withPathCondition conditionState condition) (fun (thenResult, thenState) ->
            elseBranch (State.withPathCondition conditionState !!condition) (fun (elseResult, elseState) ->
            let result = merge2 condition !!condition thenResult elseResult
            let state = Merging.merge2States condition !!condition (State.popPathCondition thenState) (State.popPathCondition elseState)
            k (result, state)))
        conditionInvocation state (fun (condition, conditionState) ->
        let thenCondition =
            Propositional.conjunction condition.metadata (condition :: State.pathConditionOf conditionState)
            |> Merging.unguard |> Merging.merge
        let elseCondition =
            Propositional.conjunction condition.metadata (!!condition :: State.pathConditionOf conditionState)
            |> Merging.unguard |> Merging.merge
        match thenCondition, elseCondition, condition with
        | False, _, _ -> elseBranch conditionState k
        | _, False, _ -> thenBranch conditionState k
        | _, _, (Terms.ErrorT _ as e) -> k (errorHandler e, conditionState)
        | _, _, UnionT gvs -> Merging.commonGuardedErroredMapk execution errorHandler gvs conditionState merge k
        | _ -> execution conditionState condition k)

// ------------------------------- Substitution -------------------------------

    let rec substitute subst term =
        match term.term with
        | HeapRef(path, t, v) ->
            path |> NonEmptyList.toList |> substitutePath subst (fun path' ->
            let path'' = NonEmptyList.ofList path'
            if path'' = path then term else HeapView term.metadata path'' t v)
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
            if args = args' then term else Expression term.metadata op args' t)
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
