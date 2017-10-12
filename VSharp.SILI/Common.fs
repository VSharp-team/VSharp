namespace VSharp

open VSharp.Terms
open VSharp.Types.Constructor
open Hierarchy

module internal Common =

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

    type private SymbolicTypeSource(t : TermType) =
        inherit SymbolicConstantSource()

    let rec is metadata leftType rightType =
        let makeBoolConst name termType = Constant name (SymbolicTypeSource termType) Bool Metadata.empty
        in
        let concreteIs (dotNetTypeHierarchy : Hierarchy) rightTermType =
            let b = makeBoolConst (dotNetTypeHierarchy.Name) rightTermType in
            function
            | ReferenceType(t, _, _)
            | StructureType(t, _ ,_) -> Terms.MakeBool (t.Equals dotNetTypeHierarchy) metadata
            | SubType(t, _, _, name) as termType when dotNetTypeHierarchy.Is t ->
                implies (makeBoolConst name termType) b metadata
            | ArrayType (t, SymbolicDimension name) as termType ->
                implies (makeBoolConst name termType) b metadata
            | SubType(t, _, _, _) when not <| dotNetTypeHierarchy.Is t -> Terms.MakeFalse metadata
            // TODO: squash all Terms.MakeFalse into default case and get rid of __notImplemented__()
            | PointerType _ -> Terms.MakeFalse metadata
            | _ -> __notImplemented__()
        in
        let subTypeIs (dotNetTypeHierarchy : Hierarchy) rightTermType rightName =
            let b = makeBoolConst rightName rightTermType in
            function
            | ReferenceType(t, _, _) -> Terms.MakeBool (t.Is dotNetTypeHierarchy) metadata
            | StructureType _ -> Terms.MakeBool (Hierarchy(typedefof<System.ValueType>).Is dotNetTypeHierarchy) metadata
            | SubType(t, _, _, _) when t.Is dotNetTypeHierarchy -> Terms.MakeTrue metadata
            | SubType(t, _, _, name) as termType when dotNetTypeHierarchy.Is t ->
                implies (makeBoolConst name termType) b metadata
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
        | leftType, (StructureType(t, _, _) as termType)
        | leftType, (ReferenceType(t, _, _) as termType) -> concreteIs t termType leftType
        | leftType, (SubType(t, _, _, name) as termType) -> subTypeIs t termType name leftType
        | _ -> Terms.MakeFalse metadata

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
            |> Merging.unguardTerm |> Merging.merge
        in
        let elseCondition =
            Propositional.conjunction condition.metadata (!!condition :: State.pathConditionOf conditionState)
            |> Merging.unguardTerm |> Merging.merge
        in
        match thenCondition, elseCondition, condition with
        | False, _, _ -> elseBranch conditionState k
        | _, False, _ -> thenBranch conditionState k
        | _, _, (Terms.ErrorT _ as e) -> k (errorHandler e, conditionState)
        | _, _, UnionT gvs -> Merging.commonGuardedErroredMapk execution errorHandler gvs conditionState merge k
        | _ -> execution conditionState condition k)
