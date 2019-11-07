namespace VSharp.Core

#nowarn "69"

open VSharp
open VSharp.Core.Types

type SolverResult = Sat | Unsat | Unknown
type ISolver =
    abstract Solve : term -> SolverResult
    abstract SolvePathCondition : term -> term list -> SolverResult

module internal Common =
    let mutable private solver : ISolver option = None
    let configureSolver s = solver <- Some s
    let private solve term =
        match solver with
        | Some s -> s.Solve term
        | None -> Unknown
    let private solvePC term pc =
        match solver with
        | Some s -> s.SolvePathCondition term pc
        | None -> Unknown

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

    type subtypeElement =
        | Term of term
        | Type of termType
        override x.ToString() =
            match x with
            | Term term ->
                assert(isRef term)
                toString term
            | Type typ -> toString typ

    [<StructuralEquality;NoComparison>]
    type private symbolicSubtypeSource =
        {left : subtypeElement; right : subtypeElement}
        interface IStatedSymbolicConstantSource with
            override x.SubTerms = Seq.empty

    let private makeSubtypeBoolConst mtd left right =
        let subtypeName = sprintf "(%O <: %O)" left right
        let source = {left = left; right = right}
        Constant mtd subtypeName source Bool

    let rec typeIsType mtd leftType rightType = // left is subtype of right
        let boolConst left right = makeSubtypeBoolConst mtd (Type left) (Type right)

        match leftType, rightType with
        | _ when leftType = rightType -> makeTrue mtd
        | Null, _
        | Void, _   | _, Void
        | Bottom, _ | _, Bottom -> makeFalse mtd
        | Pointer _, Pointer _ -> makeTrue mtd
        | ArrayType _ , ArrayType(_, SymbolicDimension) -> makeTrue mtd
        | ArrayType(t1, ConcreteDimension d1), ArrayType(t2, ConcreteDimension d2) ->
            if d1 = d2 then typeIsType mtd t1 t2 else makeFalse mtd
        | ComplexType, ComplexType ->
            let lt = toDotNetType leftType
            let rt = toDotNetType rightType
            if rt.IsAssignableFrom lt then makeTrue mtd
            elif TypeUtils.isGround lt && TypeUtils.isGround rt then makeFalse mtd
            else boolConst leftType rightType
        | _ -> makeFalse mtd

    let refIsType mtd ref typ =
        let typeCheck ref =
            let boolConst ref = if isSymbolicRef ref then makeSubtypeBoolConst mtd (Term ref) (Type typ) else False
            let refType = baseTypeOfRef ref
            typeIsType mtd refType typ ||| boolConst ref
        Merging.guardedErroredApply typeCheck ref

    let typeIsRef mtd typ ref =
        let typeCheck ref =
            let boolConst ref = if isSymbolicRef ref then makeSubtypeBoolConst mtd (Type typ) (Term ref) else True
            let refType = baseTypeOfRef ref
            match typ with
            | InterfaceType _ -> makeFalse mtd
            | _ -> typeIsType mtd typ refType &&& boolConst ref
        Merging.guardedErroredApply typeCheck ref

    let refIsRef mtd leftRef rightRef =
        let typeCheck left right =
            let leftType = baseTypeOfRef left
            let rightType = baseTypeOfRef right
            match left, right with
            | SymbolicRef, SymbolicRef -> makeSubtypeBoolConst mtd (Term left) (Term right)
            | SymbolicRef, ConcreteRef -> refIsType mtd left rightType
            | ConcreteRef, SymbolicRef -> typeIsRef mtd leftType right
            | ConcreteRef, ConcreteRef -> typeIsType mtd leftType rightType
            | _ -> __unreachable__()
        let guardedApplyToRightRef left = Merging.guardedErroredApply (typeCheck left) rightRef
        Merging.guardedErroredApply guardedApplyToRightRef leftRef

    let typesEqual mtd x y = typeIsType mtd x y &&& typeIsType mtd y x

    [<StructuralEquality;NoComparison>]
    type private isValueTypeConstantSource =
        {termType : termType}
        interface IStatedSymbolicConstantSource with
            override x.SubTerms = Seq.empty

    let isValueType metadata termType = // TODO: add this to typeIsType
        let makeIsValueTypeBoolConst termType =
            Constant metadata (sprintf "IsValueType(%O)" termType) ({termType = termType}) Bool
        match termType with
        | TypeVariable(Id t) when TypeUtils.isValueTypeParameter t -> makeTrue metadata
        | TypeVariable(Id t) when TypeUtils.isReferenceTypeParameter t -> makeFalse metadata
        | TypeVariable _ -> makeIsValueTypeBoolConst termType
        | Null -> __unreachable__()
        | t -> makeBool metadata (toDotNetType t).IsValueType

    type symbolicSubtypeSource with
        interface IStatedSymbolicConstantSource with
            override x.Compose ctx state =
                let fillTerm = State.fillHoles ctx state
                let fillType = State.substituteTypeVariables ctx state
                match x.left, x.right with
                | Term l, Term r -> refIsRef ctx.mtd (fillTerm l) (fillTerm r)
                | Term l, Type r -> refIsType ctx.mtd (fillTerm l) (fillType r)
                | Type l, Term r -> typeIsRef ctx.mtd (fillType l) (fillTerm r)
                | Type l, Type r -> typeIsType ctx.mtd (fillType l) (fillType r)

    type isValueTypeConstantSource with
         interface IStatedSymbolicConstantSource with
            override x.Compose ctx state =
                let typ = State.substituteTypeVariables ctx state x.termType
                isValueType ctx.mtd typ

    [<StructuralEquality;NoComparison>]
    type private isNullableConstantSource =
        {termType : termType}
        interface IStatedSymbolicConstantSource with
            override x.SubTerms = Seq.empty

    let isNullable metadata termType =
        let makeIsNullableBoolConst termType =
            Constant metadata (sprintf "IsNullable(%O)" termType) ({termType = termType}) Bool
        match termType with
        | TypeVariable(Id t) when TypeUtils.isReferenceTypeParameter t -> makeFalse metadata
        | TypeVariable _ -> makeIsNullableBoolConst termType
        | Null -> __unreachable__()
        | _ -> makeBool metadata (System.Nullable.GetUnderlyingType(toDotNetType termType) <> null)

    type isNullableConstantSource with
         interface IStatedSymbolicConstantSource with
            override x.Compose ctx state =
                let typ = State.substituteTypeVariables ctx state x.termType
                isNullable ctx.mtd typ

// ---------------------------------------- Branching ---------------------------------------

    let commonStatelessConditionalExecutionk pc conditionInvocation thenBranch elseBranch merge merge2 errorHandler k =
        let execution condition k =
            thenBranch (fun thenResult ->
            elseBranch (fun elseResult ->
            k <| merge2 condition !!condition thenResult elseResult))
        let chooseBranch condition k =
            match condition with
            | Terms.True ->  thenBranch k
            | Terms.False -> elseBranch k
            | condition ->
                match solvePC condition pc with
                | Unsat -> elseBranch k
                | _ ->
                    match solvePC (!!condition) pc with
                    | Unsat -> thenBranch k
                    | _ -> execution condition k
        conditionInvocation (fun condition ->
        Merging.commonGuardedErroredApplyk chooseBranch errorHandler condition merge k)

    let statelessConditionalExecutionWithMergek pc conditionInvocation thenBranch elseBranch k = commonStatelessConditionalExecutionk pc conditionInvocation thenBranch elseBranch Merging.merge Merging.merge2Terms id k
    let statelessConditionalExecutionWithMerge pc conditionInvocation thenBranch elseBranch = statelessConditionalExecutionWithMergek pc conditionInvocation thenBranch elseBranch id

    let commonStatedConditionalExecutionk (state : state) conditionInvocation thenBranch elseBranch mergeResults mergeStates merge2Results merge2States errorHandler k =
        let execution conditionState condition k =
            assert (condition <> True && condition <> False)
            thenBranch (State.withPathCondition conditionState condition) (fun (thenResult, thenState) ->
            elseBranch (State.withPathCondition conditionState !!condition) (fun (elseResult, elseState) ->
            let result = merge2Results condition !!condition thenResult elseResult
            let state = merge2States condition !!condition (State.popPathCondition thenState) (State.popPathCondition elseState)
            k (result, state)))
        let chooseBranch conditionState condition k =
            let thenCondition = Merging.conditionUnderState condition conditionState
            let elseCondition = Merging.conditionUnderState !!condition conditionState
            match thenCondition, elseCondition with
            | False, _ -> elseBranch conditionState k
            | _, False -> thenBranch conditionState k
            | _ ->
                match solvePC condition (State.pathConditionOf conditionState) with
                | Unsat -> elseBranch conditionState k
                | _ ->
                    match solvePC !!condition (State.pathConditionOf conditionState) with
                    | Unsat -> thenBranch conditionState k
                    | _ -> execution conditionState condition k
        conditionInvocation state (fun (condition, conditionState) ->
        Merging.commonGuardedErroredStatedApplyk chooseBranch errorHandler conditionState condition mergeResults mergeStates k)

    let statedConditionalExecutionWithMergek state conditionInvocation thenBranch elseBranch k =
        commonStatedConditionalExecutionk state conditionInvocation thenBranch elseBranch Merging.merge Merging.mergeStates Merging.merge2Terms Merging.merge2States id k
    let statedConditionalExecutionWithMerge state conditionInvocation thenBranch elseBranch = statedConditionalExecutionWithMergek state conditionInvocation thenBranch elseBranch id
