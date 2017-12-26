namespace VSharp

open VSharp.Terms
open VSharp.Types.Constructor
open Hierarchy

module internal Common =
    open System

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

    type private SymbolicSubtypeSource(left : TermType, right : TermType) =
        inherit SymbolicConstantSource()

    let rec is metadata leftType rightType =
        let subtypeName lname rname = String.concat "" ["("; lname; " <: "; rname; ")"]
        let makeBoolConst lname rname leftTermType rightTermType =
            Constant (subtypeName lname rname) (SymbolicSubtypeSource(leftTermType, rightTermType)) Bool metadata
        match leftType, rightType with
        | TermType.Null, _
        | Void, _   | _, Void
        | Bottom, _ | _, Bottom -> Terms.MakeFalse metadata
        | Reference _, Reference _ -> Terms.MakeTrue metadata
        | Pointer _, Pointer _ -> Terms.MakeTrue metadata
        | Func _, Func _ -> Terms.MakeTrue metadata
        | ArrayType(t1, c1), ArrayType(_, SymbolicDimension _) -> Terms.MakeTrue metadata
        | ArrayType(_, SymbolicDimension _) as t1, (ArrayType _ as t2) ->
            if t1 <> t2 then (makeBoolConst (t1.ToString()) (t2.ToString()) t1 t2) else MakeTrue metadata
        | ArrayType(t1, c1), ArrayType(t2, c2) ->
            if c1 = c2 then is metadata t1 t2 else Terms.MakeFalse metadata
        | GeneralType(GeneralName (lname, t)) as t1, t2 ->
            (is metadata t t2 ||| is metadata t2 t) &&& makeBoolConst lname (t2.ToString()) t1 t2
        | t1, (GeneralType(GeneralName (rname, t)) as t2) ->
            is metadata t1 t &&& makeBoolConst (t1.ToString()) rname t1 t2 
        | ConcreteType lt as t1, (ConcreteType rt as t2) ->
            if lt.IsGround && rt.IsGround
                then Terms.MakeBool (lt.Is rt) metadata
                else if lt.Is rt then Terms.MakeTrue metadata else makeBoolConst (t1.ToString()) (t2.ToString()) t1 t2
        | _ -> Terms.MakeFalse metadata

    type private IsValueType(termType : TermType) =
        inherit SymbolicConstantSource()

    let internal isValueType metadata termType =
        let makeBoolConst name = Constant (sprintf "IsValueType(%s)" name) (IsValueType termType) Bool metadata
        match termType with
        | ConcreteType t when t.Inheritor.IsValueType -> MakeTrue metadata
        | GeneralType(GeneralName(name, t)) ->
            if (Types.ToDotNetType t).IsValueType
                then makeBoolConst name
                else MakeFalse metadata
        | _ -> MakeFalse metadata

    let internal fromDotNetGeneralTypeWithConstraint metadata dotnetType =
        let name = "GeneralArrayType"
        let CreatedDim = function
            | SymbolicDimension _ -> SymbolicDimension (IdGenerator.startingWith name)
            | d -> d
        let termType = FromDotNetType dotnetType
        let rec getNewType termType =
            match termType with
            | ArrayType(elemType, dim) ->
                let newElemType = getNewType elemType
                ArrayType(newElemType, CreatedDim dim)
            | ConcreteType t when t.Inheritor.IsSealed && not t.Inheritor.IsGenericParameter -> termType
            | _ -> CreateGeneralType termType ()
        getNewType termType

    let internal fromMetadataGeneralTypeWithConstraint metadata metadataType =
        let dotnetType = MetadataToDotNetType metadataType
        fromDotNetGeneralTypeWithConstraint metadata dotnetType

    let internal simpleConditionalExecution conditionInvocation thenBranch elseBranch merge merge2 k =
        let execution condition k =
            thenBranch (fun thenResult ->
            elseBranch (fun elseResult ->
            k <| merge2 condition !!condition thenResult elseResult))
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
            let result = merge2 condition !!condition thenResult elseResult
            let state = Merging.merge2States condition !!condition (State.popPathCondition thenState) (State.popPathCondition elseState)
            k (result, state)))
        conditionInvocation state (fun (condition, conditionState) ->
        let thenCondition =
            Propositional.conjunction condition.metadata (condition :: State.pathConditionOf conditionState)
            |> Merging.unguardTerm |> Merging.merge
        let elseCondition =
            Propositional.conjunction condition.metadata (!!condition :: State.pathConditionOf conditionState)
            |> Merging.unguardTerm |> Merging.merge
        match thenCondition, elseCondition, condition with
        | False, _, _ -> elseBranch conditionState k
        | _, False, _ -> thenBranch conditionState k
        | _, _, (Terms.ErrorT _ as e) -> k (errorHandler e, conditionState)
        | _, _, UnionT gvs -> Merging.commonGuardedErroredMapk execution errorHandler gvs conditionState merge k
        | _ -> execution conditionState condition k)
