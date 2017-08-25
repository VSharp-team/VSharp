namespace VSharp

open VSharp.Terms

module internal Common =

    let internal simplifyPairwiseCombinations = Propositional.simplifyPairwiseCombinations

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
        let concreteIs (dotNetType : System.Type) =
            let b = makeBoolConst dotNetType.FullName (ClassType(dotNetType, [], [])) in
            function
            | Types.ReferenceType(t, _, _)
            | Types.StructureType(t, _ ,_) -> Terms.MakeBool (t = dotNetType) metadata
            | SubType(t, _, _, name) as termType when t.IsAssignableFrom(dotNetType) ->
                implies (makeBoolConst name termType) b metadata
            | SubType(t, _, _, _) when not <| t.IsAssignableFrom(dotNetType) -> Terms.MakeFalse metadata
            | ArrayType _ -> Terms.MakeBool (dotNetType.IsAssignableFrom(typedefof<obj>)) metadata
            // TODO: squash all Terms.MakeFalse into default case and get rid of __notImplemented__()
            | PointerType _ -> Terms.MakeFalse metadata
            | _ -> __notImplemented__()
        in
        let subTypeIs (dotNetType: System.Type, rightName) =
            let b = makeBoolConst rightName (SubType(dotNetType, [], [], rightName)) in
            function
            | Types.ReferenceType(t, _, _) -> Terms.MakeBool (dotNetType.IsAssignableFrom(t)) metadata
            | Types.StructureType(t, _, _) -> Terms.MakeBool (dotNetType = typedefof<obj> || dotNetType = typedefof<System.ValueType>) metadata
            | SubType(t, _, _, _) when dotNetType.IsAssignableFrom(t) -> Terms.MakeTrue metadata
            | SubType(t, _, _, name) as termType when t.IsAssignableFrom(dotNetType) ->
                implies (makeBoolConst name termType) b metadata
            | ArrayType _ -> Terms.MakeBool (dotNetType.IsAssignableFrom(typedefof<obj>)) metadata
            | _ -> __notImplemented__()
        in
        match leftType, rightType with
        | TermType.Null, _
        | Void, _   | _, Void
        | Bottom, _ | _, Bottom -> Terms.MakeFalse metadata
        | PointerType left, PointerType right -> Terms.MakeTrue metadata
        | Func _, Func _ -> Terms.MakeTrue metadata
        | ArrayType(t1, c1), ArrayType(_, 0) -> Terms.MakeTrue metadata
        | ArrayType(t1, c1), ArrayType(t2, c2) -> if c1 = c2 then is metadata t1 t2 else Terms.MakeFalse metadata
        | leftType, Types.StructureType(t, _, _)
        | leftType, Types.ReferenceType(t, _, _) -> concreteIs t leftType
        | leftType, SubType(t, _, _, name) -> subTypeIs (t, name) leftType
        | _ -> Terms.MakeFalse metadata
