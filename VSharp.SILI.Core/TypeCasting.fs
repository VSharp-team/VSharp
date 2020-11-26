namespace VSharp.Core

#nowarn "69"

open VSharp
open VSharp.Core.Types

module internal TypeCasting =
    type private subtypeElement =
        | SymbolicType of heapAddress
        | ConcreteType of symbolicType
        override x.ToString() =
            match x with
            | SymbolicType address -> toString address
            | ConcreteType typ -> toString typ
        member internal x.SubTerm =
            match x with
            | SymbolicType t -> Some t
            | _ -> None

    [<StructuralEquality;NoComparison>]
    type private symbolicSubtypeSource =
        {left : subtypeElement; right : subtypeElement}
        interface IStatedSymbolicConstantSource with
            override x.SubTerms = optCons (optCons [] x.left.SubTerm) x.right.SubTerm :> term seq
            override x.Time = VectorTime.zero

    let private makeSubtypeBoolConst left right =
        let subtypeName = sprintf "(%O <: %O)" left right
        let source = {left = left; right = right}
        Constant subtypeName source Bool

    let rec typeIsType leftType rightType = // left is subtype of right
        let boolConst left right = makeSubtypeBoolConst (ConcreteType left) (ConcreteType right)

        match leftType, rightType with
        | _ when leftType = rightType -> True
        | Null, _
        | Void, _   | _, Void -> False
        | ArrayType _, ClassType(Id obj, _) when obj <> typedefof<obj> -> False // TODO: use more common heuristics
        | Numeric _, Numeric _ -> True
        | Pointer _, Pointer _ -> True
        | ArrayType _, ArrayType(_, SymbolicDimension) -> True
        | ArrayType(t1, ConcreteDimension d1), ArrayType(t2, ConcreteDimension d2) ->
            if d1 = d2 then typeIsType t1 t2 else False
        | ComplexType, ComplexType ->
            let lt = toDotNetType leftType
            let rt = toDotNetType rightType
            if rt.IsAssignableFrom lt then True
            elif TypeUtils.isGround lt && TypeUtils.isGround rt then False
            else boolConst leftType rightType
        | _ -> False

    let addressIsType leftAddress leftType targetTyp =
        let typeCheck address =
            let boolConst address =
                match address.term with
                | ConcreteHeapAddress _ -> False
                | _ -> makeSubtypeBoolConst (SymbolicType address) (ConcreteType targetTyp)
            typeIsType leftType targetTyp ||| boolConst address
        Merging.guardedApply typeCheck leftAddress

    let typeIsAddress leftType rightAddress rightType =
        let typeCheck rightAddress =
            let boolConst address =
                match address.term with
                | ConcreteHeapAddress _ -> True
                | _ -> makeSubtypeBoolConst (ConcreteType leftType) (SymbolicType address)
            match leftType with
            | InterfaceType _ -> False
            | _ -> typeIsType leftType rightType &&& boolConst rightAddress
        Merging.guardedApply typeCheck rightAddress

    let addressIsAddress leftAddress leftType rightAddress rightType =
        let typeCheck leftAddress rightAddress =
            match leftAddress.term, rightAddress.term with
            | ConcreteHeapAddress _, ConcreteHeapAddress _ -> typeIsType leftType rightType
            | ConcreteHeapAddress _, _ -> typeIsAddress leftType rightAddress rightType
            | _, ConcreteHeapAddress _ -> addressIsType leftAddress leftType rightType
            | _ -> makeSubtypeBoolConst (SymbolicType leftAddress) (SymbolicType rightAddress)
        Merging.guardedApply (fun left -> Merging.guardedApply (typeCheck left) rightAddress) leftAddress

    let typesEqual x y = typeIsType x y &&& typeIsType y x

    let rec typeIsRef typ ref =
        match ref.term with
        | HeapRef(addr, rightType) -> typeIsAddress typ addr rightType
        | Union gvs -> gvs |> List.map (fun (g, v) -> (g, typeIsRef typ v)) |> Merging.merge
        | _ -> internalfailf "Checking subtyping: expected heap reference, but got %O" ref

    let rec refIsType ref typ =
        match ref.term with
        | HeapRef(addr, leftType) -> addressIsType addr leftType typ
        | Union gvs -> gvs |> List.map (fun (g, v) -> (g, refIsType v typ)) |> Merging.merge
        | _ -> internalfailf "Checking subtyping: expected heap reference, but got %O" ref

    let rec refIsRef leftRef rightRef =
        match leftRef.term, rightRef.term with
        | HeapRef(leftAddr, leftType), HeapRef(rightAddr, rightType) -> addressIsAddress leftAddr leftType rightAddr rightType
        | Union gvs, _ -> gvs |> List.map (fun (g, v) -> (g, refIsRef v rightRef)) |> Merging.merge
        | _, Union gvs -> gvs |> List.map (fun (g, v) -> (g, refIsRef leftRef v)) |> Merging.merge
        | _ -> internalfailf "Checking subtyping: expected heap reference, but got %O" ref

    type symbolicSubtypeSource with
        interface IStatedSymbolicConstantSource with
            override x.Compose state =
                let fillTerm = Memory.fillHoles state
                let fillType = Memory.substituteTypeVariables state
                match x.left, x.right with
                | SymbolicType l, SymbolicType r ->
                    let l = fillTerm l
                    let r = fillTerm r
                    addressIsAddress l (Memory.typeOfHeapLocation state l) r (Memory.typeOfHeapLocation state r)
                | SymbolicType l, ConcreteType r ->
                    let l = fillTerm l
                    addressIsType l (Memory.typeOfHeapLocation state l) (fillType r)
                | ConcreteType l, SymbolicType r ->
                    let r = fillTerm r
                    typeIsAddress (fillType l) r (Memory.typeOfHeapLocation state r)
                | ConcreteType l, ConcreteType r -> typeIsType (fillType l) (fillType r)

    let isNullable termType =
        match termType with
        | TypeVariable(Id t) when TypeUtils.isReferenceTypeParameter t -> false
        | TypeVariable _ -> __insufficientInformation__ "Can't determine if %O is a nullable type or not!" termType
        | Null -> __unreachable__()
        | _ -> System.Nullable.GetUnderlyingType(toDotNetType termType) <> null

    let private doCast term targetType =
        match term.term with
        | Ptr(_, _, _) ->
            match targetType with
            | Pointer typ' -> castReferenceToPointer typ' term
            | _ -> internalfailf "Can't cast pointer %O to type %O" term targetType
        | HeapRef(addr, typ) -> if Types.isConcreteSubtype typ targetType then term else HeapRef addr targetType
        | Ref _ -> __notImplemented__() // TODO: can this happen? Ref points to primitive type!
        | Struct _ -> term
        | _ -> __unreachable__()

    let canCast term targetType =
        let castCheck term =
            match term.term with
            | Concrete(value, _) -> canCastConcrete value targetType |> makeBool
            | Ptr(_, typ, _) -> typeIsType (Pointer typ) targetType
            | Ref _ -> typeIsType (typeOfRef term) targetType
            | HeapRef(address, baseType) -> addressIsType address baseType targetType
            | _ -> typeIsType (typeOf term) targetType
        Merging.guardedApply castCheck term


    let cast term targetType =
        let castUnguarded term =
            match typeOf term with
            | t when t = targetType -> term
            | Bool
            | Numeric _ -> primitiveCast term targetType
            | Pointer _
            | StructType _
            | ClassType _
            | InterfaceType _
            | TypeVariable _
            | ArrayType _ -> doCast term targetType
            | Null -> nullRef
            | _ -> __unreachable__()
        Merging.guardedApply castUnguarded term

    let castReferenceToPointer state reference =
        let getType ref =
            match ref.term with
            | Ref(PrimitiveStackLocation key) -> Memory.typeOfStackLocation state key
            | _ -> typeOfRef ref
        let doCast reference =
            let typ = commonTypeOf getType reference
            Terms.castReferenceToPointer typ reference
        Merging.guardedApply doCast reference
