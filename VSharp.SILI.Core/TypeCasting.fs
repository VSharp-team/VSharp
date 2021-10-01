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
            override x.IndependentWith otherSource =
                match otherSource with
                | :? symbolicSubtypeSource as otherSubtype -> x <> otherSubtype
                | _ -> true

    let private makeSubtypeBoolConst left right =
        let subtypeName = sprintf "(%O <: %O)" left right
        let source = {left = left; right = right}
        Constant subtypeName source Bool

    let rec commonTypeIsType nullCase leftType rightType =
        let boolConst left right = makeSubtypeBoolConst (ConcreteType left) (ConcreteType right)
        isConcreteSubtype nullCase leftType rightType makeBool boolConst

    // left is subtype of right
    let typeIsType = commonTypeIsType (always false)

    let commonAddressIsType nullCase leftAddress leftType targetTyp =
        let typeCheck address =
            let boolConst address =
                match address.term with
                | ConcreteHeapAddress _ -> False
                | _ -> makeSubtypeBoolConst (SymbolicType address) (ConcreteType targetTyp)
            commonTypeIsType nullCase leftType targetTyp ||| boolConst address
        Merging.guardedApply typeCheck leftAddress

    let addressIsType = commonAddressIsType (always false)

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

    let rec typeIsRef state typ ref =
        match ref.term with
        | HeapRef(addr, sightType) ->
            let rightType = Memory.mostConcreteTypeOfHeapRef state addr sightType
            typeIsAddress typ addr rightType
        | Union gvs -> gvs |> List.map (fun (g, v) -> (g, typeIsRef state typ v)) |> Merging.merge
        | _ -> internalfailf "Checking subtyping: expected heap reference, but got %O" ref

    let rec commonRefIsType nullCase state ref typ =
        match ref.term with
        | HeapRef(addr, sightType) ->
            let leftType = Memory.mostConcreteTypeOfHeapRef state addr sightType
            commonAddressIsType nullCase addr leftType typ
        | Union gvs -> gvs |> List.map (fun (g, v) -> (g, commonRefIsType nullCase state v typ)) |> Merging.merge
        | _ -> internalfailf "Checking subtyping: expected heap reference, but got %O" ref

    let refIsType = commonRefIsType (always false)
    let refIsAssignableToType = commonRefIsType (not << isValueType)

    let rec refIsRef state leftRef rightRef =
        match leftRef.term, rightRef.term with
        | HeapRef(leftAddr, leftSightType), HeapRef(rightAddr, rightSightType) ->
            let leftType = Memory.mostConcreteTypeOfHeapRef state leftAddr leftSightType
            let rightType = Memory.mostConcreteTypeOfHeapRef state rightAddr rightSightType
            addressIsAddress leftAddr leftType rightAddr rightType
        | Union gvs, _ -> gvs |> List.map (fun (g, v) -> (g, refIsRef state v rightRef)) |> Merging.merge
        | _, Union gvs -> gvs |> List.map (fun (g, v) -> (g, refIsRef state leftRef v)) |> Merging.merge
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
        | Null -> false
        | _ -> System.Nullable.GetUnderlyingType(toDotNetType termType) <> null

    let private doCast term targetType =
        match term.term with
        | Ptr _ ->
            match targetType with
            | Pointer typ' -> castReferenceToPointer typ' term
            // Converting ptr to number (conv.u8 instruction, for example) results in the same ptr, because number conversion is pointless
            | Numeric _ -> term
            | _ -> internalfailf "Can't cast pointer %O to type %O" term targetType
        | HeapRef(addr, _) -> HeapRef addr targetType
        | Ref _ when isByRef targetType -> term
        | Ref _ -> __notImplemented__() // TODO: can this happen? Ref points to primitive type!
        | Struct _ -> term
        | _ -> __unreachable__()

    let canCast state term targetType =
        let castCheck term =
            match term.term with
            | Concrete(value, _) -> canCastConcrete value targetType |> makeBool
            | Ptr(_, typ, _) -> typeIsType (Pointer typ) targetType
            | Ref address -> typeIsType (Memory.baseTypeOfAddress state address) targetType
            | HeapRef(address, sightType) ->
                let baseType = Memory.mostConcreteTypeOfHeapRef state address sightType
                addressIsType address baseType targetType
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

    let rec private nearestBiggerTypeForEvaluationStack (t : System.Type) =
        match t with
        | _ when t = typeof<int8>    -> Int32
        | _ when t = typeof<int16>   -> Int32
        | _ when t = typeof<int32>   -> Int32
        | _ when t = typeof<int64>   -> Int64
        | _ when t = typeof<byte>    -> Int32
        | _ when t = typeof<char>    -> Int32
        | _ when t = typeof<uint16>  -> Int32
        | _ when t = typeof<uint32>  -> Int32
        | _ when t = typeof<uint64>  -> Int64
        | _ when t = typeof<float32> -> F
        | _ when t = typeof<float>   -> F
        | _ when t.IsEnum -> t.GetEnumUnderlyingType() |> nearestBiggerTypeForEvaluationStack
        | _ -> __notImplemented__()

    let castToEvaluationStackType x =
        match typeOf x with
        // This case is needed for references to primitive types
        | _ when isReference x -> x
        // TODO: need to add conversion from bool to int?
        // | Bool -> cast x Int32
        | Numeric(Id typ) -> nearestBiggerTypeForEvaluationStack typ |> cast x
        | _ -> x
