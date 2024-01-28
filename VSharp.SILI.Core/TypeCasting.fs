namespace VSharp.Core

#nowarn "69"

open System
open VSharp
open VSharp.Core
open VSharp.TypeUtils

module internal TypeCasting =
    type private subtypeElement =
        | SymbolicType of heapAddress
        | ConcreteType of Type
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
            override x.TypeOfLocation = typeof<bool>

    let (|TypeSubtypeTypeSource|_|) (src : ISymbolicConstantSource) =
        match src with
        | :? symbolicSubtypeSource as s ->
            match s.left, s.right with
            | ConcreteType u, ConcreteType v -> Some(u, v)
            | _ -> None
        | _ -> None

    let (|RefSubtypeTypeSource|_|) (src : ISymbolicConstantSource) =
        match src with
        | :? symbolicSubtypeSource as s ->
            match s.left, s.right with
            | SymbolicType u, ConcreteType v -> Some(u, v)
            | _ -> None
        | _ -> None

    let (|TypeSubtypeRefSource|_|) (src : ISymbolicConstantSource) =
        match src with
        | :? symbolicSubtypeSource as s ->
            match s.left, s.right with
            | ConcreteType u, SymbolicType v -> Some(u, v)
            | _ -> None
        | _ -> None

    let (|RefSubtypeRefSource|_|) (src : ISymbolicConstantSource) =
        match src with
        | :? symbolicSubtypeSource as s ->
            match s.left, s.right with
            | SymbolicType u, SymbolicType v -> Some(u, v)
            | _ -> None
        | _ -> None

    [<StructuralEquality;NoComparison>]
    type private symbolicTypeEqualSource =
        { address : term; targetType : Type }
        interface IStatedSymbolicConstantSource with
            override x.SubTerms = List.singleton x.address
            override x.Time = VectorTime.zero
            override x.TypeOfLocation = typeof<bool>

    let (|RefEqTypeSource|_|) (src : ISymbolicConstantSource) =
        match src with
        | :? symbolicTypeEqualSource as s -> Some(s.address, s.targetType)
        | _ -> None

    let private makeSubtypeBoolConst left right =
        let subtypeName = $"({left} <: {right})"
        let source = {left = left; right = right}
        Constant subtypeName source typeof<bool>

    let private makeTypeEqBoolConst ref typ =
        let name = $"(typeof<{ref}> = {typ})"
        let source = {address = ref; targetType = typ}
        Constant name source typeof<bool>

    // left is subtype of right
    let rec typeIsType leftType rightType =
        let boolConst left right = makeSubtypeBoolConst (ConcreteType left) (ConcreteType right)
        isConcreteSubtype leftType rightType makeBool boolConst

    let addressIsType leftAddress leftType targetType =
        let typeCheck address =
            let boolConst address =
                match address.term with
                | ConcreteHeapAddress _ -> False()
                | _ -> makeSubtypeBoolConst (SymbolicType address) (ConcreteType targetType)
            typeIsType leftType targetType ||| boolConst address
        Merging.guardedApply typeCheck leftAddress

    let addressEqType address addressType targetType =
        let typeCheck address =
            let boolConst address =
                match address.term with
                | ConcreteHeapAddress _ -> False()
                | _ -> makeTypeEqBoolConst address targetType
            if addressType = targetType then True()
            else boolConst address
        Merging.guardedApply typeCheck address

    let typeIsAddress leftType rightAddress rightType =
        let typeCheck rightAddress =
            let boolConst address =
                match address.term with
                | ConcreteHeapAddress _ -> True()
                | _ -> makeSubtypeBoolConst (ConcreteType leftType) (SymbolicType address)
            match leftType with
            | InterfaceType _ -> False()
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
            let rightType = state.memory.MostConcreteTypeOfHeapRef addr sightType
            typeIsAddress typ addr rightType
        | Ref address ->
            let rightType = address.TypeOfLocation
            typeIsType typ rightType
        | Union gvs -> Merging.guardedMap (typeIsRef state typ) gvs
        | _ -> internalfailf $"Checking subtyping: expected heap reference, but got {ref}"

    let rec refIsType state ref typ =
        match ref.term with
        | HeapRef(addr, sightType) ->
            let leftType = state.memory.MostConcreteTypeOfHeapRef addr sightType
            addressIsType addr leftType typ
        | Ref address ->
            let leftType = address.TypeOfLocation
            typeIsType leftType typ
        | Union gvs ->
            let refIsType term = refIsType state term typ
            Merging.guardedMap refIsType gvs
        | _ -> internalfailf $"Checking subtyping: expected heap reference, but got {ref}"

    let rec refEqType state ref typ =
        match ref.term with
        | HeapRef(addr, sightType) ->
            let leftType = state.memory.MostConcreteTypeOfHeapRef addr sightType
            addressEqType addr leftType typ
        | Ref address ->
            let leftType = address.TypeOfLocation
            makeBool (leftType = typ)
        | Union gvs ->
            let refEqType term = refEqType state term typ
            Merging.guardedMap refEqType gvs
        | _ -> internalfailf $"Checking subtyping: expected heap reference, but got {ref}"

    let rec refIsRef state leftRef rightRef =
        let memory = state.memory
        match leftRef.term, rightRef.term with
        | HeapRef(leftAddr, leftSightType), HeapRef(rightAddr, rightSightType) ->
            let leftType = memory.MostConcreteTypeOfHeapRef leftAddr leftSightType
            let rightType = memory.MostConcreteTypeOfHeapRef rightAddr rightSightType
            addressIsAddress leftAddr leftType rightAddr rightType
        | Ref leftAddress, HeapRef(rightAddr, rightSightType) ->
            let leftType = leftAddress.TypeOfLocation
            let rightType = memory.MostConcreteTypeOfHeapRef rightAddr rightSightType
            typeIsAddress leftType rightAddr rightType
        | HeapRef(leftAddr, leftSightType), Ref rightAddress ->
            let leftType = memory.MostConcreteTypeOfHeapRef leftAddr leftSightType
            let rightType = rightAddress.TypeOfLocation
            addressIsType leftAddr leftType rightType
        | Union gvs, _ ->
            let refIsRef term = refIsRef state term rightRef
            Merging.guardedMap refIsRef gvs
        | _, Union gvs -> Merging.guardedMap (refIsRef state leftRef) gvs
        | _ -> internalfailf $"Checking subtyping: expected heap reference, but got {ref}"

    type symbolicSubtypeSource with
        interface IStatedSymbolicConstantSource with
            override x.Compose state =
                let fillTerm = state.FillHoles
                let fillType = state.SubstituteTypeVariables
                let memory = state.memory
                match x.left, x.right with
                | SymbolicType l, SymbolicType r ->
                    let l = fillTerm l
                    let r = fillTerm r
                    addressIsAddress l (memory.TypeOfHeapLocation l) r (memory.TypeOfHeapLocation r)
                | SymbolicType l, ConcreteType r ->
                    let l = fillTerm l
                    let notMock() = addressIsType l (memory.TypeOfHeapLocation l) (fillType r)
                    match l with
                    | {term = ConcreteHeapAddress addr} ->
                        // None when addr is null
                        match PersistentDict.tryFind memory.AllocatedTypes addr with
                        | Some(MockType mockType) ->
                            let assignableExists = mockType.SuperTypes |> Seq.exists (fun st -> st.IsAssignableTo r)
                            if assignableExists then True() else False()
                        | _ -> notMock()
                    | _ -> notMock()
                | ConcreteType l, SymbolicType r ->
                    let r = fillTerm r
                    let notMock() = typeIsAddress (fillType l) r (memory.TypeOfHeapLocation r)
                    match r with
                    | {term = ConcreteHeapAddress addr} ->
                        // None when addr is null
                        match PersistentDict.tryFind memory.AllocatedTypes addr with
                        | Some(MockType _) -> False()
                        | _ -> notMock()
                    | _ -> notMock()
                | ConcreteType l, ConcreteType r -> typeIsType (fillType l) (fillType r)

    type symbolicTypeEqualSource with
        interface IStatedSymbolicConstantSource with
            override x.Compose state =
                let address = state.FillHoles x.address
                let memory = state.memory
                let notMock() =
                    let targetType = state.SubstituteTypeVariables x.targetType
                    let addressType = memory.TypeOfHeapLocation address
                    addressEqType address addressType targetType
                match address.term with
                | ConcreteHeapAddress addr ->
                    // None when addr is null
                    match PersistentDict.tryFind memory.AllocatedTypes addr with
                    | Some(MockType _) -> False()
                    | _ -> notMock()
                | _ -> notMock()

    let private doCast term targetType =
        match term.term, targetType with
        | Ptr(address, _, indent), Pointer typ -> Ptr address typ indent
        // Converting IntPtr to number
        | DetachedPtr offset, Numeric t -> primitiveCast offset t
        // Converting ptr to number (conv.u8 instruction, for example) results in the same ptr, because number conversion is pointless
        | Ptr _, Numeric _ -> term
        | Ptr(HeapLocation(address, _), _, ConcreteT(:? int as offset, _)), ByRef t when address = zeroAddress() && offset = 0 -> nullRef t
        // CASE: pointer from concolic
        | Ptr(address, Void, offset), ByRef typ' -> Ptr address typ' offset
        | Ptr _, ByRef _ ->
            Logger.trace $"Casting nonnull ptr to ByRef type {targetType}"
            term
        | Ptr(address, _, indent), typ ->
            Logger.warning $"Casting pointer {term} to non-pointer type {typ}"
            Ptr address typ indent
        | Ref _, ByRef _ -> term
        | Ref address, _ when address.TypeOfLocation = targetType -> term
        | Ref address, Pointer typ' when address.TypeOfLocation = typ' -> term
        | Ref address, Pointer typ' ->
            let baseAddress, offset = Pointers.addressToBaseAndOffset address
            Ptr baseAddress typ' offset
        | Ref address, _ ->
            let baseAddress, offset = Pointers.addressToBaseAndOffset address
            Ptr baseAddress targetType offset
        | HeapRef(addr, sightType), _ when isAssignable sightType targetType || isAssignable targetType sightType ->
            HeapRef addr targetType
        | HeapRef _, _ ->
            Logger.trace $"Unsafe cast in safe context: address {term}, type {targetType}"
            term
//            Ptr (HeapLocation addr) typ (makeNumber 0)
        | Struct(_ , t), _ when t = targetType -> term
        | Struct _, _ -> internalfailf $"Casting struct to {targetType}"
        | Combined(slices, t), _ when isPointer t && isPointer targetType ->
            combine slices targetType
        | _ -> internalfailf $"Can't cast {term} to type {targetType}"

    let canCast state term targetType =
        let castCheck term =
            let memory = state.memory
            match term.term with
            | Concrete(value, _) -> canCastConcrete value targetType |> makeBool
            | Ptr(_, typ, _) -> typeIsType (typ.MakePointerType()) targetType
            | Ref address -> typeIsType ((memory.BaseTypeOfAddress address).MakeByRefType()) targetType
            | HeapRef(address, sightType) ->
                let baseType = memory.MostConcreteTypeOfHeapRef address sightType
                addressIsType address baseType targetType
            | _ -> typeIsType (typeOf term) targetType
        Merging.guardedApply castCheck term

    let cast term targetType =
        let castUnguarded term =
            match typeOf term with
            | t when t = targetType -> term
            // Case for method pointer
            | t when t.IsAssignableTo typeof<System.Reflection.MethodInfo> && not (isRefOrPtr term) ->
                primitiveCast term targetType
            | Bool
            | Numeric _ when isRefOrPtr term |> not -> primitiveCast term targetType
            | Numeric _
            | Pointer _
            | ByRef _
            | StructType _
            | ClassType _
            | InterfaceType _
            | TypeVariable _
            | ArrayType _ -> doCast term targetType
            | _ -> __unreachable__()
        Merging.guardedApply castUnguarded term

    let rec private nearestBiggerTypeForEvaluationStack (t : Type) =
        match t with
        | _ when t = typeof<int8>    -> typeof<int32>
        | _ when t = typeof<int16>   -> typeof<int32>
        | _ when t = typeof<int32>   -> typeof<int32>
        | _ when t = typeof<int64>   -> typeof<int64>
        | _ when t = typeof<byte>    -> typeof<uint32> // TODO: need to use signed?
        | _ when t = typeof<char>    -> typeof<uint32> // TODO: need to use signed?
        | _ when t = typeof<uint16>  -> typeof<uint32> // TODO: need to use signed?
        | _ when t = typeof<uint32>  -> typeof<uint32>
        | _ when t = typeof<uint64>  -> typeof<uint64>
        | _ when t = typeof<float32> -> typeof<float32>
        | _ when t = typeof<float>   -> typeof<float>
        | _ when t = typeof<IntPtr>  -> typeof<IntPtr>
        | _ when t = typeof<UIntPtr> -> typeof<UIntPtr>
        | _ when t.IsEnum -> EnumUtils.getEnumUnderlyingTypeChecked t |> nearestBiggerTypeForEvaluationStack
        | _ -> __notImplemented__()

    let castToEvaluationStackType x =
        match typeOf x with
        // This case is needed for references to primitive types
        | _ when isReference x -> x
        // TODO: need to add conversion from bool to int?
        // | Bool -> cast x Int32
        | Numeric typ -> nearestBiggerTypeForEvaluationStack typ |> cast x
        | _ -> x
