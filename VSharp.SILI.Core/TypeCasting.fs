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

    let private makeSubtypeBoolConst left right =
        let subtypeName = sprintf "(%O <: %O)" left right
        let source = {left = left; right = right}
        Constant subtypeName source typeof<bool>

    let rec commonTypeIsType leftType rightType =
        let boolConst left right = makeSubtypeBoolConst (ConcreteType left) (ConcreteType right)
        isConcreteSubtype leftType rightType makeBool boolConst

    // left is subtype of right
    let typeIsType = commonTypeIsType

    let commonAddressIsType leftAddress leftType targetTyp =
        let typeCheck address =
            let boolConst address =
                match address.term with
                | ConcreteHeapAddress _ -> False
                | _ -> makeSubtypeBoolConst (SymbolicType address) (ConcreteType targetTyp)
            commonTypeIsType leftType targetTyp ||| boolConst address
        Merging.guardedApply typeCheck leftAddress

    let addressIsType = commonAddressIsType

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
        | Ref address ->
            let rightType = typeOfAddress address
            typeIsType typ rightType
        | Union gvs -> gvs |> List.map (fun (g, v) -> (g, typeIsRef state typ v)) |> Merging.merge
        | _ -> internalfailf "Checking subtyping: expected heap reference, but got %O" ref

    let rec commonRefIsType state ref typ =
        match ref.term with
        | HeapRef(addr, sightType) ->
            let leftType = Memory.mostConcreteTypeOfHeapRef state addr sightType
            commonAddressIsType addr leftType typ
        | Ref address ->
            let leftType = typeOfAddress address
            typeIsType leftType typ
        | Union gvs -> gvs |> List.map (fun (g, v) -> (g, commonRefIsType state v typ)) |> Merging.merge
        | _ -> internalfailf "Checking subtyping: expected heap reference, but got %O" ref

    let refIsType = commonRefIsType
    let refIsAssignableToType = commonRefIsType

    let rec refIsRef state leftRef rightRef =
        match leftRef.term, rightRef.term with
        | HeapRef(leftAddr, leftSightType), HeapRef(rightAddr, rightSightType) ->
            let leftType = Memory.mostConcreteTypeOfHeapRef state leftAddr leftSightType
            let rightType = Memory.mostConcreteTypeOfHeapRef state rightAddr rightSightType
            addressIsAddress leftAddr leftType rightAddr rightType
        | Ref leftAddress, HeapRef(rightAddr, rightSightType) ->
            let leftType = typeOfAddress leftAddress
            let rightType = Memory.mostConcreteTypeOfHeapRef state rightAddr rightSightType
            typeIsAddress leftType rightAddr rightType
        | HeapRef(leftAddr, leftSightType), Ref rightAddress ->
            let leftType = Memory.mostConcreteTypeOfHeapRef state leftAddr leftSightType
            let rightType = typeOfAddress rightAddress
            addressIsType leftAddr leftType rightType
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
                    let notMock() = addressIsType l (Memory.typeOfHeapLocation state l) (fillType r)
                    match l with
                    | {term = ConcreteHeapAddress addr} ->
                        // None when addr is null
                        match PersistentDict.tryFind state.allocatedTypes addr with
                        | Some(MockType mockType) ->
                            let assignableExists = mockType.SuperTypes |> Seq.exists (fun st -> st.IsAssignableTo r)
                            if assignableExists then True else False
                        | _ -> notMock()
                    | _ -> notMock()
                | ConcreteType l, SymbolicType r ->
                    let r = fillTerm r
                    let notMock() = typeIsAddress (fillType l) r (Memory.typeOfHeapLocation state r)
                    match r with
                    | {term = ConcreteHeapAddress addr} ->
                        // None when addr is null
                        match PersistentDict.tryFind state.allocatedTypes addr with
                        | Some(MockType _) -> False
                        | _ -> notMock()
                    | _ -> notMock()
                | ConcreteType l, ConcreteType r -> typeIsType (fillType l) (fillType r)

    let private doCast term targetType =
        match term.term, targetType with
        | Ptr(address, _, indent), Pointer typ' -> Ptr address typ' indent
        // Converting ptr to number (conv.u8 instruction, for example) results in the same ptr, because number conversion is pointless
        | Ptr _, Numeric _ -> term
        | Ptr(HeapLocation(address, _), _, ConcreteT(:? int as offset, _)), ByRef t when address = zeroAddress && offset = 0 -> nullRef t
        // CASE: pointer from concolic
        | Ptr(address, Void, offset), ByRef typ' -> Ptr address typ' offset // TODO: need to change type?
        | Ptr _, ByRef _ ->
            Logger.trace "Casting nonnull ptr to ByRef type %O" targetType
            term
        | Ref _, ByRef _ -> term
        | Ref address, Pointer typ' ->
            let baseAddress, offset = Pointers.addressToBaseAndOffset address
            Ptr baseAddress typ' offset
        | Ref address, _ when typeOfAddress address = targetType -> term
        | Ref _, _ ->
            // TODO: can this happen? Ref points to primitive type!
            internalfailf "casting ref %O to type %O" term targetType
        | HeapRef(addr, sightType), _ when isAssignable sightType targetType || isAssignable targetType sightType ->
            HeapRef addr targetType
        | HeapRef _, _ ->
            Logger.trace "unsafe cast in safe context: address %O, type %O" term targetType
            term
//            Ptr (HeapLocation addr) typ (makeNumber 0)
        | Struct _, _ -> internalfailf "Casting struct to %O" targetType
        | _ -> internalfailf "Can't cast %O to type %O" term targetType

    let canCast state term targetType =
        let castCheck term =
            match term.term with
            | Concrete(value, _) -> canCastConcrete value targetType |> makeBool
            | Ptr(_, typ, _) -> typeIsType (typ.MakePointerType()) targetType
            | Ref address -> typeIsType ((Memory.baseTypeOfAddress state address).MakeByRefType()) targetType
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
