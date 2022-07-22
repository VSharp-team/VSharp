namespace VSharp.Core

#nowarn "69"

open System
open System.Collections.Generic
open VSharp
open VSharp.Core
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

    let private doCast term targetType =
        match term.term, targetType with
        | Ptr(address, _, indent), Pointer typ' -> Ptr address typ' indent
        // Converting ptr to number (conv.u8 instruction, for example) results in the same ptr, because number conversion is pointless
        | Ptr _, Numeric _ -> term
        | Ptr(HeapLocation(address, _), _, ConcreteT(:? int as offset, _)), ByRef _ when address = zeroAddress && offset = 0 -> nullRef
        // CASE: pointer from concolic
        | Ptr(address, Void, offset), ByRef typ' -> Ptr address typ' offset // TODO: need to change type?
        | Ptr _, ByRef _ ->
            Logger.trace "Casting nonnull ptr to ByRef type %O" targetType
            term
        | Ref _, ByRef _ -> term
        | Ref address, Pointer typ' ->
            let baseAddress, offset = Pointers.addressToBaseAndOffset address
            Ptr baseAddress typ' offset
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
            | Ptr(_, typ, _) -> typeIsType (Pointer typ) targetType
            | Ref address -> typeIsType (Memory.baseTypeOfAddress state address |> ByRef) targetType
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
            | Null -> nullRef
            | _ -> __unreachable__()
        Merging.guardedApply castUnguarded term

    let rec private nearestBiggerTypeForEvaluationStack (t : Type) =
        match t with
        | _ when t = typeof<int8>    -> Int32
        | _ when t = typeof<int16>   -> Int32
        | _ when t = typeof<int32>   -> Int32
        | _ when t = typeof<int64>   -> Int64
        | _ when t = typeof<byte>    -> UInt32 // TODO: need to use signed?
        | _ when t = typeof<char>    -> UInt32 // TODO: need to use signed?
        | _ when t = typeof<uint16>  -> UInt32 // TODO: need to use signed?
        | _ when t = typeof<uint32>  -> UInt32
        | _ when t = typeof<uint64>  -> UInt64
        | _ when t = typeof<float32> -> F
        | _ when t = typeof<float>   -> D
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

    let solveTypes (model : model) (state : state) =
        let m = CallStack.getCurrentFunc state.stack
        let typeOfAddress addr =
            if VectorTime.less addr VectorTime.zero then model.state.allocatedTypes.[addr]
            else state.allocatedTypes.[addr]
        let supertypeConstraints = Dictionary<concreteHeapAddress, List<Type>>()
        let subtypeConstraints = Dictionary<concreteHeapAddress, List<Type>>()
        let notSupertypeConstraints = Dictionary<concreteHeapAddress, List<Type>>()
        let notSubtypeConstraints = Dictionary<concreteHeapAddress, List<Type>>()
        let addresses = HashSet<concreteHeapAddress>()
        model.state.allocatedTypes |> PersistentDict.iter (fun (addr, _) ->
            addresses.Add(addr) |> ignore
            Dict.getValueOrUpdate supertypeConstraints addr (fun () ->
                let list = List<Type>()
                addr |> typeOfAddress |> toDotNetType |> list.Add
                list) |> ignore)

        let add dict address typ =
            match model.Eval address with
            | {term = ConcreteHeapAddress addr} when addr <> VectorTime.zero ->
                addresses.Add addr |> ignore
                let list = Dict.getValueOrUpdate dict addr (fun () -> List<Type>())
                let typ = toDotNetType typ
                if not <| list.Contains typ then
                    list.Add typ
            | {term = ConcreteHeapAddress _} -> ()
            | term -> internalfailf "Unexpected address %O in subtyping constraint!" term

        PC.toSeq state.pc |> Seq.iter (term >> function
            | Constant(_, TypeSubtypeTypeSource _, _) -> __notImplemented__()
            | Constant(_, RefSubtypeTypeSource(address, typ), _) -> add supertypeConstraints address typ
            | Constant(_, TypeSubtypeRefSource(typ, address), _) -> add subtypeConstraints address typ
            | Constant(_, RefSubtypeRefSource _, _) -> __notImplemented__()
            | Negation({term = Constant(_, TypeSubtypeTypeSource _, _)})-> __notImplemented__()
            | Negation({term = Constant(_, RefSubtypeTypeSource(address, typ), _)}) -> add notSupertypeConstraints address typ
            | Negation({term = Constant(_, TypeSubtypeRefSource(typ, address), _)}) -> add notSubtypeConstraints address typ
            | Negation({term = Constant(_, RefSubtypeRefSource _, _)}) -> __notImplemented__()
            | _ -> ())
        let toList (d : Dictionary<concreteHeapAddress, List<Type>>) addr =
            let l = Dict.tryGetValue d addr null
            if l = null then [] else List.ofSeq l
        let addresses = List.ofSeq addresses
        let inputConstraints =
            addresses
            |> Seq.map (fun addr -> {supertypes = toList supertypeConstraints addr; subtypes = toList subtypeConstraints addr
                                     notSupertypes = toList notSupertypeConstraints addr; notSubtypes = toList notSubtypeConstraints addr})
            |> List.ofSeq
        let typeGenericParameters = m.DeclaringType.GetGenericArguments()
        let methodGenericParameters = if m.IsConstructor then Array.empty else m.GenericArguments
        let solverResult = TypeSolver.solve inputConstraints (Array.append typeGenericParameters methodGenericParameters |> List.ofArray)
        match solverResult with
        | TypeSat(refsTypes, typeParams) ->
            let refineTypes addr (t : Type) =
                let typ = Constructor.fromDotNetType t
                model.state.allocatedTypes <- PersistentDict.add addr typ model.state.allocatedTypes
                if t.IsValueType then
                    let value = makeDefaultValue typ
                    model.state.boxedLocations <- PersistentDict.add addr value model.state.boxedLocations
            Seq.iter2 refineTypes addresses refsTypes
            let classParams, methodParams = List.splitAt typeGenericParameters.Length typeParams
            Some(Array.ofList classParams, Array.ofList methodParams)
        | TypeUnsat -> None
        | TypeVariablesUnknown -> raise (InsufficientInformationException "Could not detect appropriate substitution of generic parameters")
        | TypesOfInputsUnknown -> raise (InsufficientInformationException "Could not detect appropriate types of inputs")

    let checkSatWithSubtyping state =
        match SolverInteraction.checkSat state with
        | SolverInteraction.SmtSat satInfo ->
            let model = satInfo.mdl
            try
                match solveTypes model state with
                | None -> SolverInteraction.SmtUnsat {core = Array.empty}
                | Some _ -> SolverInteraction.SmtSat {satInfo with mdl = model}
            with :? InsufficientInformationException as e ->
                SolverInteraction.SmtUnknown e.Message
        | result -> result
