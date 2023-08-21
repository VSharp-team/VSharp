namespace VSharp.Core

open System
open System.Reflection
open System.Collections.Generic
open FSharpx.Collections
open VSharp
open VSharp.Core
open VSharp.CSharpUtils

// ------------------------------------------------- Type mocks -------------------------------------------------

type TypeMock(supertypes : Type seq) =
    do
        if supertypes |> Seq.exists (fun s -> s.ContainsGenericParameters) then
            __insufficientInformation__ "Mocks of generic types are not completely supported yet..."

    let mutable supertypes = supertypes
    let uid = Guid.NewGuid()

    interface ITypeMock with
        override x.Name =
            // TODO: generate prettier name without GUIDs
            let supertypeNames = supertypes |> Seq.map (fun t -> t.Name) |> join "_"
            $"Mock_{supertypeNames}_{uid}"
        override x.SuperTypes = supertypes
        override x.IsValueType = supertypes |> Seq.exists (fun t -> t.IsAssignableTo typeof<ValueType> || t.IsValueType)
        override x.Copy() = TypeMock(supertypes)
    static member Empty = TypeMock(Seq.empty)
    override x.ToString() = (x :> ITypeMock).Name
    member x.WithSupertypes(supertypes' : Type seq) : unit =
        supertypes <- supertypes'

// ------------------------------------------------- Type constraints -------------------------------------------------

module TypeStorage =

    // TODO: move this to SolverInteraction and parse all pc at once
    let addTypeConstraints (typesConstraints : typesConstraints) conditions =
        let supertypeConstraints = Dictionary<term, HashSet<Type>>()
        let subtypeConstraints = Dictionary<term, HashSet<Type>>()
        let notSupertypeConstraints = Dictionary<term, HashSet<Type>>()
        let notSubtypeConstraints = Dictionary<term, HashSet<Type>>()
        let addresses = ResizeArray<term>()

        // Creating type constraints from path condition
        let add (dict : Dictionary<term, HashSet<Type>>) address typ =
            let types =
                let types = ref null
                if dict.TryGetValue(address, types) then types.Value
                else
                    let typesSet = HashSet<_>()
                    dict.Add(address, typesSet)
                    addresses.Add address
                    typesSet
            types.Add typ |> ignore

        let addConstraints _ term next into =
            match term.term with
            | Constant(_, TypeCasting.TypeSubtypeTypeSource _, _) ->
                internalfail "TypeSolver is not fully implemented"
            | Constant(_, TypeCasting.RefSubtypeTypeSource(address, typ), _) ->
                add supertypeConstraints address typ |> next
            | Constant(_, TypeCasting.TypeSubtypeRefSource(typ, address), _) ->
                add subtypeConstraints address typ |> next
            | Constant(_, TypeCasting.RefSubtypeRefSource _, _) ->
                internalfail "TypeSolver is not fully implemented"
            | Negation({term = Constant(_, TypeCasting.TypeSubtypeTypeSource _, _)}) ->
                internalfail "TypeSolver is not fully implemented"
            | Negation({term = Constant(_, TypeCasting.RefSubtypeTypeSource(address, typ), _)}) ->
                add notSupertypeConstraints address typ |> next
            | Negation({term = Constant(_, TypeCasting.TypeSubtypeRefSource(typ, address), _)}) ->
                add notSubtypeConstraints address typ |> next
            | Negation({term = Constant(_, TypeCasting.RefSubtypeRefSource _, _)}) ->
                internalfail "TypeSolver is not fully implemented"
            | Constant(_, (Memory.HeapAddressSource _ as source), _) ->
                // Adding super types from testing function info
                add supertypeConstraints term source.TypeOfLocation |> next
            | _ -> into ()
        iterSeq addConstraints conditions

        let toList (d : Dictionary<term, HashSet<Type>>) address =
            let set = ref null
            if d.TryGetValue(address, set) then List.ofSeq set.Value
            else List.empty
        // Adding type constraints
        for address in addresses do
            let typeConstraints =
                typeConstraints.Create
                    (toList supertypeConstraints address)
                    (toList subtypeConstraints address)
                    (toList notSupertypeConstraints address)
                    (toList notSubtypeConstraints address)
            typesConstraints.Add address typeConstraints

    let addTypeConstraint (typesConstraints : typesConstraints) condition =
        List.singleton condition |> addTypeConstraints typesConstraints

// ------------------------------------------------- Type solver core -------------------------------------------------

type typeSolvingResult =
    | TypeSat
    | TypeUnsat

module TypeSolver =

    let mutable private userAssembly = None
    type private substitution = pdict<Type, symbolicType>

    let getAssemblies() =
        seq {
            yield! AssemblyManager.GetAssemblies()
            yield Reflection.mscorlibAssembly
        }

    let private enumerateNonAbstractSupertypes predicate (typ : Type) =
        let rec getNonAbstractSupertypes predicate (t: Type) =
            if typ = null || typ.IsAbstract then List.empty
            else
                let supertypes = getNonAbstractSupertypes predicate t.BaseType
                if predicate typ then typ::supertypes else supertypes
        assert userAssembly.IsSome
        let types = getNonAbstractSupertypes predicate typ
        candidates(types, None, userAssembly.Value)

    let private hasSubtypes (t : Type) =
        not t.IsSealed && not t.IsArray

    let private canBeMocked (t : Type) =
        (hasSubtypes t && TypeUtils.isPublic t) || TypeUtils.isDelegate t

    let private enumerateTypes supertypes mock validate assemblies =
        let types = seq {
            if List.isEmpty supertypes && validate typeof<obj> then
                yield typeof<obj>
            else
                yield! supertypes |> Seq.filter validate
            if List.forall hasSubtypes supertypes then
                // This case is for reference types and interfaces (because value types are sealed)
                let assemblies =
                    match supertypes |> Seq.tryFind (TypeUtils.isPublic >> not) with
                    | Some u -> Seq.singleton u.Assembly
                    | None ->
                        // Dynamic mock assemblies may appear here
                        assemblies |> Seq.filter (fun a -> not a.IsDynamic)
                let suitable (t : Type) =
                    // Byref-like can not be casted to any reference type or interface, so filtering them
                    not t.ContainsGenericParameters && not t.IsByRefLike && validate t
                for assembly in assemblies do
                    let types = assembly.GetExportedTypesChecked()
                    // TODO: in any assembly, there is no array types, so need to generate it manually
                    yield! types |> Seq.filter suitable
        }

        let mock = if List.forall canBeMocked supertypes then Some (mock supertypes) else None
        assert userAssembly.IsSome
        candidates(types, mock, userAssembly.Value)

    let private enumerateNonAbstractTypes supertypes mock validate (assemblies : Assembly seq) =
        enumerateTypes supertypes mock (fun t -> not t.IsAbstract && validate t) assemblies

    let rec private substitute (subst : substitution) (t : Type) =
        let substFunction t =
            match PersistentDict.tryFind subst t with
            | Some (ConcreteType t) -> t
            | _ -> t
        Reflection.concretizeType substFunction t

    let private satisfiesTypeParameterConstraints (parameter : Type) subst (t : Type) =
        let (&&&) = Microsoft.FSharp.Core.Operators.(&&&)
        let specialConstraints = parameter.GenericParameterAttributes &&& GenericParameterAttributes.SpecialConstraintMask
        let isReferenceType = specialConstraints &&& GenericParameterAttributes.ReferenceTypeConstraint = GenericParameterAttributes.ReferenceTypeConstraint
        let isNotNullableValueType = specialConstraints &&& GenericParameterAttributes.NotNullableValueTypeConstraint = GenericParameterAttributes.NotNullableValueTypeConstraint
        let hasDefaultConstructor = specialConstraints &&& GenericParameterAttributes.DefaultConstructorConstraint = GenericParameterAttributes.DefaultConstructorConstraint
        // TODO: check 'managed' constraint
        // Byref-like structures can not be generic argument
        (not t.IsByRefLike) &&
        (not t.ContainsGenericParameters) &&
        (not isReferenceType || not t.IsValueType) &&
        (not isNotNullableValueType || (t.IsValueType && Nullable.GetUnderlyingType t = null)) &&
        (not hasDefaultConstructor || t.IsValueType || not t.IsAbstract && t.GetConstructor(Type.EmptyTypes) <> null) &&
        (parameter.GetGenericParameterConstraints() |> Array.forall (substitute subst >> t.IsAssignableTo))

    let private satisfiesConstraints (constraints : typeConstraints) subst (candidate : Type) =
        // TODO: need to find subst to generic parameters satisfying constraints
        constraints.subtypes |> List.forall (substitute subst >> candidate.IsAssignableFrom) &&
        constraints.supertypes |> List.forall (substitute subst >> candidate.IsAssignableTo) &&
        constraints.notSubtypes |> List.forall (substitute subst >> candidate.IsAssignableFrom >> not) &&
        constraints.notSupertypes |> List.forall (substitute subst >> candidate.IsAssignableTo >> not)

    let private typeCandidates getMock subst constraints =
        assert userAssembly.IsSome
        match constraints.supertypes |> List.tryFind (fun t -> t.IsSealed) with
        | Some t ->
            if TypeUtils.isDelegate t then
                // Forcing mock usage for delegate types
                let mock = getMock None constraints.supertypes
                candidates(Seq.empty, Some(mock), userAssembly.Value)
            else
                let types = t |> Seq.singleton
                candidates(types, None, userAssembly.Value)
        | _ ->
            let validate = satisfiesConstraints constraints subst
            match constraints.subtypes with
            | [] ->
                let assemblies = getAssemblies()
                enumerateNonAbstractTypes constraints.supertypes (getMock None) validate assemblies
            | t :: _ ->
                 enumerateNonAbstractSupertypes validate t

    let private typeParameterCandidates getMock subst (parameter : Type, constraints : typeConstraints) =
        let validate typ = satisfiesTypeParameterConstraints parameter subst typ
        let supertypes = constraints.supertypes |> List.map (substitute subst)
        enumerateTypes supertypes getMock validate (getAssemblies())

    let rec private collectTypeVariables (acc : Type list) (typ : Type) =
        if typ.IsGenericParameter then
            if List.contains typ acc then acc
            else typ.GetGenericParameterConstraints() |> Array.fold collectTypeVariables (typ::acc)
        elif typ.HasElementType then
            typ.GetElementType() |> collectTypeVariables acc
        elif not typ.IsGenericType then acc
        else typ.GetGenericArguments() |> Array.fold collectTypeVariables acc

    let private getMock (typeMocks : IDictionary<Type list, ITypeMock>) (current : ITypeMock option) (supertypes : Type list) : ITypeMock =
        let supertypes = supertypes |> List.sortBy (fun t -> {t=t})
        let mock = ref (TypeMock.Empty :> ITypeMock)
        if typeMocks.TryGetValue(supertypes, mock) then mock.Value
        else
            match current with
            | Some (:? TypeMock as currentMock as current) ->
                let oldSupertypes = current.SuperTypes |> List.ofSeq
                currentMock.WithSupertypes supertypes
                let success = typeMocks.Remove(oldSupertypes)
                assert success
                typeMocks.Add(supertypes, currentMock)
                currentMock
            | None ->
                let mock = TypeMock(supertypes)
                typeMocks.Add(supertypes, mock)
                mock
            | Some _  -> __unreachable__()

    let private generateGenericConstraints (typeVars : Type list) =
        // TODO: divide dependent constraints into groups by dependence
        let isIndependent (t : Type) =
            t.GetGenericParameterConstraints()
            |> Array.forall (collectTypeVariables [] >> List.isEmpty)
        let parameterConstraints (t : Type) =
            let superTypes = t.GetGenericParameterConstraints() |> List.ofArray
            t, typeConstraints.FromSuperTypes superTypes
        let indep, dep = List.partition isIndependent typeVars
        let getConstraints = List.map parameterConstraints
        getConstraints indep, [getConstraints dep]

    let private refineMock getMock constraints (mock : ITypeMock) =
        let constraintsSuperTypes = constraints.supertypes
        let hasPrivateSuperType = List.exists (TypeUtils.isPublic >> not) constraintsSuperTypes
        if hasPrivateSuperType then None
        else
            let mockSuperTypes = List.ofSeq mock.SuperTypes
            let supertypes =
                if List.isEmpty constraintsSuperTypes then mockSuperTypes
                else List.concat [mockSuperTypes; constraintsSuperTypes] |> List.distinct
            let numOfSuperTypes = List.length supertypes
            let numOfMockSuperTypes = List.length mockSuperTypes
            assert(numOfSuperTypes >= numOfMockSuperTypes)
            let changedSupertypes = numOfSuperTypes <> numOfMockSuperTypes
            let mockConstraints = {constraints with supertypes = supertypes}
            let satisfies =
                List.isEmpty constraints.subtypes
                && (mockConstraints.IsContradicting() |> not)
                && List.forall canBeMocked constraints.supertypes
            if satisfies && changedSupertypes then getMock (Some mock) supertypes |> Some
            elif satisfies then Some mock
            else None

    let private solveConstraints typesConstraints (getCandidates : _ -> candidates) =
        let typesCandidates = List.map getCandidates typesConstraints
        if typesCandidates |> List.exists (fun c -> c.IsEmpty) then None
        else Some typesCandidates

    let private solveTypesConstraints getMock typesConstraints subst =
        solveConstraints typesConstraints (typeCandidates getMock subst)

    let private solveGenericConstraints getMock indTypesConstraints subst =
        let refineSubst (candidatesList : candidates list) =
            let candidates = candidatesList |> List.map (fun l -> l.Pick())
            let types, _  = List.unzip indTypesConstraints
            List.zip types candidates
            |> PersistentDict.ofSeq
            |> Some
        solveConstraints indTypesConstraints (typeParameterCandidates getMock subst)
        |> Option.bind refineSubst

    // 'typeParameters' must contain either not generic type or generic parameter
    let private decodeTypeSubst (subst : substitution) typeParameters =
        let getSubst (typ : Type) =
            if typ.IsGenericParameter then PersistentDict.find subst typ
            else
                assert(not typ.ContainsGenericParameters)
                ConcreteType typ
        Array.map getSubst typeParameters

    let rec private solve (getMock : ITypeMock option -> Type list -> ITypeMock) (inputConstraints : typeConstraints list) (typeParameters : Type[]) =
        if inputConstraints |> List.exists (fun c -> c.IsContradicting()) then None
        else
            let decodeTypeSubst (subst : substitution) = decodeTypeSubst subst typeParameters
            let collectVars acc constraints =
                let acc = constraints.supertypes |> List.fold collectTypeVariables acc
                let acc = constraints.subtypes |> List.fold collectTypeVariables acc
                let acc = constraints.notSupertypes |> List.fold collectTypeVariables acc
                constraints.notSubtypes |> List.fold collectTypeVariables acc
            let typeVars = Array.fold collectTypeVariables List.empty typeParameters
            let typeVars = List.fold collectVars typeVars inputConstraints

            let indepGC, depGC = generateGenericConstraints typeVars
            let subst = solveGenericConstraints (getMock None) indepGC (pdict.Empty())
            match subst with
            | None -> None
            | Some subst ->
                // TODO: do solving dependent generic constraints in more complex way
                let depGC = List.concat depGC
                let rec solveTypesVarsRec subst = function
                    | [] ->
                        match solveTypesConstraints getMock inputConstraints subst with
                        | None -> None
                        | Some candidates -> Some (candidates, decodeTypeSubst subst)
                    | (t, c) :: rest ->
                        let candidates = typeParameterCandidates (getMock None) subst (t, c)
                        let rec tryCandidates subst = function
                            | Seq.Empty -> None
                            | Seq.Cons(cand, cands) ->
                                match solveTypesVarsRec (PersistentDict.add t cand subst) rest with
                                | None -> tryCandidates subst cands
                                | x -> x
                        tryCandidates subst candidates.Types
                solveTypesVarsRec subst depGC

    let private getGenericParameters (m : IMethod) =
        let declaringType = m.DeclaringType
        let typeGenericArguments =
            if declaringType.IsGenericType then declaringType.GetGenericArguments()
            else Array.empty
        let methodGenericArguments = m.GenericArguments
        typeGenericArguments, methodGenericArguments

    let solveMethodParameters (typeStorage : typeStorage) (m : IMethod) =
        let declaringType = m.DeclaringType
        userAssembly <- Some declaringType.Assembly
        let methodBase = m.MethodBase
        let needToSolve =
            declaringType.IsGenericType && Array.isEmpty typeStorage.ClassesParams
            || methodBase.IsGenericMethod && Array.isEmpty typeStorage.MethodsParams
        if not needToSolve then Some(typeStorage.ClassesParams, typeStorage.MethodsParams)
        else
            let typeParams, methodParams = getGenericParameters m
            let genericParams = Array.append typeParams methodParams
            let solvingResult = solve (getMock typeStorage.TypeMocks) List.empty genericParams
            match solvingResult with
            | Some (_, genericParams) ->
                let classParams, methodParams = Array.splitAt typeParams.Length genericParams
                typeStorage.ClassesParams <- classParams
                typeStorage.MethodsParams <- methodParams
                Some(classParams, methodParams)
            | None -> None

    let private refineCandidates getMock typeConstraint (candidates : candidates)  =
        let satisfies (t : Type) = satisfiesConstraints typeConstraint (pdict.Empty()) t
        let refineMock = refineMock getMock typeConstraint
        candidates.Filter(satisfies, refineMock)

    let private refineStorage getMock (typeStorage : typeStorage) typeGenericArguments methodGenericArguments =
        let mutable emptyCandidates = false
        let constraints = typeStorage.Constraints
        let addressesTypes = typeStorage.AddressesTypes
        let newAddresses = Dictionary<term, typeConstraints>()

        for address in constraints.NewAddresses do
            if not emptyCandidates then
                let typeConstraint = constraints[address]
                let candidates = ref (candidates.Empty())
                if addressesTypes.TryGetValue(address, candidates) then
                    let candidates = refineCandidates getMock typeConstraint candidates.Value
                    if candidates.IsEmpty then emptyCandidates <- true
                    addressesTypes[address] <- candidates
                else newAddresses.Add(address, typeConstraint)

        constraints.ClearNewAddresses()
        let addresses = newAddresses.Keys
        if emptyCandidates then TypeUnsat
        elif addresses.Count = 0 then
            assert typeStorage.IsValid
            TypeSat
        else
            let addresses = List.ofSeq addresses
            let constraints = List.ofSeq newAddresses.Values
            let genericParams = Array.append typeGenericArguments methodGenericArguments
            match solve getMock constraints genericParams with
            | None -> TypeUnsat
            | Some (candidates, typeParams) ->
                let addCandidates address types = addressesTypes.Add(address, types)
                List.iter2 addCandidates addresses candidates
                assert typeStorage.IsValid
                if Array.isEmpty genericParams |> not then
                    let classParams, methodParams = Array.splitAt typeGenericArguments.Length typeParams
                    typeStorage.ClassesParams <- classParams
                    typeStorage.MethodsParams <- methodParams
                TypeSat

    let private addressInModel (model : model) address =
        match model.Eval address with
        | {term = ConcreteHeapAddress address} -> address
        | _ -> internalfail $"[Type solver] evaluating address in model: unexpected address {address}"

    let private mergeConstraints (constraints : typesConstraints) (addresses : term seq) =
        let resultConstraints = typeConstraints.Empty()
        for address in addresses do
            let constraints = constraints[address]
            resultConstraints.Merge constraints |> ignore
        resultConstraints

    let private evalInModel model (typeStorage : typeStorage) =
        // Clustering addresses, which are equal in model
        let eqInModel = Dictionary<concreteHeapAddress, List<term>>()
        let addressesTypes = typeStorage.AddressesTypes
        for entry in addressesTypes do
            let address = entry.Key
            let concreteAddress = addressInModel model address
            if concreteAddress <> VectorTime.zero then
                let current = ref null
                if eqInModel.TryGetValue(concreteAddress, current) then
                    let same = current.Value
                    same.Add(address)
                else
                    let same = List()
                    same.Add(address)
                    eqInModel.Add(concreteAddress, same)

        // Intersecting type candidates for same addresses in model
        let evaledTypes = Dictionary<concreteHeapAddress, symbolicType>()
        let constraints = typeStorage.Constraints
        // Configuring 'getMock' to create only new mocks (not refining existing)
        let getMock _ supertypes = getMock typeStorage.TypeMocks None supertypes
        for entry in eqInModel do
            let same = entry.Value
            let evaledType =
                let address = Seq.head same
                let candidates = addressesTypes[address]
                assert(candidates.IsEmpty |> not)
                if same.Count > 1 then
                    let merged = mergeConstraints constraints same
                    let refined = refineCandidates getMock merged candidates
                    assert(refined.IsEmpty |> not)
                    refined.Pick()
                else candidates.Pick()
            evaledTypes.Add(entry.Key, evaledType)
        evaledTypes

    let private refineTypesInModel model (typeStorage : typeStorage) =
        match model with
        | StateModel modelState ->
            for entry in evalInModel model typeStorage do
                let address = entry.Key
                let typeForModel = entry.Value
                modelState.allocatedTypes <- PersistentDict.add address typeForModel modelState.allocatedTypes
        | PrimitiveModel _ -> internalfail "Refining types in model: got primitive model"

    let solveTypes (model : model) (state : state) =
        let m = CallStack.stackTrace state.stack |> List.last
        userAssembly <- Some m.DeclaringType.Assembly
        let typeParams, methodParams = getGenericParameters m
        let typeStorage = state.typeStorage
        let getMock = getMock typeStorage.TypeMocks
        let result = refineStorage getMock typeStorage typeParams methodParams
        match result with
        | TypeSat -> refineTypesInModel model typeStorage
        | _ -> ()
        result

    let refineTypes (state : state) =
        match solveTypes state.model state with
        | TypeSat _ -> ()
        | TypeUnsat -> internalfail "Refining types: branch is unreachable"

    let getCallVirtCandidates state (thisRef : heapAddress) (thisType: Type) (ancestorMethod : IMethod) =
        userAssembly <- Some ancestorMethod.DeclaringType.Assembly
        match thisRef.term with
        | HeapRef({term = ConcreteHeapAddress thisAddress}, _) when VectorTime.less state.startingTime thisAddress ->
            state.allocatedTypes[thisAddress] |> Seq.singleton
        | HeapRef(thisAddress, _) ->
            let thisConstraints = List.singleton thisType |> typeConstraints.FromSuperTypes
            let typeStorage = state.typeStorage
            typeStorage.AddConstraint thisAddress thisConstraints
            let ancestorMethod = ancestorMethod.MethodBase :?> MethodInfo
            let checkOverrides t =
                 Reflection.canOverrideMethod t ancestorMethod
            let getMock = getMock typeStorage.TypeMocks
            let result = refineStorage getMock typeStorage Array.empty Array.empty
            match result with
            | TypeSat ->
                let candidates = typeStorage[thisAddress].Value
                let optionMock m = Some m
                let filtered = candidates.Filter(checkOverrides, optionMock)
                let truncated = filtered.Take(5)
                typeStorage[thisAddress] <- truncated.Eval()
                truncated.Types
            | TypeUnsat -> Seq.empty
        | Ref address when Reflection.typeImplementsMethod thisType (ancestorMethod.MethodBase :?> MethodInfo) ->
            assert(thisType = typeOfAddress address)
            ConcreteType thisType |> Seq.singleton
        | _ -> internalfail $"Getting callvirt candidates: unexpected this {thisRef}"
