namespace VSharp.Core

open System
open System.Reflection
open System.Collections.Generic
open FSharpx.Collections
open VSharp
open VSharp.Core
open VSharp.CSharpUtils
open VSharp.TestExtensions

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
        let equalityConstraints = Dictionary<term, HashSet<Type>>()
        let supertypeConstraints = Dictionary<term, HashSet<Type>>()
        let subtypeConstraints = Dictionary<term, HashSet<Type>>()
        let inequalityConstraints = Dictionary<term, HashSet<Type>>()
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
            | Constant(_, TypeCasting.RefEqTypeSource(address, typ), _) ->
                add equalityConstraints address typ |> next
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
            | Negation({term = Constant(_, TypeCasting.RefEqTypeSource(address, typ), _)}) ->
                add inequalityConstraints address typ |> next
            | Negation({term = Constant(_, TypeCasting.TypeSubtypeRefSource(typ, address), _)}) ->
                add notSubtypeConstraints address typ |> next
            | Negation({term = Constant(_, TypeCasting.RefSubtypeRefSource _, _)}) ->
                internalfail "TypeSolver is not fully implemented"
            | Constant(_, (Memory.HeapAddressSource _ as source), _)
            | Constant(_, (Memory.PointerAddressSource _ as source), _) ->
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
                    (toList equalityConstraints address)
                    (toList supertypeConstraints address)
                    (toList subtypeConstraints address)
                    (toList inequalityConstraints address)
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

    let private arrayKinds = [
        OneDimensionalArray
        yield! List.map MultidimensionalArray [1..32]
    ]

    let genericSolvingDepth = 2

    let getAssemblies() =
        seq {
            yield! AssemblyManager.GetAssemblies()
            yield Reflection.mscorlibAssembly
        }

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

    let private enumerateSupertypes predicate (typ : Type) =
        let rec getSupertypes (t : Type) (supertypes : candidate list) =
            if t = null then supertypes
            else
                assert(t <> typeof<Void>)
                let supertypes =
                    match predicate t with
                    | Some c -> c :: supertypes
                    | None -> supertypes
                getSupertypes t.BaseType supertypes
        assert userAssembly.IsSome
        let types = getSupertypes typ List.empty
        candidates(types, None, userAssembly.Value)

    let private enumerateNonAbstractSupertypes predicate (typ : Type) =
        let predicate (t : Type) = if t.IsAbstract then None else predicate t
        enumerateSupertypes predicate typ

    let private hasSubtypes (t : Type) =
        not t.IsSealed && not t.IsArray

    let private canBeMocked (t : Type) =
        (hasSubtypes t && TypeUtils.isPublic t && not (Reflection.hasNonPublicAbstractMethods t))
        || TypeUtils.isDelegate t

    let private isGeneratedMock (t : Type) =
        let generatedAttribute = AssemblyManager.NormalizeType(typeof<GeneratedAttribute>)
        t.GetCustomAttribute(generatedAttribute) <> null
        || t.GetCustomAttribute<GeneratedAttribute>() <> null

    type private candidatePrototype =
        | TypePrototype of Type
        | ArrayPrototype of arrayKind

    let private enumerateTypes supertypes mock (validate : candidatePrototype -> candidate option) (assemblies : Assembly seq) =
        assert userAssembly.IsSome
        let userAssembly = userAssembly.Value
        assert(List.forall (fun t -> t <> typeof<Void>) supertypes)
        let types = seq {
            if List.isEmpty supertypes then
                match TypePrototype typeof<obj> |> validate with
                | Some c -> yield c
                | None -> ()
            else
                yield! Seq.choose (TypePrototype >> validate) supertypes
            if List.forall hasSubtypes supertypes then
                // This case is for reference types and interfaces (because value types are sealed)
                let assemblies =
                    match supertypes |> Seq.tryFind (TypeUtils.isPublic >> not) with
                    | Some u -> Seq.singleton u.Assembly
                    | None ->
                        // Dynamic mock assemblies may appear here
                        let assemblies = assemblies |> Seq.filter (fun a -> not a.IsDynamic)
                        Seq.append [userAssembly; Reflection.mscorlibAssembly] assemblies |> Seq.distinct
                let makeCandidate (t : Type) =
                    // Byref-like can not be casted to any reference type or interface, so filtering them
                    let isInvalid =
                        t.IsByRefLike
                        || t = typeof<Void>
                        || t.GetCustomAttribute<System.Runtime.CompilerServices.CompilerGeneratedAttribute>() <> null
                        || isGeneratedMock t
                    if isInvalid then None
                    else TypePrototype t |> validate
                for assembly in assemblies do
                    let types =
                        if assembly = userAssembly then assembly.GetTypesChecked()
                        else assembly.GetExportedTypesChecked()
                    // TODO: in any assembly, there is no array types, so need to generate it manually
                    yield! Seq.choose makeCandidate types

                yield! List.choose (ArrayPrototype >> validate) arrayKinds
        }

        let mock =
            if List.forall canBeMocked supertypes then
                try
                    Some (mock supertypes)
                with :? InsufficientInformationException -> None
            else None
        candidates(types, mock, userAssembly)

    let private enumerateNonAbstractTypes supertypes mock validate (assemblies : Assembly seq) =
        let validate prototype =
            match prototype with
            | TypePrototype t when t.IsAbstract -> None
            | _ -> validate prototype
        enumerateTypes supertypes mock validate assemblies

    let private chooseCandidate (constraints : typeConstraints) subst c =
        match c with
        | Candidate t as c ->
            if GroundUtils.satisfiesConstraints constraints subst t then Some c
            else None
        | GenericCandidate gc ->
            gc.AddConstraints constraints |> Option.map GenericCandidate
        | ArrayCandidate ac ->
            ac.AddConstraints constraints |> Option.map ArrayCandidate

    let private candidatesFromEquality equalityConstraints validate =
        assert userAssembly.IsSome
        let userAssembly = userAssembly.Value
        assert(List.length equalityConstraints = 1)
        let t = List.head equalityConstraints
        assert(validate t |> Option.isSome)
        let types = Candidate t |> List.singleton
        candidates(types, None, userAssembly)

    let private typeParameterCandidates makeArrayCandidate makeGenericCandidates =
        let getMock _ = EmptyTypeMock() :> ITypeMock
        let validate prototype =
            match prototype with
            | TypePrototype t ->
                if t.IsGenericTypeDefinition then
                    makeGenericCandidates t |> Option.map GenericCandidate
                else Candidate t |> Some
            | ArrayPrototype kind -> makeArrayCandidate kind |> ArrayCandidate |> Some
        let assemblies = getAssemblies()
        enumerateTypes List.empty getMock validate assemblies

    let private typeParameterGroundCandidates getMock subst (parameter : Type, constraints : typeConstraints) =
        let validate prototype =
            match prototype with
            | TypePrototype typ ->
                let subst = PersistentDict.add parameter (ConcreteType typ) subst
                if not typ.IsGenericTypeDefinition
                   && GroundUtils.satisfiesTypeParameterConstraints parameter subst typ
                   && constraints.IsSuitable (GroundUtils.substitute subst) typ
                then
                    Candidate typ |> Some
                else None
            | _ -> None
        let supertypes = constraints.supertypes |> List.map (GroundUtils.substitute subst)
        enumerateTypes supertypes getMock validate (getAssemblies())

    let rec private collectTypeVariables (acc : Type list) (typ : Type) =
        if typ.IsGenericParameter then
            if List.contains typ acc then acc
            else typ.GetGenericParameterConstraints() |> Array.fold collectTypeVariables (typ::acc)
        elif typ.HasElementType then
            typ.GetElementType() |> collectTypeVariables acc
        elif not typ.IsGenericType then acc
        else typ.GetGenericArguments() |> Array.fold collectTypeVariables acc

    let rec private makeParameterSubstitutions childDepth (parameters: Type[]) depth makeGenericCandidate =
        parameterSubstitutions.TryCreate
            parameters
            depth
            (typeParameterCandidates makeArrayCandidate)
            makeGenericCandidate
            childDepth

    and private makeGenericCandidate (typedef : Type) depth =
        let childDepth _ _ _ = Int32.MaxValue
        genericCandidate.TryCreate typedef depth (makeParameterSubstitutions childDepth)

    and private makeArrayCandidate kind =
        let makeArrayParameterSubstitution parameter =
            let parameters = Array.singleton parameter
            let childDepth _ _ _ = Int32.MaxValue
            let substs = makeParameterSubstitutions childDepth parameters genericSolvingDepth makeGenericCandidate
            assert substs.IsSome
            substs.Value
        arrayCandidate(kind, makeArrayParameterSubstitution)

    let private typeCandidates getMock subst constraints (makeGenericCandidates : Type -> genericCandidate option) =
        assert userAssembly.IsSome
        match constraints.supertypes |> List.tryFind (fun t -> t.IsSealed) with
        | Some t ->
            if TypeUtils.isDelegate t then
                // Forcing mock usage for delegate types
                let mock = getMock None constraints.supertypes
                candidates(Seq.empty, Some(mock), userAssembly.Value)
            else
                let types = Candidate t |> Seq.singleton
                candidates(types, None, userAssembly.Value)
        | _ ->
            let makeCandidates prototype =
                match prototype with
                | TypePrototype t ->
                    if t.IsGenericTypeDefinition then
                        makeGenericCandidates t |> Option.map GenericCandidate
                    else Candidate t |> Some
                | ArrayPrototype kind -> makeArrayCandidate kind |> ArrayCandidate |> Some

            let validate t =
                match makeCandidates t with
                | Some c -> chooseCandidate constraints subst c
                | None -> None

            let equal = constraints.equal
            match constraints.subtypes with
            | _ when List.isEmpty equal |> not -> candidatesFromEquality equal (TypePrototype >> validate)
            | [] ->
                let assemblies = getAssemblies()
                enumerateNonAbstractTypes constraints.supertypes (getMock None) validate assemblies
            | t :: _ ->
                enumerateNonAbstractSupertypes (TypePrototype >> validate) t

    let private refineMock getMock constraints (mock : ITypeMock) =
        let supertypeConstraints = constraints.supertypes
        let equalityConstraints = constraints.equal
        let subtypeConstraints = constraints.subtypes
        let hasPrivateSuperType = lazy (List.exists (TypeUtils.isPublic >> not) supertypeConstraints)
        let hasEqualityConstraints = lazy (List.isEmpty equalityConstraints |> not)
        let hasSubtypeConstraints = lazy (List.isEmpty subtypeConstraints |> not)
        let canNotBeMocked = lazy (List.exists (canBeMocked >> not) supertypeConstraints)
        let nonSuitable =
            hasPrivateSuperType.Value
            || hasEqualityConstraints.Value
            || hasSubtypeConstraints.Value
            || canNotBeMocked.Value
        if nonSuitable then None
        else
            let mockSuperTypes = List.ofSeq mock.SuperTypes
            let supertypes =
                if List.isEmpty supertypeConstraints then mockSuperTypes
                else List.concat [mockSuperTypes; supertypeConstraints] |> List.distinct
            let numOfSuperTypes = List.length supertypes
            let numOfMockSuperTypes = List.length mockSuperTypes
            assert(numOfSuperTypes >= numOfMockSuperTypes)
            let changedSupertypes = numOfSuperTypes <> numOfMockSuperTypes
            let mockConstraints = {constraints with supertypes = supertypes}
            let satisfies = mockConstraints.IsContradicting() |> not
            if satisfies && changedSupertypes then getMock (Some mock) supertypes |> Some
            elif satisfies then Some mock
            else None

    let private solveConstraints (getCandidates : _ -> candidates) typesConstraints =
        let typesCandidates = List.map getCandidates typesConstraints
        if typesCandidates |> List.exists (fun c -> c.IsEmpty) then None
        else Some typesCandidates

    let private solveTypesConstraints getMock typesConstraints subst =
        let makeGenericCandidate t = makeGenericCandidate t genericSolvingDepth
        let getCandidates constraints =
            typeCandidates getMock subst constraints makeGenericCandidate
        solveConstraints getCandidates typesConstraints

    let private solveParams getMock subst (typeParameters : Type[]) =
        let isConcrete t =
            match t with
            | ConcreteType _ -> true
            | _ -> false
        let parameterConstraints (t : Type) =
            let superTypes = t.GetGenericParameterConstraints() |> List.ofArray
            t, typeConstraints.FromSuperTypes superTypes
        let processDependent (substs: substitution seq) (p, _ as parameter) =
            seq {
                for subst in substs do
                    let candidates = typeParameterGroundCandidates (getMock None) subst parameter
                    for t in candidates.Types do
                        yield PersistentDict.add p t subst
            }
        let childDepth param (maxDepths: Dictionary<_, _>) depth = depth - maxDepths[param] - 1

        let paramSubsts = makeParameterSubstitutions childDepth typeParameters (genericSolvingDepth + 1) makeGenericCandidate
        let substs =
            match paramSubsts with
            | Some substs -> substs.Substitutions |> Seq.map (PersistentDict.map id ConcreteType)
            | None -> Seq.empty
        let independent, dependent = GenericUtils.splitByDependence typeParameters
        let independentGC = independent |> List.ofSeq |> List.map parameterConstraints
        let independentTypes, _ = List.unzip independentGC
        let candidatesList = solveConstraints (typeParameterGroundCandidates (getMock None) subst) independentGC
        let dependent = Array.map parameterConstraints dependent
        let brutForceSubsts =
            match candidatesList with
            | Some [] ->
                let indepSubst = List.singleton PersistentDict.empty
                Array.fold processDependent indepSubst dependent
            | Some candidatesList ->
                let indepSubsts =
                    candidatesList
                    |> List.map (fun c -> c.Types)
                    |> List.cartesian
                    |> Seq.map (List.zip independentTypes >> PersistentDict.ofSeq)
                Array.fold processDependent indepSubsts dependent
            | None -> Seq.empty
        seq {
            let withMocks = List<_>()
            for subst in brutForceSubsts do
                if PersistentDict.forall (snd >> isConcrete) subst then
                    yield subst
                else withMocks.Add(subst)
            yield! substs
            yield! withMocks
        }

    let rec private solve (getMock : ITypeMock option -> Type list -> ITypeMock) (inputConstraints : typeConstraints list) (typeParameters : Type[]) =
        if inputConstraints |> List.exists (fun c -> c.IsContradicting()) then None
        else
            let decodeTypeSubst (subst : substitution) = CommonUtils.decodeTypeSubst subst typeParameters
            let collectVars acc constraints =
                let acc = constraints.equal |> List.fold collectTypeVariables acc
                let acc = constraints.supertypes |> List.fold collectTypeVariables acc
                let acc = constraints.subtypes |> List.fold collectTypeVariables acc
                let acc = constraints.notEqual |> List.fold collectTypeVariables acc
                let acc = constraints.notSupertypes |> List.fold collectTypeVariables acc
                constraints.notSubtypes |> List.fold collectTypeVariables acc
            let typeVars = Array.fold collectTypeVariables List.empty typeParameters
            let typeVars = List.fold collectVars typeVars inputConstraints |> Array.ofList

            let solveWithSubst subst =
                solveTypesConstraints getMock inputConstraints subst
                |> Option.map (makePair (decodeTypeSubst subst))

            solveParams getMock (pdict.Empty()) typeVars |> Seq.tryPick solveWithSubst |> Option.map (fun (a, b) -> b, a)

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
        if not needToSolve then
            Some (typeStorage.ClassesParams, typeStorage.MethodsParams)
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
        let satisfies = chooseCandidate typeConstraint (pdict.Empty())
        let refineMock = refineMock getMock typeConstraint
        candidates.Filter satisfies refineMock

    let private refineStorage getMock (typeStorage : typeStorage) typeGenericArguments methodGenericArguments =
        let mutable emptyCandidates = false
        let constraints = typeStorage.Constraints
        assert constraints.IsValid()
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
        let resultConstraints = typeConstraints.Empty
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

    let private solveTypesWithoutModel (state : state) =
        let m = CallStack.stackTrace state.memory.Stack |> List.last
        userAssembly <- Some m.DeclaringType.Assembly
        let typeParams, methodParams = getGenericParameters m
        let typeStorage = state.typeStorage
        let getMock = getMock typeStorage.TypeMocks
        refineStorage getMock typeStorage typeParams methodParams

    let private checkInequalityViaCandidates (typeStorage : typeStorage) =
        let unequal = HashSet<term * term>()
        let addressesTypes = typeStorage.AddressesTypes
        for KeyValue(address1, candidates1) in addressesTypes do
            for KeyValue(address2, _) in addressesTypes do
                if address1 <> address2 then
                    let address1Constraints = typeStorage.Constraints[address1].Copy()
                    let address2Constraints = typeStorage.Constraints[address2]
                    address1Constraints.Merge address2Constraints |> ignore
                    let getMock = getMock typeStorage.TypeMocks
                    let refined = refineCandidates getMock address1Constraints candidates1
                    if refined.IsEmpty && unequal.Contains(address2, address1) |> not then
                        unequal.Add(address1, address2) |> ignore
        Some unequal

    let checkInequality (state : state) =
        let typeStorage = state.typeStorage
        let constraints = typeStorage.Constraints
        let mutable hasInterface = false
        for KeyValue(_, addressConstraints) in constraints do
            hasInterface <- hasInterface || addressConstraints.supertypes |> List.exists (fun t -> t.IsInterface)
        if not hasInterface then constraints.CheckInequality()
        else
            match solveTypesWithoutModel state with
            | TypeSat -> checkInequalityViaCandidates typeStorage
            | TypeUnsat -> None

    let private refineTypesInModel model (typeStorage : typeStorage) =
        match model with
        | StateModel modelState ->
            for entry in evalInModel model typeStorage do
                let address = entry.Key
                let typeForModel = entry.Value
                let memory = modelState.memory
                memory.AllocatedTypes <- PersistentDict.add address typeForModel memory.AllocatedTypes
        | PrimitiveModel _ -> internalfail "Refining types in model: got primitive model"

    let solveTypes (model : model) (state : state) =
        let result = solveTypesWithoutModel state
        match result with
        | TypeSat -> refineTypesInModel model state.typeStorage
        | _ -> ()
        result

    let refineTypes (state : state) =
        match solveTypes state.model state with
        | TypeSat -> ()
        | TypeUnsat -> internalfail "Refining types: branch is unreachable"

    let keepOnlyMock state thisRef =
        match thisRef.term with
        | HeapRef({term = ConcreteHeapAddress thisAddress}, _) when VectorTime.less state.startingTime thisAddress -> ()
        | HeapRef(thisAddress, _) ->
            let typeStorage = state.typeStorage
            match typeStorage[thisAddress] with
            | Some candidates ->
                typeStorage[thisAddress] <- candidates.KeepOnlyMock()
            | None -> ()
        | _ -> ()

    let getCallVirtCandidates state (thisRef : heapAddress) (thisType : Type) (ancestorMethod : IMethod) =
        userAssembly <- Some ancestorMethod.DeclaringType.Assembly
        match thisRef.term with
        | HeapRef({term = ConcreteHeapAddress thisAddress}, _) when VectorTime.less state.startingTime thisAddress ->
            state.memory.AllocatedTypes[thisAddress] |> Seq.singleton
        | HeapRef(thisAddress, _) ->
            let thisConstraints = List.singleton thisType |> typeConstraints.FromSuperTypes
            let typeStorage = state.typeStorage
            typeStorage.AddConstraint thisAddress thisConstraints
            let checkOverrides t =
                match t with
                | Candidate t as c when ancestorMethod.IsImplementedInType t -> Some c
                | Candidate _ -> None
                // TODO: check generic and array candidate #types
                | GenericCandidate _ -> None
                | ArrayCandidate _ -> None
            let getMock = getMock typeStorage.TypeMocks
            let result = refineStorage getMock typeStorage Array.empty Array.empty
            match result with
            | TypeSat ->
                let candidates = typeStorage[thisAddress].Value
                let resolveOverride candidate =
                    // TODO: resolve override for array candidates #types
                    match candidate with
                    | Candidate t ->
                        let overridden = ancestorMethod.ResolveOverrideInType t
                        overridden.DeclaringType
                    | GenericCandidate gc ->
                        let overridden = ancestorMethod.ResolveOverrideInType gc.Typedef
                        overridden.DeclaringType
                    | ArrayCandidate _ -> __notImplemented__()
                let checkMockOverrides (m : ITypeMock) =
                    if ancestorMethod.MethodBase.IsPublic then Some m
                    else None
                let filtered = candidates.Filter checkOverrides checkMockOverrides
                let distinct = filtered.DistinctBy(resolveOverride)
                let truncated = distinct.Take(5).Eval()
                typeStorage[thisAddress] <- truncated
                refineTypesInModel state.model typeStorage
                truncated.Types
            | TypeUnsat -> Seq.empty
        | Ref address when ancestorMethod.IsImplementedInType thisType ->
            assert(thisType = address.TypeOfLocation)
            ConcreteType thisType |> Seq.singleton
        | _ -> internalfail $"Getting callvirt candidates: unexpected this {thisRef}"
