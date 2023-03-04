namespace VSharp.Core

open System
open System.Reflection
open System.Collections.Generic
open FSharpx.Collections
open VSharp
open VSharp.Core

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


// ------------------------------------------------- Type solver core -------------------------------------------------

type typeSolvingResult =
    | TypeSat of typeModel
    | TypeUnsat

module TypeSolver =
    type private substitution = pdict<Type, symbolicType>

    let rec private enumerateNonAbstractSupertypes predicate (typ : Type) =
        if typ = null || typ.IsAbstract then List.empty
        else
            let supertypes = enumerateNonAbstractSupertypes predicate typ.BaseType
            if predicate typ then typ::supertypes else supertypes

    let private enumerateTypes (supertypes : Type list) (mock : Type list -> ITypeMock) validate (assemblies : Assembly seq) =
        seq {
            let mutable hasPrivateSuperType = false
            yield! supertypes |> Seq.filter validate |> Seq.map ConcreteType
            let assemblies =
                match supertypes |> Seq.tryFind (TypeUtils.isPublic >> not) with
                | Some u -> hasPrivateSuperType <- true; Seq.singleton u.Assembly
                | None ->
                    // Dynamic mock assemblies may appear here
                    assemblies |> Seq.filter (fun a -> not a.IsDynamic)
            for assembly in assemblies do
                let types = assembly.GetExportedTypes()
                yield! types |> Seq.filter (fun t -> not t.ContainsGenericParameters && validate t) |> Seq.map ConcreteType
            if not hasPrivateSuperType then
                yield mock supertypes |> MockType
        }

    let private enumerateNonAbstractTypes supertypes mock validate (assemblies : Assembly seq) =
        enumerateTypes supertypes mock (fun t -> not t.IsAbstract && validate t) assemblies

    let private isContradicting (c : typeConstraints) =
        let nonComparable (t : Type) (u : Type) =
            u.IsClass && t.IsClass && (not <| u.IsAssignableTo t) && (not <| u.IsAssignableFrom t)
        // X <: u and u <: t and X </: t
        c.supertypes |> List.exists (fun u -> c.notSupertypes |> List.exists u.IsAssignableTo)
        || // u <: X and t <: u and t </: X
        c.subtypes |> List.exists (fun u -> c.notSubtypes |> List.exists u.IsAssignableFrom)
        || // u <: X and X <: t and u </: t
        c.subtypes |> List.exists (fun u -> c.supertypes |> List.exists (u.IsAssignableTo >> not))
        || // No multiple inheritance -- X <: u and X <: t and u </: t and t </: u and t, u are classes
        c.supertypes |> List.exists (fun u -> c.supertypes |> List.exists (nonComparable u))
        || // u </: X and X <: u when u is sealed
        c.supertypes |> List.exists (fun u -> u.IsSealed && c.notSubtypes |> List.contains u)

    let rec private substitute (subst : substitution) (t : Type) =
        let substFunction t =
            match PersistentDict.tryFind subst t with
            | Some (ConcreteType t) -> t
            | _ -> t
        Reflection.concretizeType substFunction t

    let private satisfiesTypeParameterConstraints (parameter : Type) subst (t : Type) =
        let (&&&) = Microsoft.FSharp.Core.Operators.(&&&)
        let isReferenceType = parameter.GenericParameterAttributes &&& GenericParameterAttributes.ReferenceTypeConstraint = GenericParameterAttributes.ReferenceTypeConstraint
        let isNotNullableValueType = parameter.GenericParameterAttributes &&& GenericParameterAttributes.NotNullableValueTypeConstraint = GenericParameterAttributes.NotNullableValueTypeConstraint
        let hasDefaultConstructor = parameter.GenericParameterAttributes &&& GenericParameterAttributes.DefaultConstructorConstraint = GenericParameterAttributes.NotNullableValueTypeConstraint
        // TODO: check 'managed' constraint
        (not t.ContainsGenericParameters) &&
        (not isReferenceType || not t.IsValueType) &&
        (not isNotNullableValueType || (t.IsValueType && Nullable.GetUnderlyingType t = null)) &&
        (not hasDefaultConstructor || t.GetConstructor(Type.EmptyTypes) <> null) &&
        (parameter.GetGenericParameterConstraints() |> Array.forall (substitute subst >> t.IsAssignableTo))

    let private satisfiesConstraints (constraints : typeConstraints) subst (candidate : Type) =
        // TODO: need to find subst to generic parameters satisfying constraints
        constraints.subtypes |> List.forall (substitute subst >> candidate.IsAssignableFrom) &&
        constraints.supertypes |> List.forall (substitute subst >> candidate.IsAssignableTo) &&
        constraints.notSubtypes |> List.forall (substitute subst >> candidate.IsAssignableFrom >> not) &&
        constraints.notSupertypes |> List.forall (substitute subst >> candidate.IsAssignableTo >> not)

    let private typeCandidates getMock subst constraints =
        match constraints.supertypes |> List.tryFind (fun t -> t.IsSealed) with
        | Some t ->
            if TypeUtils.isDelegate t then
                // Forcing mock usage for delegate types
                let mock = getMock None constraints.supertypes
                MockType mock |> Seq.singleton
            else
                ConcreteType t |> Seq.singleton
        | _ ->
            let validate = satisfiesConstraints constraints subst
            match constraints.subtypes with
            | [] ->
                let assemblies = AssemblyManager.GetAssemblies()
                enumerateNonAbstractTypes constraints.supertypes (getMock None) validate assemblies
            | t :: _ -> enumerateNonAbstractSupertypes validate t |> Seq.map ConcreteType

    let private typeParameterCandidates getMock subst (parameter : Type, constraints : typeConstraints) =
        let validate typ = satisfiesTypeParameterConstraints parameter subst typ
        let supertypes = constraints.supertypes |> List.map (substitute subst)
        enumerateTypes supertypes getMock validate (AssemblyManager.GetAssemblies())

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

    let private addressInModel (model : model) address =
        match model.Eval address with
        | {term = ConcreteHeapAddress address} -> address
        | address -> internalfail $"[Type solver] evaluating address in model: unexpected address {address}"

    let private generateConstraints state (model : model) conditions =
        match model with
        | StateModel(_, typeModel) ->
            let supertypeConstraints = Dictionary<concreteHeapAddress, HashSet<Type>>()
            let subtypeConstraints = Dictionary<concreteHeapAddress, HashSet<Type>>()
            let notSupertypeConstraints = Dictionary<concreteHeapAddress, HashSet<Type>>()
            let notSubtypeConstraints = Dictionary<concreteHeapAddress, HashSet<Type>>()
            let evaledAddresses = Dictionary<term, concreteHeapAddress>()

            // Creating type constraints from path condition
            let add (dict : Dictionary<concreteHeapAddress, HashSet<Type>>) address typ =
                match model.Eval address with
                | {term = ConcreteHeapAddress concreteAddress} when concreteAddress <> VectorTime.zero ->
                    evaledAddresses[address] <- concreteAddress
                    let types =
                        let types = ref null
                        if dict.TryGetValue(concreteAddress, types) then types.Value
                        else
                            let typesSet = HashSet<_>()
                            dict.Add(concreteAddress, typesSet)
                            typesSet
                    types.Add typ |> ignore
                | {term = ConcreteHeapAddress _} -> ()
                | term -> internalfailf "Unexpected address %O in subtyping constraint!" term

            let rec addConstraints acc condition k =
                match condition.term with
                | Constant(_, TypeCasting.TypeSubtypeTypeSource _, _) ->
                    internalfail "TypeSolver is not fully implemented"
                | Constant(_, TypeCasting.RefSubtypeTypeSource(address, typ), _) ->
                    add supertypeConstraints address typ |> k
                | Constant(_, TypeCasting.TypeSubtypeRefSource(typ, address), _) ->
                    add subtypeConstraints address typ |> k
                | Constant(_, TypeCasting.RefSubtypeRefSource _, _) ->
                    internalfail "TypeSolver is not fully implemented"
                | Negation({term = Constant(_, TypeCasting.TypeSubtypeTypeSource _, _)}) ->
                    internalfail "TypeSolver is not fully implemented"
                | Negation({term = Constant(_, TypeCasting.RefSubtypeTypeSource(address, typ), _)}) ->
                    add notSupertypeConstraints address typ |> k
                | Negation({term = Constant(_, TypeCasting.TypeSubtypeRefSource(typ, address), _)}) ->
                    add notSubtypeConstraints address typ |> k
                | Negation({term = Constant(_, TypeCasting.RefSubtypeRefSource _, _)}) ->
                    internalfail "TypeSolver is not fully implemented"
                | Conjunction(xs) ->
                    Cps.List.foldlk addConstraints acc xs k
                | _ -> k ()

            Cps.Seq.foldlk addConstraints () conditions id

            let typeOfAddress address =
                match address.term with
                | Constant(_, source, _) -> source.TypeOfLocation
                | ConcreteHeapAddress a ->
                    match state.allocatedTypes[a] with
                    | ConcreteType t -> t
                    | MockType m ->  internalfail $"Generating constraints: unexpected mock from allocatedTypes {m}"
                | _ -> internalfail $"Generating constraints: unexpected address {address}"

            let toList (d : Dictionary<concreteHeapAddress, HashSet<Type>>) address =
                let set = ref null
                if d.TryGetValue(address, set) then List.ofSeq set.Value
                else List.empty
            let addresses = ResizeArray<term>()
            for entry in evaledAddresses do
                let address = entry.Key
                let evaled = entry.Value
                addresses.Add address
                // Adding super types from parameters info of testing function
                let superType = typeOfAddress address
                let superTypes = toList supertypeConstraints evaled
                let superTypes =
                    if superType <> typeof<obj> then superType :: superTypes |> List.distinct
                    else superTypes
                let typeConstraint =
                    {
                        supertypes = superTypes
                        subtypes = toList subtypeConstraints evaled
                        notSupertypes = toList notSupertypeConstraints evaled
                        notSubtypes = toList notSubtypeConstraints evaled
                    }
                typeModel.AddConstraint address typeConstraint

            // Clustering type constraints with same address in model
            // TODO: make more efficient #type
            let eqInModel = Dictionary<concreteHeapAddress, List<term>>()
            let currentConstraints = typeModel.constraints
            for entry in currentConstraints do
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

            // Merging constraints with same address in model
            for entry in eqInModel do
                let same = entry.Value
                if same.Count > 1 then
                    let mutable resultConstraints = typeConstraints.Empty
                    for address in same do
                        let constraints = currentConstraints[address]
                        resultConstraints <- resultConstraints.Merge constraints
                        if addresses.Contains address |> not then addresses.Add address
                    for address in same do
                        currentConstraints[address] <- resultConstraints

            // Adding new constraints and all which have same address in model
            let constraints = ResizeArray<typeConstraints>()
            for address in addresses do
                constraints.Add currentConstraints[address]
            addresses.ToArray(), constraints.ToArray()
        | PrimitiveModel _ -> __unreachable__()

    let private generateGenericConstraints (typeVars : Type list) =
        // TODO: divide dependent constraints into groups by dependence
        let isIndependent (t : Type) =
            t.GetGenericParameterConstraints()
            |> Array.forall (collectTypeVariables [] >> List.isEmpty)
        let parameterConstraints (t : Type) =
            t, { typeConstraints.Empty with supertypes = t.GetGenericParameterConstraints() |> List.ofArray}
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
            let satisfies = List.isEmpty constraints.subtypes && (isContradicting mockConstraints |> not)
            if satisfies && changedSupertypes then getMock (Some mock) supertypes |> Some
            elif satisfies then Some mock
            else None

    let private solveConstraints typesConstraints (getCandidates : _ -> seq<symbolicType>) =
        let typesCandidates = List.map getCandidates typesConstraints
        if List.exists Seq.isEmpty typesCandidates then None
        else Some typesCandidates

    let private solveTypesConstraints getMock typesConstraints subst =
        solveConstraints typesConstraints (typeCandidates getMock subst)

    let private solveGenericConstraints getMock indTypesConstraints subst =
        let refineSubst candidates =
            let candidates = List.map Seq.head candidates
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
        if List.exists isContradicting inputConstraints then None
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
                        tryCandidates subst candidates
                solveTypesVarsRec subst depGC

    let private getGenericParameters (m : IMethod) =
        let declaringType = m.DeclaringType
        let typeGenericArguments =
            if declaringType.IsGenericType then declaringType.GetGenericArguments()
            else Array.empty
        let methodGenericArguments = m.GenericArguments
        typeGenericArguments, methodGenericArguments

    let solveMethodParameters (typeModel : typeModel) (m : IMethod) =
        let declaringType = m.DeclaringType
        let methodBase = m.MethodBase
        let needToSolve =
            declaringType.IsGenericType && Array.isEmpty typeModel.classesParams
            || methodBase.IsGenericMethod && Array.isEmpty typeModel.methodsParams
        if not needToSolve then Some(typeModel.classesParams, typeModel.methodsParams)
        else
            let typeParams, methodParams = getGenericParameters m
            let genericParams = Array.append typeParams methodParams
            let solvingResult = solve (getMock typeModel.typeMocks) List.empty genericParams
            match solvingResult with
            | Some (_, genericParams) ->
                let classParams, methodParams = Array.splitAt typeParams.Length genericParams
                typeModel.classesParams <- classParams
                typeModel.methodsParams <- methodParams
                Some(classParams, methodParams)
            | None -> None

    let private refineModel getMock (typeModel : typeModel) addresses typesConstraints typeGenericArguments methodGenericArguments =
        let mutable emptyCandidates = false
        let addressesTypes = typeModel.addressesTypes
        let addressesLength = Array.length addresses
        assert(addressesLength = Array.length typesConstraints)
        let newAddresses = Dictionary<term, typeConstraints>()

        for i = 0 to addressesLength - 1 do
            if not emptyCandidates then
                let address = addresses[i]
                let typeConstraint = typesConstraints[i]
                let types = ref null
                if addressesTypes.TryGetValue(address, types) then
                    let refineType symbolicType =
                        match symbolicType with
                        | ConcreteType typ ->
                            if satisfiesConstraints typeConstraint (pdict.Empty()) typ then Some symbolicType
                            else None
                        | MockType mock -> refineMock getMock typeConstraint mock |> Option.map MockType
                    let types = Seq.choose refineType types.Value
                    if Seq.isEmpty types then emptyCandidates <- true
                    addressesTypes[address] <- types
                else newAddresses.Add(address, typeConstraint)

        let addresses = newAddresses.Keys
        if emptyCandidates then TypeUnsat
        elif addresses.Count = 0 then TypeSat typeModel
        else
            let addresses = List.ofSeq addresses
            let constraints = List.ofSeq newAddresses.Values
            let genericParams = Array.append typeGenericArguments methodGenericArguments
            match solve getMock constraints genericParams with
            | None -> TypeUnsat
            | Some (candidates, typeParams) ->
                let addToModel address types = typeModel.addressesTypes.Add(address, types)
                List.iter2 addToModel addresses candidates
                if Array.isEmpty genericParams |> not then
                    let classParams, methodParams = Array.splitAt typeGenericArguments.Length typeParams
                    typeModel.classesParams <- classParams
                    typeModel.methodsParams <- methodParams
                TypeSat typeModel

    // TODO: delete, when model from Z3 will not be recreated
    let private refineTypesInModel modelState model typeModel =
        for entry in typeModel.addressesTypes do
            let address = entry.Key
            let types = entry.Value
            assert(Seq.isEmpty types |> not)
            let addressForModel = addressInModel model address
            let typeForModel = Seq.head types
            match typeForModel with
            | ConcreteType t when t.IsValueType ->
                let value = makeDefaultValue t
                modelState.boxedLocations <- PersistentDict.add addressForModel value modelState.boxedLocations
            | _ -> ()
            modelState.allocatedTypes <- PersistentDict.add addressForModel typeForModel modelState.allocatedTypes

    let solveTypes (model : model) (state : state) condition =
        match model with
        | StateModel(modelState, typeModel) ->
            let m = CallStack.stackTrace state.stack |> List.last
            let addresses, constraints = Seq.singleton condition |> generateConstraints state model
            let typeParams, methodParams = getGenericParameters m
            let getMock = getMock typeModel.typeMocks
            let result = refineModel getMock typeModel addresses constraints typeParams methodParams
            match result with
            | TypeSat _ -> refineTypesInModel modelState model typeModel
            | _ -> ()
            result
        | PrimitiveModel _ -> internalfail "Solving types: got primitive model"

    let checkSatWithSubtyping state condition =
        match SolverInteraction.checkSat state with
        | SolverInteraction.SmtSat ({mdl = StateModel(modelState, _) as model} as satInfo) ->
            try
                match solveTypes model state condition with
                | TypeUnsat -> SolverInteraction.SmtUnsat {core = Array.empty}
                | TypeSat typeModel ->
                    SolverInteraction.SmtSat {satInfo with mdl = StateModel(modelState, typeModel)}
            with :? InsufficientInformationException as e ->
                SolverInteraction.SmtUnknown e.Message
        | result -> result

    let refineTypes (state : state) condition =
        match solveTypes state.model state condition with
        | TypeSat _ -> ()
        | TypeUnsat -> internalfail "Refining types: branch is unreachable"

    let getCallVirtCandidates state (thisRef : heapAddress) (thisType: Type) (ancestorMethod : IMethod) =
        match thisRef.term with
        | HeapRef({term = ConcreteHeapAddress thisAddress}, _) when VectorTime.less VectorTime.zero thisAddress ->
            state.allocatedTypes[thisAddress] |> Seq.singleton
        | HeapRef(thisAddress, _) ->
            match state.model with
            | StateModel(_, typeModel) ->
                let thisConstraints = { typeConstraints.Empty with supertypes = thisType |> List.singleton }
                typeModel.AddConstraint thisAddress thisConstraints
                let ancestorMethod = ancestorMethod.MethodBase :?> MethodInfo
                let checkOverrides = function
                    | ConcreteType t -> Reflection.canOverrideMethod t ancestorMethod
                    | MockType _ -> true
                let addresses = Array.singleton thisAddress
                let constraints = Array.singleton thisConstraints
                let getMock = getMock typeModel.typeMocks
                let result = refineModel getMock typeModel addresses constraints Array.empty Array.empty
                match result with
                | TypeSat typeModel -> typeModel[thisAddress].Value |> Seq.filter checkOverrides
                | TypeUnsat -> Seq.empty
            | PrimitiveModel _ -> internalfail "Getting callvirt candidates: got primitive model"
        | _ -> internalfail $"Getting callvirt candidates: unexpected this {thisRef}"
