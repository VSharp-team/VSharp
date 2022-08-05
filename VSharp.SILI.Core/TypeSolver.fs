namespace VSharp.Core

open System
open System.Reflection
open System.Collections.Generic
open FSharpx.Collections
open VSharp
open VSharp.Core

// ------------------------------------------------- Type mocks -------------------------------------------------

[<StructuralEquality; NoComparison>]
type functionResultConstantSource = {mock : MethodMock; callIndex : int; concreteThis : concreteHeapAddress; args : term list}
with
    interface ISymbolicConstantSource with
        override x.TypeOfLocation = x.mock.Method.ReturnType
        override x.SubTerms = []
        override x.Time = VectorTime.zero
    override x.ToString() =
        let args = x.args |> List.map toString |> join ", "
        $"{x.mock.Method.Name}({args}):{x.callIndex}"

and MethodMock(method : IMethod, typeMock : ITypeMock) =
    let mutable callIndex = 0
    let callResults = ResizeArray<term>()

    member x.Method : IMethod = method
    member x.Type : ITypeMock = typeMock
    interface IMethodMock with
        override x.BaseMethod =
            match method.MethodBase with
            | :? MethodInfo as mi -> mi
            | _ -> __notImplemented__()

        override x.Call concretizedThis args =
            let returnType = method.ReturnType
            if returnType = typeof<Void> then None
            else
                let src : functionResultConstantSource = {mock = x; callIndex = callIndex; concreteThis = concretizedThis; args = args}
                let result = Constant (toString src) src returnType
                callIndex <- callIndex + 1
                callResults.Add result
                Some result

        override x.GetImplementationClauses() = callResults.ToArray()

    member private x.SetIndex idx = callIndex <- idx
    member private x.SetClauses clauses =
        callResults.Clear()
        callResults.AddRange clauses

    member internal x.Copy(newTypeMock : ITypeMock) =
        let result = MethodMock(method, newTypeMock)
        result.SetIndex callIndex
        result.SetClauses callResults
        result

type TypeMock private (supertypes : Type seq, methodMocks : IDictionary<IMethod, MethodMock>) =
    do
        if supertypes |> Seq.exists (fun s -> s.ContainsGenericParameters) then
            __insufficientInformation__ "Mocks of generic types are not completely supported yet..."

    let uid = Guid.NewGuid()
    new(supertypes) = TypeMock(supertypes, Dictionary<IMethod, MethodMock>())
    interface ITypeMock with
        override x.Name =
            // TODO: generate prettier name without GUIDs
            let supertypeNames = supertypes |> Seq.map (fun t -> t.Name) |> join "_"
            $"Mock_{supertypeNames}_{uid}"
        override x.SuperTypes = supertypes
        override x.MethodMock m =
            Dict.getValueOrUpdate methodMocks m (fun () -> MethodMock(m, x)) :> IMethodMock
        override x.MethodMocks with get() = methodMocks.Values |> Seq.cast<_>
        override x.Copy() = x.WithSupertypes supertypes
    override x.ToString() = (x :> ITypeMock).Name
    member x.WithSupertypes(supertypes' : Type seq) =
        let newMethods = Dictionary<_,_>()
        let result = TypeMock(supertypes', newMethods)
        methodMocks |> Seq.iter (fun kvp -> newMethods.Add(kvp.Key, kvp.Value.Copy(result)))
        result


// ------------------------------------------------- Type solver core -------------------------------------------------

type typeConstraints = { supertypes : Type list; subtypes : Type list; notSubtypes : Type list; notSupertypes : Type list; mock : ITypeMock option}

type typeSolvingResult =
    | TypeSat of symbolicType list * symbolicType list
    | TypeUnsat

module TypeSolver =
    type private substitution = pdict<Type, symbolicType>

    let rec private enumerateNonAbstractSupertypes predicate (typ : Type) =
        if typ = null || typ.IsAbstract then []
        else
            let supertypes = enumerateNonAbstractSupertypes predicate typ.BaseType
            if predicate typ then typ::supertypes else supertypes

    let private enumerateTypes (supertypes : Type list) (mock : Type list -> ITypeMock) validate (assemblies : Assembly seq) =
        seq {
            let mutable sure = true
            yield! supertypes |> Seq.filter validate |> Seq.map ConcreteType
            let assemblies =
                match supertypes |> Seq.tryFind (fun u -> u.IsNotPublic) with
                | Some u -> [u.Assembly] :> _ seq
                | None -> sure <- false; assemblies
            for assembly in assemblies do
                yield! assembly.GetExportedTypes() |> Seq.filter validate |> Seq.map ConcreteType
            if supertypes |> Seq.forall (fun t -> t.IsPublic) then
                yield mock supertypes |> MockType
        }

    let private enumerateNonAbstractTypes supertypes mock validate (assemblies : Assembly seq) =
        enumerateTypes supertypes mock (fun t -> not t.IsAbstract && validate t) assemblies

    let private isContradicting (c : typeConstraints) =
        // X <: u and u <: t and X </: t
        c.supertypes |> List.exists (fun u -> c.notSupertypes |> List.exists u.IsAssignableTo)
        || // u <: X and t <: u and t </: X
        c.subtypes |> List.exists (fun u -> c.notSubtypes |> List.exists u.IsAssignableFrom)
        || // u <: X and X <: t and u </: t
        c.subtypes |> List.exists (fun u -> c.supertypes |> List.exists (u.IsAssignableTo >> not))
        || // No multiple inheritance -- X <: u and X <: t and u </: t and t </: u and t, u are classes
        c.supertypes |> List.exists (fun u -> c.supertypes |> List.exists (fun t -> u.IsClass && t.IsClass && (not <| u.IsAssignableTo t) && (not <| u.IsAssignableFrom t)))

    let rec private substitute (subst : substitution) (t : Type) =
        if not t.IsGenericType then t
        elif t.IsGenericParameter then
            if PersistentDict.contains t subst then
                match subst.[t] with
                | ConcreteType u -> u
                | _ -> t
            else t
//        elif t.HasElementType then
//            let e' = t.GetElementType() |> substPartially subst
        else
            let args = t.GetGenericArguments()
            let args' = args |> Array.map (substitute subst)
            if Array.forall2 (=) args args' then t
            else t.GetGenericTypeDefinition().MakeGenericType(args')

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

    let private inputCandidates getMock constraints subst =
        let validate = satisfiesConstraints constraints subst
        match constraints.subtypes with
        | [] -> enumerateNonAbstractTypes constraints.supertypes (getMock constraints.mock) validate (AssemblyManager.assemblies())
        | t :: _ -> enumerateNonAbstractSupertypes validate t |> Seq.map ConcreteType

    let private typeParameterCandidates getMock parameter subst =
        let validate typ = satisfiesTypeParameterConstraints parameter subst typ
        let supertypes = parameter.GetGenericParameterConstraints() |> Array.map (substitute subst) |> List.ofArray
        enumerateTypes supertypes getMock validate (AssemblyManager.assemblies())

    let rec private collectTypeVariables (acc : Type list) (typ : Type) =
        if typ.IsGenericParameter then
            if List.contains typ acc then acc
            else
                typ.GetGenericParameterConstraints() |> Array.fold collectTypeVariables (typ::acc)
        elif typ.HasElementType then
            collectTypeVariables acc (typ.GetElementType())
        elif not typ.IsGenericType then acc
        else
            typ.GetGenericArguments() |> Array.fold collectTypeVariables acc

    let private getMock (typeMocks : IDictionary<Type list, ITypeMock>) (current : ITypeMock option) (supertypes : Type list) =
        let supertypes = supertypes |> List.sortBy (fun t -> {t=t})
        Dict.getValueOrUpdate typeMocks supertypes (fun () ->
            match current with
            | Some (:? TypeMock as current)  ->
                let newMock = current.WithSupertypes supertypes
                typeMocks.Add(supertypes, newMock)
                newMock :> ITypeMock
            | Some _  -> __unreachable__()
            | None -> TypeMock(supertypes) :> ITypeMock)

    let private solve (getMock : ITypeMock option -> Type list -> ITypeMock) (inputConstraintsList : typeConstraints list) (typeParameters : Type list) =
        if inputConstraintsList |> List.exists isContradicting then TypeUnsat
        else
            let typeVars = typeParameters |> List.fold collectTypeVariables []
            let typeVars = inputConstraintsList |> List.fold (fun acc constraints ->
                            let acc = constraints.supertypes |> List.fold collectTypeVariables acc
                            let acc = constraints.subtypes |> List.fold collectTypeVariables acc
                            let acc = constraints.notSupertypes |> List.fold collectTypeVariables acc
                            let acc = constraints.notSubtypes |> List.fold collectTypeVariables acc
                            acc) typeVars
            let decodeTypeSubst (subst : substitution) =
                 List.map (PersistentDict.find subst) typeParameters
            let mutable resultInputs = []
            let mutable resultSubst = PersistentDict.empty
            let rec solveInputsRec acc subst = function
                | [] -> Some (List.rev acc)
                | constraints::rest ->
                    // TODO: for ref <: ref constraints we should also accumulate t into another subst
                    match inputCandidates getMock constraints subst |> Seq.tryHead with
                    | Some c -> solveInputsRec (c::acc) subst rest
                    | None -> None
            let rec solveTypesVarsRec subst = function
                // TODO: this should be done in more complex way!
                // 1. clusterize type parameters by equality (i.e. squash loops in type variables subtyping graph)
                // 2. supertypes of mocks can contain mocks (for instance, we have constraint T <: U, where U is assigned a fresh mock)
                | [] ->
                    match solveInputsRec [] subst inputConstraintsList with
                    | Some ts -> resultInputs <- ts; resultSubst <- subst; true
                    | None -> false
                | t::ts ->
                   typeParameterCandidates (getMock None) t subst |> Seq.exists (fun u -> solveTypesVarsRec (PersistentDict.add t u subst) ts)
            if solveTypesVarsRec PersistentDict.empty typeVars then
                TypeSat(resultInputs, decodeTypeSubst resultSubst)
            else TypeUnsat


// ------------------------------------------------- Type solver wrappers -------------------------------------------------

    let private generateConstraints (model : model) (state : state) =
        let typeOfAddress addr =
            if VectorTime.less addr VectorTime.zero then model.state.allocatedTypes.[addr]
            else state.allocatedTypes.[addr]
        let mocks = Dictionary<concreteHeapAddress, ITypeMock>()
        let supertypeConstraints = Dictionary<concreteHeapAddress, List<Type>>()
        let subtypeConstraints = Dictionary<concreteHeapAddress, List<Type>>()
        let notSupertypeConstraints = Dictionary<concreteHeapAddress, List<Type>>()
        let notSubtypeConstraints = Dictionary<concreteHeapAddress, List<Type>>()
        let addresses = HashSet<concreteHeapAddress>()
        model.state.allocatedTypes |> PersistentDict.iter (fun (addr, _) ->
            addresses.Add(addr) |> ignore
            Dict.getValueOrUpdate supertypeConstraints addr (fun () ->
                let list = List<Type>()
                match typeOfAddress addr with
                | ConcreteType t -> list.Add t
                | MockType m ->
                    list.AddRange m.SuperTypes
                    mocks.Add(addr, m)
                list) |> ignore)

        let add dict address typ =
            match model.Eval address with
            | {term = ConcreteHeapAddress addr} when addr <> VectorTime.zero ->
                addresses.Add addr |> ignore
                let list = Dict.getValueOrUpdate dict addr (fun () -> List<_>())
                if not <| list.Contains typ then
                    list.Add typ
            | {term = ConcreteHeapAddress _} -> ()
            | term -> internalfailf "Unexpected address %O in subtyping constraint!" term

        PC.toSeq state.pc |> Seq.iter (term >> function
            | Constant(_, TypeCasting.TypeSubtypeTypeSource _, _) -> __notImplemented__()
            | Constant(_, TypeCasting.RefSubtypeTypeSource(address, typ), _) -> add supertypeConstraints address typ
            | Constant(_, TypeCasting.TypeSubtypeRefSource(typ, address), _) -> add subtypeConstraints address typ
            | Constant(_, TypeCasting.RefSubtypeRefSource _, _) -> __notImplemented__()
            | Negation({term = Constant(_, TypeCasting.TypeSubtypeTypeSource _, _)})-> __notImplemented__()
            | Negation({term = Constant(_, TypeCasting.RefSubtypeTypeSource(address, typ), _)}) -> add notSupertypeConstraints address typ
            | Negation({term = Constant(_, TypeCasting.TypeSubtypeRefSource(typ, address), _)}) -> add notSubtypeConstraints address typ
            | Negation({term = Constant(_, TypeCasting.RefSubtypeRefSource _, _)}) -> __notImplemented__()
            | _ -> ())
        let toList (d : Dictionary<concreteHeapAddress, List<Type>>) addr =
            let l = Dict.tryGetValue d addr null
            if l = null then [] else List.ofSeq l
        let addresses = List.ofSeq addresses
        addresses, addresses
        |> Seq.map (fun addr ->
            {supertypes = toList supertypeConstraints addr
             subtypes = toList subtypeConstraints addr
             notSupertypes = toList notSupertypeConstraints addr
             notSubtypes = toList notSubtypeConstraints addr
             mock = if mocks.ContainsKey addr then Some mocks.[addr] else None})
        |> List.ofSeq

    let solveTypes (model : model) (state : state) =
        let m = CallStack.stackTrace state.stack |> List.last
        let typeGenericParameters = m.DeclaringType.GetGenericArguments()
        let methodGenericParameters = if m.IsConstructor then Array.empty else m.GenericArguments
        let addresses, inputConstraints = generateConstraints model state
        let solverResult = solve (getMock state.typeMocks) inputConstraints (Array.append typeGenericParameters methodGenericParameters |> List.ofArray)
        match solverResult with
        | TypeSat(refsTypes, typeParams) ->
            let refineTypes addr t =
                model.state.allocatedTypes <- PersistentDict.add addr t model.state.allocatedTypes
                match t with
                | ConcreteType t ->
                    if t.IsValueType then
                        let value = makeDefaultValue t
                        model.state.boxedLocations <- PersistentDict.add addr value model.state.boxedLocations
                | _ -> ()
            Seq.iter2 refineTypes addresses refsTypes
            let classParams, methodParams = List.splitAt typeGenericParameters.Length typeParams
            Some(Array.ofList classParams, Array.ofList methodParams)
        | TypeUnsat -> None

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

    let getCallVirtCandidates state (thisAddress : heapAddress) =
        match state.model with
        | Some model ->
            match model.Eval thisAddress with
            | {term = HeapRef({term = ConcreteHeapAddress thisAddress}, _)} ->
                let addresses, inputConstraints = generateConstraints (Option.get state.model) state
                let index = List.findIndex ((=)thisAddress) addresses
                thisAddress, inputCandidates (getMock state.typeMocks) inputConstraints.[index] PersistentDict.empty
            | _ -> __unreachable__()
        | None -> __unreachable__()
