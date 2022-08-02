namespace VSharp.Core

open System
open System.Reflection
open System.Collections.Generic
open FSharpx.Collections
open VSharp
open VSharp.Core

// ------------------------------------------------- Type mocks -------------------------------------------------

type functionResultConstantSource = {mock : MethodMock; callIndex : int; args : term list}
with
    interface IMemoryAccessConstantSource with
        override x.TypeOfLocation = x.mock.Method.ReturnType
        override x.Compose _ = Constant (toString x) x x.mock.Method.ReturnType
        override x.SubTerms = []
        override x.Time = VectorTime.zero
    override x.ToString() =
        let args = x.args |> List.map toString |> join ", "
        $"{x.mock.Method.Name}({args})"

and MethodMock(method : IMethod) =
    let mutable callIndex = 0
    let callResults = ResizeArray<term>()

    member x.Method : IMethod = method
    interface IMethodMock with
        override x.BaseMethod =
            match method.MethodBase with
            | :? MethodInfo as mi -> mi
            | _ -> __notImplemented__()

        override x.Call args =
            let returnType = method.ReturnType
            if returnType = typeof<Void> then None
            else
                let src : functionResultConstantSource = {mock = x; callIndex = callIndex; args = args}
                let result = Constant (toString src) src returnType
                callIndex <- callIndex + 1
                callResults.Add result
                Some result

        override x.GetImplementationClauses() = callResults.ToArray()

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
            Dict.getValueOrUpdate methodMocks m (fun () -> MethodMock(m)) :> IMethodMock
        override x.MethodMocks with get() = methodMocks.Values |> Seq.cast<_>
        override x.WithSuperTypes supertypes' =
            TypeMock(supertypes', Dictionary<_,_>(methodMocks))
    override x.ToString() = (x :> ITypeMock).Name

// ------------------------------------------------- Type solver core -------------------------------------------------

type typeConstraints = { supertypes : Type list; subtypes : Type list; notSubtypes : Type list; notSupertypes : Type list; mock : ITypeMock option}

type typeSolvingResult =
    | TypeSat of symbolicType list * symbolicType list
    | TypeUnsat

module TypeSolver =
    type private 'a searchResult =
        | Found of 'a
        | NotFound
        | NotExists

    type private typeSearchResult = Type list searchResult

    type private substitution = pdict<Type, symbolicType>

    let private typeVarsCache = Dictionary<Type * substitution, substitution searchResult>()
    let private inputsCache = Dictionary<typeConstraints * substitution, typeSearchResult>()

    let private findNonAbstractSupertype predicate (typ : Type) =
        let rec findRec predicate (typ : Type) sure =
            if typ = null then
                if sure then NotExists else NotFound
            else
                match if typ.IsAbstract then NotExists else predicate typ with
                | Found ts -> Found ts
                | NotExists -> findRec predicate typ.BaseType sure
                | NotFound -> findRec predicate typ.BaseType false
        findRec predicate typ true

    let private findType (supertypes : Type list) (mock : ITypeMock option) searchRest (assemblies : Assembly seq) =
        let mutable sure = true
        let mutable result = NotExists
        let predicate t =
            match searchRest (ConcreteType t) with
            | Found res -> result <- Found res; true
            | NotExists -> false
            | NotFound -> sure <- false; false
        if Seq.exists predicate supertypes then result
        else
            let assemblies =
                match supertypes |> Seq.tryFind (fun u -> u.IsNotPublic) with
                | Some u -> [u.Assembly] :> _ seq
                | None -> sure <- false; assemblies
            let found = assemblies |> Seq.exists (fun assembly -> assembly.GetExportedTypes() |> Seq.exists predicate)
            if found then result
            elif sure then NotExists
            else
                let t =
                    match mock with
                    | Some mock -> mock.WithSuperTypes supertypes
                    | None -> TypeMock(supertypes)
                    |> MockType
                searchRest t

    let private findNonAbstractType supertypes mock searchRest (assemblies : Assembly seq) =
        findType supertypes mock (function
            | ConcreteType t when t.IsAbstract -> NotExists
            | t -> searchRest t) assemblies

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

    let private satisfyInput constraints subst validate =
//        match PersistentDict.tryFind inputsCache (constraints, subst) with
//        | Some t when validate t -> Found t
//        | _ ->
            let validateConcrete typ =
                if satisfiesConstraints constraints subst typ then
                    validate (ConcreteType typ)
                else NotExists
            let validate = function
                | ConcreteType t -> validateConcrete t
                | t -> validate t
            match constraints.subtypes with
            | [] -> findNonAbstractType constraints.supertypes constraints.mock validate  (AssemblyManager.assemblies())
            | t :: _ -> findNonAbstractSupertype validateConcrete t//)

    let private satisfyTypeParameter parameter subst validate =
//        Dict.getValueOrUpdate typeVarsCache (parameter, subst) (fun () ->
            let validate = function
                | ConcreteType typ when not <| satisfiesTypeParameterConstraints parameter subst typ -> NotExists
                | typ -> validate typ
            let supertypes = parameter.GetGenericParameterConstraints() |> Array.map (substitute subst) |> List.ofArray
            findType supertypes None validate (AssemblyManager.assemblies())//)

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

    let private solve (inputConstraintsList : typeConstraints list) (typeParameters : Type list) =
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
            let mutable inputs = []
            let mutable typeVariablesUnknown = true
            let rec solveInputsRec acc subst = function
                | [] -> Found (List.rev acc)
                | constraints::rest ->
                    satisfyInput constraints subst (fun t -> solveInputsRec (t::acc) subst rest) // TODO: for ref <: ref constraints we should also accumulate t into another subst
            let rec solveTypesVarsRec subst = function
                // TODO: this should be done in more complex way!
                // 1. clusterize type parameters by equality (i.e. squash loops in type variables subtyping graph)
                // 2. supertypes of mocks can contain mocks (for instance, we have constraint T <: U, where U is assigned a fresh mock)
                | [] ->
                    typeVariablesUnknown <- false
                    match solveInputsRec [] subst inputConstraintsList with
                    | Found ts -> inputs <- ts; Found subst
                    | NotFound -> NotFound
                    | NotExists -> NotExists
                | t::ts ->
                   satisfyTypeParameter t subst (fun u -> solveTypesVarsRec (PersistentDict.add t u subst) ts)
            match solveTypesVarsRec PersistentDict.empty typeVars with
            | Found subst -> TypeSat(inputs, decodeTypeSubst subst)
            | NotExists -> TypeUnsat
            | NotFound -> __unreachable__()


// ------------------------------------------------- Type solver wrappers -------------------------------------------------

    let solveTypes (model : model) (state : state) =
        let m = CallStack.stackTrace state.stack |> List.last
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
        let inputConstraints =
            addresses
            |> Seq.map (fun addr ->
                {supertypes = toList supertypeConstraints addr
                 subtypes = toList subtypeConstraints addr
                 notSupertypes = toList notSupertypeConstraints addr
                 notSubtypes = toList notSubtypeConstraints addr
                 mock = if mocks.ContainsKey addr then Some mocks.[addr] else None})
            |> List.ofSeq
        let typeGenericParameters = m.DeclaringType.GetGenericArguments()
        let methodGenericParameters = if m.IsConstructor then Array.empty else m.GenericArguments
        let solverResult = solve inputConstraints (Array.append typeGenericParameters methodGenericParameters |> List.ofArray)
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

    let reset() =
        typeVarsCache.Clear();
        inputsCache.Clear()
