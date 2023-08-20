module VSharp.Core.GenericUtils

open System
open System.Collections.Generic
open VSharp

type parameterSubstitution = pdict<Type, Type>

let substitute (typedef: Type) (subst: parameterSubstitution) =
    let substitute t = PersistentDict.tryFind subst t |> Option.defaultValue t
    Reflection.concretizeType substitute typedef

let rec private getDependencies (acc, depth) t =
    let deps, maxDepth = getConstraintDependencies acc t
    deps, max maxDepth depth

and private getConstraintDependencies acc (typ: Type) =
    if typ.IsGenericParameter then typ :: acc, 0
    elif typ.IsGenericType then
        let deps, maxDepth = Array.fold getDependencies (acc, 0) (typ.GetGenericArguments())
        deps, maxDepth + 1
    else acc, 0

let private collectDependencies parameters =
    let dependencies = Dictionary<Type, Type list>()
    let maxDepths = Dictionary<Type, int>()
    let addParameterDependencies (typ: Type) =
        assert typ.IsGenericParameter
        let parameterDependencies, depth = typ.GetGenericParameterConstraints() |> Array.fold getDependencies ([], 0)
        dependencies.Add(typ, parameterDependencies)
        maxDepths.Add(typ, depth)

    parameters |> Array.iter addParameterDependencies
    dependencies, maxDepths

let private reverseDependencies (dependencies: Dictionary<Type, Type list>) =
    let reversedDeps = Dictionary<Type, Type list>()
    let depsCounts = Dictionary<Type, int>()
    for KeyValue(key, _) in dependencies do
        reversedDeps.Add(key, List.empty)
        depsCounts.Add(key, 0)

    for KeyValue(parameter, paramDependencies) in dependencies do
        for dep in paramDependencies do
            depsCounts[parameter] <- depsCounts[parameter] + 1
            reversedDeps[dep] <- parameter :: reversedDeps[dep]

    reversedDeps, depsCounts

let private refineMaxDepths (dependencies: Dictionary<Type, Type list>) (maxDepths: Dictionary<Type, int>) =
    let newMaxDepths = Dictionary<Type, int>()
    let mutable isCycled = false
    let rec dfs t =
        newMaxDepths.Add(t, -1)
        let mutable maxDepth = 0
        for dep in dependencies[t] do
            let contains, depth = newMaxDepths.TryGetValue dep
            if contains then
                if depth = -1 then
                    isCycled <- true
                else
                    let newDepth = depth + maxDepths[dep]
                    maxDepth <- max maxDepth newDepth
            else
                let depth = dfs dep
                maxDepth <- max maxDepth depth

        newMaxDepths[t] <- maxDepth
        newMaxDepths[t] + maxDepths[t]

    for KeyValue(key, _) in dependencies do
        if newMaxDepths.ContainsKey(key) |> not then
            dfs key |> ignore
    newMaxDepths, isCycled

let layerDependencies (dependencies: Dictionary<Type, Type list>) (counts: Dictionary<Type, int>) =
    let extractZeroes () =
        let zeroes = List<Type>()
        for KeyValue(typ, count) in counts do
            if count = 0 then zeroes.Add typ

        for zero in zeroes do
            counts.Remove(zero) |> ignore
        zeroes

    let layers = List<List<Type>>()
    while counts.Count > 0 do
        let zeroes = extractZeroes()
        layers.Add(zeroes)
        for zero in zeroes do
            for dep in dependencies[zero] do
                counts[dep] <- counts[dep] - 1

    layers |> Seq.map (fun list -> list :> seq<_>)

let private trackIndices (typ: Type) (supertype: Type) =
    let rec helper (typ: Type) (supertype: Type) indices =
        if not typ.IsGenericType then Array.empty
        elif not supertype.IsGenericType then Array.replicate (typ.GetGenericArguments().Length) List.empty
        elif typ.GetGenericTypeDefinition() = supertype.GetGenericTypeDefinition() then indices
        else
            let bt = typ.BaseType
            let btArgs = TypeUtils.getGenericArgs bt

            let mapping = Dictionary<Type, List<int>>()
            let add i t =
                let contains, indices = mapping.TryGetValue t
                if contains then indices.Add(i)
                else mapping.Add(t, List([i]))
            Array.iteri add btArgs

            let get t =
                let contains, indices = mapping.TryGetValue t
                if contains then List.ofSeq indices
                else List.empty
            let track = Array.map get btArgs |> helper bt supertype

            let typArgs = TypeUtils.getGenericArgs typ
            let traceback = List.collect (fun i -> get typArgs[i] |> List.collect (fun i -> track[i]) |> List.distinct)
            Array.map traceback indices

    let genericsCount = (TypeUtils.getGenericArgs typ).Length
    Array.init genericsCount List.singleton |> helper typ supertype

let makeLayers parameters =
    let dependencies, maxDepths = collectDependencies parameters
    for KeyValue(param, deps) in dependencies do
        dependencies[param] <- List.distinct deps
    let revDeps, counts = reverseDependencies dependencies
    let maxDepths, isCycled = refineMaxDepths revDeps maxDepths
    let layers =
        if isCycled then Seq.empty
        else layerDependencies revDeps counts
    layers, maxDepths, isCycled

let splitByDependence (parameters: Type array) =
    let dependencies, _ = collectDependencies parameters
    let _, counts = reverseDependencies dependencies
    parameters |> Array.partition (fun p -> counts[p] = 0)

let private update (parameters: Type array) (res: Dictionary<_,_>) fromSupertypes fromSubtypes fromInterfaces =
    for i = 0 to parameters.Length - 1 do
        if parameters[i].IsGenericParameter then
            let item (arr: _ array) = arr[i]
            let fromSupertypes = fromSupertypes |> List.map item
            let fromSubtypes = fromSubtypes |> List.map item
            let toSupertypes, toSubtypes = fromInterfaces |> List.map item |> List.unzip
            let fromClasses = fromSubtypes @ fromSupertypes |> List.concat
            let toSupertypes, toSubtypes = List.concat toSupertypes, List.concat toSubtypes

            let supertypes, subtypes = res[parameters[i]]
            res[parameters[i]] <- fromClasses @ toSupertypes @ supertypes, fromClasses @ toSubtypes @ subtypes

let rec propagateInterface parameters (interfaces: Type array) (supertype : Type) =
    let supertypeDef = TypeUtils.getTypeDef supertype
    let supertypeDefArgs = TypeUtils.getGenericArgs supertypeDef
    let supertypeArgs = TypeUtils.getGenericArgs supertype
    let index = interfaces |> Array.tryFindIndex (fun t -> t.GetGenericTypeDefinition() = supertypeDef)
    match index with
    | Some index ->
        let interfaceParams = interfaces[index].GetGenericArguments()
        let constraints = Dictionary()
        for t in parameters do
            constraints.Add(t, (List.empty, List.empty))

        let updateWithVariance i (param: Type) =
            let typ = supertypeArgs[i]
            let contains, value = constraints.TryGetValue param
            if contains then
                let supertypes, subtypes = value
                constraints[param] <-
                    match supertypeDefArgs[i] with
                    | TypeUtils.Invariant -> typ :: supertypes, typ :: subtypes
                    | TypeUtils.Covariant -> typ :: supertypes, subtypes
                    | TypeUtils.Contravariant -> supertypes, typ :: subtypes
                Some ()
            elif param.IsGenericType then
                option {
                    let typedef = param.GetGenericTypeDefinition()
                    let paramParams = param.GetGenericArguments()
                    let supertypesDefs = TypeUtils.getSupertypes param |> List.map TypeUtils.getTypeDef
                    let genericArgs = typedef.GetGenericArguments()
                    let interfaces = param.GetInterfaces()
                    let! fromInterfaces, fromSupertypes, fromSubtypes =
                        propagate typedef supertypesDefs genericArgs interfaces [typ] List.empty
                    update paramParams constraints fromSupertypes fromSubtypes fromInterfaces
                    return! Some ()
                }
            else
                let isSuitable =
                    match supertypeDefArgs[i] with
                    | TypeUtils.Invariant -> param = typ
                    | TypeUtils.Covariant -> param.IsAssignableTo typ
                    | TypeUtils.Contravariant -> param.IsAssignableFrom typ
                if isSuitable then Some ()
                else None

        let allPropagated = Array.mapi updateWithVariance interfaceParams |> Array.forall Option.isSome
        if allPropagated then
            Array.map (fun p -> constraints[p]) parameters |> Some
        else None
    | None -> None

and propagateSupertype typedef supertypes (supertype: Type) =
    let supertypeDef = TypeUtils.getTypeDef supertype
    let supertypeArgs = TypeUtils.getGenericArgs supertype
    if List.contains supertypeDef supertypes then
        trackIndices typedef supertype
        |> Array.map (List.map (fun i -> supertypeArgs[i]))
        |> Some
    else None

and propagateSubtype typedef (parameters: _ array) subtype =
    let subtypeDef = TypeUtils.getTypeDef subtype
    let subtypeArgs = TypeUtils.getGenericArgs subtype
    if TypeUtils.getSupertypes subtype |> List.map TypeUtils.getTypeDef |> List.contains typedef then
        let subtypes = Array.init parameters.Length (fun _ -> List<Type>())
        let add i = List.iter (fun j -> subtypes[j].Add subtypeArgs[i])
        trackIndices subtypeDef typedef |> Array.iteri add
        Array.map List.ofSeq subtypes |> Some
    else None

and propagate typedef supertypesDefs parameters interfaces (supertypes: Type list) (subtypes: Type list) =
    option {
        let sptInterfaces, supertypes = supertypes |> List.partition (fun t -> t.IsInterface)
        let sbtInterfaces, subtypes = subtypes |> List.partition (fun t -> t.IsInterface)

        if typedef.IsInterface then
            let! fromSupertypes = sptInterfaces |> List.map (propagateSupertype typedef supertypesDefs) |> List.sequenceOption id
            let! fromSubtypes = sbtInterfaces |> List.map (propagateSubtype typedef parameters) |> List.sequenceOption id
            if List.isEmpty supertypes |> not then return! None
            else return [], fromSupertypes, fromSubtypes
        else
            let! fromInterfaces = sptInterfaces |> List.map (propagateInterface parameters interfaces) |> List.sequenceOption id
            let! fromSupertypes = supertypes |> List.map (propagateSupertype typedef supertypesDefs) |> List.sequenceOption id
            let! fromSubtypes = subtypes |> List.map (propagateSubtype typedef parameters) |> List.sequenceOption id
            if List.isEmpty sbtInterfaces |> not then return! None
            else return fromInterfaces, fromSupertypes, fromSubtypes
    }

let propagateNotSupertype typedef supertypes (notSupertype: Type) =
    assert(notSupertype.GetGenericArguments().Length = 1)
    propagateSupertype typedef supertypes notSupertype
    |> Option.defaultValue (typedef.GetGenericArguments() |> Array.map (fun _ -> []))

let propagateNotSubtype typedef (parameters: _ array) (notSubtype: Type) =
    assert(notSubtype.GetGenericArguments().Length = 1)
    propagateSubtype typedef parameters notSubtype
    |> Option.defaultValue (typedef.GetGenericArguments() |> Array.map (fun _ -> []))

let propagateNotConstraints typedef supertypesDefs parameters interfaces (notSupertypes: Type list) (notSubtypes: Type list) =
    let nSptInterfaces, nSupertypes = notSupertypes |> List.partition (fun t -> t.IsInterface)
    let _, nSubtypes = notSubtypes |> List.partition (fun t -> t.IsInterface)

    let fromNotSupertypes = nSupertypes |> List.map (propagateNotSupertype typedef supertypesDefs)
    let fromNotSubtypes = nSubtypes |> List.map (propagateNotSubtype typedef parameters)
    let fromNotInterfaces =
        nSptInterfaces
        |> List.map (
            propagateInterface parameters interfaces
            >> Option.defaultValue (typedef.GetGenericArguments() |> Array.map(fun _ -> [], [])))
    fromNotInterfaces, fromNotSupertypes, fromNotSubtypes
