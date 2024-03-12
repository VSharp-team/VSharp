namespace VSharp.Core

open System
open System.Collections.Generic
open System.Reflection
open VSharp
open VSharp.Core

type ITypeMock =
    abstract Name : string
    abstract SuperTypes : Type seq
    abstract IsValueType : bool
    abstract Copy : unit -> ITypeMock

type private EmptyTypeMock() =
    let mockIsNotReady () = internalfail "Empty mock"
    interface ITypeMock with
        override x.Name = mockIsNotReady()
        override x.SuperTypes = mockIsNotReady()
        override x.IsValueType = mockIsNotReady()
        override x.Copy() = mockIsNotReady()

type symbolicType =
    | ConcreteType of Type
    | MockType of ITypeMock

type private substitution = pdict<Type, symbolicType>

type specialConstraints = {
    isReferenceType : bool
    isNotNullableValueType : bool
    hasDefaultConstructor : bool
}
with
    static member FromParameter (parameter : Type) =
        let (&&&) = Microsoft.FSharp.Core.Operators.(&&&)
        let specialConstraints = parameter.GenericParameterAttributes &&& GenericParameterAttributes.SpecialConstraintMask
        let isReferenceType = specialConstraints &&& GenericParameterAttributes.ReferenceTypeConstraint = GenericParameterAttributes.ReferenceTypeConstraint
        let isNotNullableValueType = specialConstraints &&& GenericParameterAttributes.NotNullableValueTypeConstraint = GenericParameterAttributes.NotNullableValueTypeConstraint
        let hasDefaultConstructor = specialConstraints &&& GenericParameterAttributes.DefaultConstructorConstraint = GenericParameterAttributes.DefaultConstructorConstraint
        {
            isReferenceType = isReferenceType
            isNotNullableValueType = isNotNullableValueType
            hasDefaultConstructor = hasDefaultConstructor
        }

type typeSpecialConstraintsInfo = {
    isByRefLike : bool
    isReferenceType : bool
    isNotNullableValueType : bool
    hasDefaultConstructor : bool
}
with
    static member FromType (t : Type) = {
        isByRefLike = t.IsByRefLike
        isReferenceType = not t.IsValueType
        isNotNullableValueType = t.IsValueType && Nullable.GetUnderlyingType t = null
        hasDefaultConstructor = t.IsValueType || not t.IsAbstract && t.GetConstructor(Type.EmptyTypes) <> null
    }

module private CommonUtils =

    let satisfiesSpecialConstraints (parameter : Type) (info : typeSpecialConstraintsInfo) =
        let sc = specialConstraints.FromParameter parameter
        // Byref-like structures can not be generic argument
        (not info.isByRefLike)
        && (not sc.isReferenceType || info.isReferenceType)
        && (not sc.isNotNullableValueType || info.isNotNullableValueType)
        && (not sc.hasDefaultConstructor || info.hasDefaultConstructor)

    // 'typeParameters' must contain either not generic type or generic parameter
    let decodeTypeSubst (subst : substitution) typeParameters =
        let getSubst (typ : Type) =
            if typ.IsGenericParameter then PersistentDict.find subst typ
            else
                assert(not typ.ContainsGenericParameters)
                ConcreteType typ
        Array.map getSubst typeParameters

    let isAssignableTo (t : Type) (u : Type) =
        if t.ContainsGenericParameters || u.ContainsGenericParameters then
            if not t.IsInterface && u.IsInterface then
                TypeUtils.typeImplementsInterface t u
            else
                TypeUtils.getSupertypes t
                |> List.map TypeUtils.getTypeDef
                |> List.contains (TypeUtils.getTypeDef u)
        else t.IsAssignableTo(u)

    let potentiallyIsAssignableTo (t : Type) (u : Type) =
        t.IsGenericParameter || u.IsGenericParameter || isAssignableTo t u

    let isAssignableFrom t u = isAssignableTo u t
    let potentiallyIsAssignableFrom t u = potentiallyIsAssignableTo u t

    let potentiallyEquals (t : Type) (u : Type) =
        if not t.ContainsGenericParameters && not u.ContainsGenericParameters then t = u
        elif t.IsGenericParameter || u.IsGenericParameter then true
        else TypeUtils.getTypeDef t = TypeUtils.getTypeDef u

    let isValueType (t : Type) =
        t.IsValueType
        || t = typeof<ValueType>
        || t = typeof<Enum>

// TODO: use set instead of list? #type
type typeConstraints =
    {
        mutable equal : Type list
        mutable supertypes : Type list
        mutable subtypes : Type list
        mutable notEqual : Type list
        mutable notSubtypes : Type list
        mutable notSupertypes : Type list
    }
with
    static member Empty with get() =
        let empty = List.empty
        {
            equal = empty
            subtypes = empty
            supertypes = empty
            notEqual = empty
            notSubtypes = empty
            notSupertypes = empty
        }

    static member FromSuperTypes (superTypes : Type list) =
        let empty = List.empty
        let superTypes = List.filter (fun t -> t <> typeof<obj>) superTypes |> List.distinct
        {
            equal = empty
            subtypes = empty
            supertypes = superTypes
            notEqual = empty
            notSubtypes = empty
            notSupertypes = empty
        }

    static member FromEqual (equal : Type list) =
        let empty = List.empty
        {
            equal = equal
            subtypes = empty
            supertypes = empty
            notEqual = empty
            notSubtypes = empty
            notSupertypes = empty
        }

    static member Create equal supertypes subtypes notEqual notSupertypes notSubtypes =
        let equal = List.distinct equal
        let supertypes = List.filter (fun t -> t <> typeof<obj>) supertypes |> List.distinct
        let subtypes = List.distinct subtypes
        let notSupertypes = List.distinct notSupertypes
        let notSubtypes = List.distinct notSubtypes
        {
            equal = equal
            subtypes = subtypes
            supertypes = supertypes
            notEqual = notEqual
            notSubtypes = notSubtypes
            notSupertypes = notSupertypes
        }

    member x.Merge(other : typeConstraints) : bool =
        let mutable changed = false
        if x.equal <> other.equal then
            changed <- true
            x.equal <- x.equal @ other.equal |> List.distinct
        if x.supertypes <> other.supertypes then
            changed <- true
            x.supertypes <- x.supertypes @ other.supertypes |> List.distinct
        if x.subtypes <> other.subtypes then
            changed <- true
            x.subtypes <- x.subtypes @ other.subtypes |> List.distinct
        if x.notEqual <> other.notEqual then
            changed <- true
            x.notEqual <- x.notEqual @ other.notEqual |> List.distinct
        if x.notSubtypes <> other.notSubtypes then
            changed <- true
            x.notSubtypes <- x.notSubtypes @ other.notSubtypes |> List.distinct
        if x.notSupertypes <> other.notSupertypes then
            changed <- true
            x.notSupertypes <- x.notSupertypes @ other.notSupertypes |> List.distinct
        changed

    member private x.EqualContradicts t =
        // X = t and X = u and t <> u
        List.exists (CommonUtils.potentiallyEquals t >> not) x.equal
        || // X = t and X <: u and t </: u
        List.exists (CommonUtils.potentiallyIsAssignableTo t >> not) x.supertypes
        || // X = t and u <: X and u </: t
        List.exists (fun u -> CommonUtils.potentiallyIsAssignableTo u t |> not) x.subtypes
        || // X = t and X </: u and t <: u
        List.exists (CommonUtils.isAssignableTo t) x.notSupertypes
        || // X = t and u </: X and u <: t
        List.exists (fun u -> CommonUtils.isAssignableTo u t) x.notSubtypes

    member private x.NotEqualContradicts t =
        // X /= t and X = u and t = u
        List.exists (CommonUtils.potentiallyEquals t) x.equal
        || // X /= t and X <: u and t = u when u is sealed
        x.supertypes |> List.exists (fun u -> t = u && u.IsSealed)

    member private x.SupertypeContradicts t =
        // X <: u and u <: t and X </: t
        x.notSupertypes |> List.exists (CommonUtils.isAssignableTo t)
        || // u </: X and X <: u when u is sealed
        t.IsSealed && x.notSubtypes |> List.contains t

    member x.IsContradicting() =
        let nonComparable (t : Type) (u : Type) =
            u.IsClass && t.IsClass && not (CommonUtils.potentiallyIsAssignableTo u t) && not (CommonUtils.potentiallyIsAssignableFrom u t)
            || t.IsSealed && u.IsInterface && not (CommonUtils.potentiallyIsAssignableTo t u)
        // checking 'equal' constraints
        x.equal |> List.exists x.EqualContradicts
        || // checking 'notEqual' constraints
        x.notEqual |> List.exists x.NotEqualContradicts
        || // checking 'supertypes' constraints
        x.supertypes |> List.exists x.SupertypeContradicts
        || // u <: X and t <: u and t </: X
        x.subtypes |> List.exists (fun u -> x.notSubtypes |> List.exists (CommonUtils.isAssignableFrom u))
        || // u <: X and X <: t and u </: t
        x.subtypes |> List.exists (fun u -> x.supertypes |> List.exists (CommonUtils.potentiallyIsAssignableTo u >> not))
        || // No multiple inheritance -- X <: u and X <: t and u </: t and t </: u and t, u are classes
        x.supertypes |> List.exists (fun u -> x.supertypes |> List.exists (nonComparable u))

    member x.AddSuperType(superType : Type) =
        if superType <> typeof<obj> then
            x.supertypes <- superType :: x.supertypes |> List.distinct

    member x.AddSubType(subType : Type) =
        x.subtypes <- subType :: x.subtypes |> List.distinct

    member x.AddEqual(equal : Type) =
        x.equal <- equal :: x.equal

    member x.AddNotSupertype(notSupertype : Type) =
        x.notSupertypes <- notSupertype :: x.notSupertypes |> List.distinct

    member x.IsSuitable substitute (candidate : Type) =
        // TODO: need to find subst to generic parameters satisfying constraints
        x.equal |> List.forall (fun t -> candidate = (substitute t)) &&
        x.notEqual |> List.forall (fun t -> candidate <> (substitute t)) &&
        x.subtypes |> List.forall (substitute >> candidate.IsAssignableFrom) &&
        x.supertypes |> List.forall (substitute >> candidate.IsAssignableTo) &&
        x.notSubtypes |> List.forall (substitute >> candidate.IsAssignableFrom >> not) &&
        x.notSupertypes |> List.forall (substitute >> candidate.IsAssignableTo >> not)

    member x.IsValid() = List.length x.equal <= 1

    member x.Negate() =
        let equal = x.equal
        let subtypes = x.subtypes
        let supertypes = x.supertypes
        x.equal <- x.notEqual
        x.subtypes <- x.notSubtypes
        x.supertypes <- x.notSupertypes
        x.notEqual <- equal
        x.notSubtypes <- subtypes
        x.notSupertypes <- supertypes

    member x.Copy() =
        {
            equal = x.equal
            supertypes = x.supertypes
            subtypes = x.subtypes
            notEqual = x.notEqual
            notSubtypes = x.notSubtypes
            notSupertypes = x.notSupertypes
        }

    member x.Filter shouldBeTaken =
            x.equal <- List.filter shouldBeTaken x.equal
            x.supertypes <- List.filter shouldBeTaken x.supertypes
            x.subtypes <- List.filter shouldBeTaken x.subtypes
            x.notEqual <- List.filter shouldBeTaken x.notEqual
            x.notSubtypes <- List.filter shouldBeTaken x.notSubtypes
            x.notSupertypes <- List.filter shouldBeTaken x.notSupertypes

type typeParameterConstraints(parameter : Type, constraints : typeConstraints) =
    do assert parameter.IsGenericParameter
    let specialConstraints = specialConstraints.FromParameter parameter

    let potentiallyIsValueType (t : Type) =
        t.IsGenericParameter || CommonUtils.isValueType t

    let potentiallyIsValueTypeOrInterface (t : Type) =
        t.IsGenericParameter || CommonUtils.isValueType t || t.IsInterface

    member x.Merge(other : typeConstraints) =
        constraints.Merge other

    member x.IsContradicting() =
        let isNotNullableValueTypeContradiction =
            specialConstraints.isNotNullableValueType
            && (constraints.supertypes |> List.exists (potentiallyIsValueTypeOrInterface >> not)
                || constraints.subtypes |> List.exists (potentiallyIsValueType >> not))
        let isReferenceTypeContradiction =
            specialConstraints.isReferenceType
            && constraints.supertypes |> List.exists CommonUtils.isValueType

        isNotNullableValueTypeContradiction
        || isReferenceTypeContradiction
        || constraints.IsContradicting()

    member x.Constraints with get() = constraints

    member x.Copy() =
        let constraints = constraints.Copy()
        typeParameterConstraints(parameter, constraints)

type typesConstraints private (newAddresses, constraints) =

    new () =
        let newAddresses = HashSet<term>()
        let allConstraints = Dictionary<term, typeConstraints>()
        typesConstraints(newAddresses, allConstraints)

    member x.Copy() =
        let copiedNewAddresses = HashSet<term>(newAddresses)
        let copiedConstraints = Dictionary<term, typeConstraints>()
        for KeyValue(address, constraints) in constraints do
            copiedConstraints.Add(address, constraints.Copy())
        typesConstraints(copiedNewAddresses, copiedConstraints)

    member private x.AddNewAddress address =
        newAddresses.Add address |> ignore

    member x.ClearNewAddresses() =
        newAddresses.Clear()

    member x.NewAddresses with get() = newAddresses

    member x.Add (address : term) (typeConstraint : typeConstraints) =
        let current = ref typeConstraints.Empty
        if constraints.TryGetValue(address, current) then
            let changed = current.Value.Merge typeConstraint
            if changed then x.AddNewAddress address
        else
            constraints.Add(address, typeConstraint)
            x.AddNewAddress address

    member x.AddSuperType address superType =
        let typeConstraint = List.singleton superType |> typeConstraints.FromSuperTypes
        x.Add address typeConstraint

    member x.CheckInequality() =
        let mutable isValid = true
        let unequal = HashSet<term * term>()
        for entry1 in constraints do
            if isValid then
                let address1 = entry1.Key
                let typeConstraints1 = entry1.Value
                for entry2 in constraints do
                    let address2 = entry2.Key
                    let typeConstraints2 = entry2.Value
                    let typeConstraints = typeConstraints1.Copy()
                    let different = address1 <> address2
                    if different then
                        typeConstraints.Merge typeConstraints2 |> ignore
                    if typeConstraints.IsContradicting() then
                        if different then
                            if unequal.Contains(address2, address1) |> not then
                                unequal.Add(address1, address2) |> ignore
                        else isValid <- false
        if isValid then Some unequal
        else None

    member x.IsValid() =
        let mutable isValid = true
        for KeyValue(_, constraints) in constraints do
            if isValid then
                isValid <- constraints.IsValid()
        isValid

    interface System.Collections.IEnumerable with
        member this.GetEnumerator() =
          upcast constraints.GetEnumerator()

    interface IEnumerable<KeyValuePair<term, typeConstraints>> with
        override this.GetEnumerator() =
          constraints.GetEnumerator()

    member x.Item(address : term) =
        constraints[address].Copy()

    member x.Count with get() = constraints.Count

module internal GroundUtils =

    let substitute (subst : substitution) (t : Type) =
        let substFunction t =
            match PersistentDict.tryFind subst t with
            | Some (ConcreteType t) -> t
            | _ -> t
        Reflection.concretizeType substFunction t

    let satisfiesTypeParameterConstraints (parameter : Type) subst (t : Type) =
        (not t.ContainsGenericParameters)
        && typeSpecialConstraintsInfo.FromType t |> CommonUtils.satisfiesSpecialConstraints parameter
        && (parameter.GetGenericParameterConstraints() |> Array.forall (substitute subst >> t.IsAssignableTo))

    let satisfiesConstraints (constraints : typeConstraints) subst (candidate : Type) =
        constraints.IsSuitable (substitute subst) candidate

module private GenericUtils =

    type parameterSubstitution = pdict<Type, Type>

    let substitute (typedef : Type) (subst : parameterSubstitution) =
        let substitute t = PersistentDict.tryFind subst t |> Option.defaultValue t
        Reflection.concretizeType substitute typedef

    let rec private getDependencies (acc, depth) (t : Type) =
        let deps, maxDepth =
            if t.IsGenericParameter then t :: acc, 0
            elif t.IsGenericType then
                let deps, maxDepth = Array.fold getDependencies (acc, 0) (t.GetGenericArguments())
                deps, maxDepth + 1
            else acc, 0
        deps, max maxDepth depth

    let private collectDependencies parameters =
        let dependencies = Dictionary<Type, Type list>()
        let maxDepths = Dictionary<Type, int>()
        let addParameterDependencies (typ : Type) =
            assert typ.IsGenericParameter
            let parameterDependencies, depth = typ.GetGenericParameterConstraints() |> Array.fold getDependencies ([], 0)
            dependencies.Add(typ, parameterDependencies)
            maxDepths.Add(typ, depth)

        parameters |> Array.iter addParameterDependencies
        dependencies, maxDepths

    let private reverseDependencies (dependencies : Dictionary<Type, Type list>) =
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

    let private refineMaxDepths (dependencies : Dictionary<Type, Type list>) (maxDepths : Dictionary<Type, int>) =
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

    let private layerDependencies (dependencies: Dictionary<Type, Type list>) (counts: Dictionary<Type, int>) =
        let layers = List<List<Type>>()
        while counts.Count > 0 do
            let zeroes =
                let zeroes = List<Type>()
                for KeyValue(typ, count) in counts do
                    if count = 0 then zeroes.Add typ
                for zero in zeroes do
                    counts.Remove(zero) |> ignore
                zeroes
            layers.Add(zeroes)
            for zero in zeroes do
                for dep in dependencies[zero] do
                    counts[dep] <- counts[dep] - 1

        layers |> Seq.map (fun list -> list :> seq<_>)

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

type parameterSubstitutions private (
    parameters,
    layers,
    maxDepths: Dictionary<Type, int>,
    parameterConstraints: Dictionary<Type, typeParameterConstraints>,
    depth,
    getCandidates,
    makeGenericCandidate: Type -> int -> genericCandidate option,
    childDepth) =

    let satisfiesConstraints subst (constraints : typeConstraints) (parameter : Type) (candidate : candidate) =
        let substitute subst t = GenericUtils.substitute t subst
        let constraints = constraints.Copy()
        constraints.Filter (TypeUtils.containsGenericParameters >> not)
        parameter.GetGenericParameterConstraints()
        |> Array.map (substitute subst)
        |> List.ofArray
        |> typeConstraints.FromSuperTypes
        |> constraints.Merge
        |> ignore

        if CommonUtils.satisfiesSpecialConstraints parameter candidate.SpecialConstraintsInfo then
            match candidate with
            | Candidate candidate as c ->
                let satisfies =
                    (not candidate.ContainsGenericParameters)
                    && constraints.IsSuitable (substitute subst) candidate
                if satisfies then Some c else None
            | GenericCandidate genericCandidate ->
                genericCandidate.AddConstraints constraints |> Option.map GenericCandidate
            | ArrayCandidate arrayCandidate ->
                arrayCandidate.AddConstraints constraints |> Option.map ArrayCandidate
        else None

    let candidate2types candidate =
        match candidate with
        | Candidate t -> Seq.singleton t
        | GenericCandidate genericCandidate -> genericCandidate.Types
        | ArrayCandidate arrayCandidate -> arrayCandidate.Types

    let unrollCandidateSubstitutions (substs: pdict<Type, candidate> seq) =
        seq {
            for subst in substs do
                let parameters, tss =
                    PersistentDict.map id candidate2types subst
                    |> PersistentDict.toSeq
                    |> List.ofSeq
                    |> List.unzip
                for ts in List.cartesian tss do
                    yield List.zip parameters ts |> PersistentDict.ofSeq
        }

    let updateConstraints parameterConstraints =
        parameterSubstitutions(
            parameters,
            layers,
            maxDepths,
            parameterConstraints,
            depth,
            getCandidates,
            makeGenericCandidate,
            childDepth)

    let makeGeneric param typ =
        let depth = childDepth param maxDepths depth
        if depth <= 0 then None
        else makeGenericCandidate typ depth

    let processLayer (substs : GenericUtils.parameterSubstitution seq) layer =
        let paramCandidates subst param =
            let candidates : candidates = makeGeneric param |> getCandidates
            let filtered : candidates =
                candidates.Filter (satisfiesConstraints subst parameterConstraints[param].Constraints param) (fun _ -> None)
            filtered.ConcreteTypes
        seq {
            for subst in substs do
                yield!
                    layer
                    |> List.ofSeq
                    |> List.map (paramCandidates subst)
                    |> List.cartesian
                    |> Seq.map (Seq.zip layer >> PersistentDict.ofSeq)
                    |> unrollCandidateSubstitutions
                    |> Seq.map (PersistentDict.fold (fun dict k v -> PersistentDict.add k v dict) subst)
        }

    let args : GenericUtils.parameterSubstitution seq =
        Seq.fold processLayer (Seq.singleton PersistentDict.empty) layers

    static member TryCreate parameterTypes depth getCandidates makeGenericCandidate childDepth =
        let layers, maxDepths, isCycled = GenericUtils.makeLayers parameterTypes
        if isCycled || depth <= 0 then None
        else
            let parameterConstraints = Dictionary<Type, typeParameterConstraints>()
            for t in parameterTypes do
                let constraints = t.GetGenericParameterConstraints() |> Array.toList |> typeConstraints.FromSuperTypes
                parameterConstraints.Add(t, typeParameterConstraints(t, constraints))
            parameterSubstitutions(
                parameterTypes,
                layers,
                maxDepths,
                parameterConstraints,
                depth,
                getCandidates,
                makeGenericCandidate,
                childDepth)
            |> Some

    member x.AddConstraints (newConstraints: Dictionary<Type, typeConstraints>) =
        let newParameterConstraints = Dictionary<Type, typeParameterConstraints>()
        let mutable isContradicting = false
        for KeyValue(param, constraints) in parameterConstraints do
            let constraints = constraints.Copy()
            constraints.Merge newConstraints[param] |> ignore
            if constraints.IsContradicting() then
                isContradicting <- true
            newParameterConstraints.Add(param, constraints)

        if isContradicting then None
        else updateConstraints newParameterConstraints |> Some

    member val Substitutions = args

and genericCandidate private (
    typedef: Type,
    depth,
    parameterSubstitutions: parameterSubstitutions,
    selfConstraints) =

    do assert(depth >= 0 && typedef.IsGenericTypeDefinition)

    let parameters = typedef.GetGenericArguments()
    let interfaces = typedef.GetInterfaces()
    let interfacesDefs = interfaces |> Array.map TypeUtils.getTypeDef
    let genericInterfaces = interfaces |> Array.filter (fun t -> t.IsGenericType)
    let supertypes = TypeUtils.getSupertypes typedef
    let supertypesDefs = supertypes |> List.map TypeUtils.getTypeDef

    let rec trackIndicesHelper (typ : Type) (supertype : Type) indices =
        // TODO: use info about constraints and return typeConstraints array option #SLAVA
        // TODO: this constraints should not be invariant #SLAVA
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
            let track = Array.map get btArgs |> trackIndicesHelper bt supertype

            let typArgs = TypeUtils.getGenericArgs typ
            let traceback = List.collect (fun i -> get typArgs[i] |> List.collect (fun i -> track[i]) |> List.distinct)
            Array.map traceback indices

    let trackIndices (typ : Type) (supertype : Type) =
        let genericsCount = (TypeUtils.getGenericArgs typ).Length
        Array.init genericsCount List.singleton |> trackIndicesHelper typ supertype

    let update (parameters: Type array) (constraints: Dictionary<Type, typeConstraints>) (propagated : typeConstraints[]) =
        for i = 0 to parameters.Length - 1 do
            if parameters[i].IsGenericParameter then
                let parameterConstraints = constraints[parameters[i]]
                let propagatedConstraints = propagated[i]
                parameterConstraints.Merge propagatedConstraints |> ignore

    let rec propagateInterface (constraints : typeConstraints array) parameters (interfaces : Type array) (supertype : Type) =
        // TODO: try to unify with 'propagateSupertype'
        let supertypeDef = TypeUtils.getTypeDef supertype
        let supertypeDefArgs = TypeUtils.getGenericArgs supertypeDef
        let supertypeArgs = TypeUtils.getGenericArgs supertype
        // TODO: need to check non-generic interfaces?
        let index = interfaces |> Array.tryFindIndex (fun t -> t = supertypeDef || t.IsGenericType && t.GetGenericTypeDefinition() = supertypeDef)
        match index with
        | Some index ->
            let interfaceParams = interfaces[index].GetGenericArguments()
            let newConstraints = Dictionary<Type, typeConstraints>()
            for t in parameters do
                newConstraints.Add(t, typeConstraints.Empty)

            let mutable propagated = true
            let updateWithVariance i (param : Type) =
                if propagated then
                    let typ = supertypeArgs[i]
                    let contains, parameterConstraints = newConstraints.TryGetValue param
                    if contains then
                        match supertypeDefArgs[i] with
                        | TypeUtils.Invariant ->
                            parameterConstraints.AddEqual typ
                        | TypeUtils.Covariant ->
                            parameterConstraints.AddSuperType typ
                        | TypeUtils.Contravariant ->
                            parameterConstraints.AddSubType typ
                    elif param.IsGenericType then
                        let typedef = param.GetGenericTypeDefinition()
                        let paramParams = param.GetGenericArguments()
                        let supertypesDefs = TypeUtils.getSupertypes param |> List.map TypeUtils.getTypeDef
                        let genericArgs = typedef.GetGenericArguments()
                        let interfaces = param.GetInterfaces()
                        let toPropagate = List.singleton typ |> typeConstraints.FromSuperTypes
                        match propagate typedef supertypesDefs genericArgs interfaces toPropagate with
                        | Some constraints -> update paramParams newConstraints constraints
                        | None -> propagated <- false
                    else
                        let isSuitable =
                            match supertypeDefArgs[i] with
                            | TypeUtils.Invariant -> CommonUtils.potentiallyEquals param typ
                            | TypeUtils.Covariant -> CommonUtils.potentiallyIsAssignableTo param typ
                            | TypeUtils.Contravariant -> CommonUtils.potentiallyIsAssignableFrom param typ
                        if not isSuitable then propagated <- false

            Array.iteri updateWithVariance interfaceParams
            if propagated then
                let iter i p = constraints[i].Merge newConstraints[p] |> ignore
                Array.iteri iter parameters
            propagated
        | None -> false

    and propagateEqual (constraints : typeConstraints array) typedef (equalType : Type) =
        let equalTypeDef = TypeUtils.getTypeDef equalType
        let equalTypeArgs = TypeUtils.getGenericArgs equalType
        let isSuitable = CommonUtils.potentiallyEquals equalTypeDef typedef
        if isSuitable then
            let parametersIndices = trackIndices typedef equalType
            for i = 0 to parametersIndices.Length - 1 do
                let indices = parametersIndices[i]
                let equal = List.map (fun i -> equalTypeArgs[i]) indices
                let propagated = typeConstraints.FromEqual equal
                constraints[i].Merge propagated |> ignore
        isSuitable

    and propagateSupertype (constraints : typeConstraints array) typedef supertypes (supertype : Type) =
        let supertypeDef = TypeUtils.getTypeDef supertype
        let supertypeArgs = TypeUtils.getGenericArgs supertype
        let contains = List.contains supertypeDef supertypes
        if contains then
            let parametersIndices = trackIndices typedef supertype
            for i = 0 to parametersIndices.Length - 1 do
                let indices = parametersIndices[i]
                let equal = List.map (fun i -> supertypeArgs[i]) indices
                let propagated = typeConstraints.FromEqual equal
                constraints[i].Merge propagated |> ignore
        contains

    and propagateSubtype (constraints : typeConstraints array) typedef subtype =
        let subtypeDef = TypeUtils.getTypeDef subtype
        let subtypeArgs = TypeUtils.getGenericArgs subtype
        let contains = TypeUtils.getSupertypes subtype |> List.map TypeUtils.getTypeDef |> List.contains typedef
        if contains then
            let indices = trackIndices subtypeDef typedef
            let mutable i = 0
            for index in indices do
                for j in index do
                    let parameterConstraints = constraints[j]
                    let t = subtypeArgs[i]
                    parameterConstraints.AddEqual t
                i <- i + 1
        contains

    and propagate (typedef : Type) supertypesDefs parameters interfaces (constraints : typeConstraints) : typeConstraints[] option =
        let sptInterfaces, supertypes = constraints.supertypes |> List.partition (fun t -> t.IsInterface)
        let sbtInterfaces, subtypes = constraints.subtypes |> List.partition (fun t -> t.IsInterface)
        let equalityConstraints = constraints.equal

        let mutable success = true
        let parametersCount = Array.length parameters
        let constraints = Array.init parametersCount (fun _ -> typeConstraints.Empty)

        for equalType in equalityConstraints do
            if success then
                success <- propagateEqual constraints typedef equalType

        if success && typedef.IsInterface then
            success <- success && List.isEmpty supertypes
            // TODO: make proper propagation when 'typedef' is interface
            for iSuperType in sptInterfaces do
                if success then
                    success <- propagateSupertype constraints typedef supertypesDefs iSuperType
            for iSubType in sbtInterfaces do
                if success then
                    success <- propagateSubtype constraints typedef iSubType
            // TODO: try to propagate 'subtypes'
            if success then Some constraints
            else None
        elif success then
            success <- List.isEmpty sbtInterfaces
            for superType in supertypes do
                if success then
                    success <- propagateSupertype constraints typedef supertypesDefs superType
            for subType in subtypes do
                if success then
                    success <- propagateSubtype constraints typedef subType
            for iSuperType in sptInterfaces do
                if success then
                    success <- propagateInterface constraints parameters interfaces iSuperType
            if success then
                Some constraints
            else None
        else None

    let propagateNotEqual constraints (typedef : Type) (notEqual : Type) =
        assert(notEqual.GetGenericArguments().Length = 1)
        propagateEqual constraints typedef notEqual |> ignore

    let propagateNotSupertype constraints (typedef : Type) supertypes (notSupertype : Type) =
        assert(notSupertype.GetGenericArguments().Length = 1)
        propagateSupertype constraints typedef supertypes notSupertype |> ignore

    let propagateNotSubtype constraints typedef (notSubtype : Type) =
        assert(notSubtype.GetGenericArguments().Length = 1)
        propagateSubtype constraints typedef notSubtype |> ignore

    let propagateConstraints (constraints : typeConstraints) : typeConstraints[] option =
        propagate typedef supertypesDefs parameters genericInterfaces constraints

    let propagateNotConstraints typedef supertypesDefs parameters interfaces (toPropagate : typeConstraints) =
        let inline filterSingleGeneric (ts : Type list) =
            ts |> List.filter (fun t -> t.IsGenericType && t.GetGenericArguments().Length = 1)
        let notEqual = filterSingleGeneric toPropagate.notEqual
        let nSptInterfaces, nSupertypes =
            filterSingleGeneric toPropagate.notSupertypes |> List.partition (fun t -> t.IsInterface)
        let nSbtInterfaces, nSubtypes =
            filterSingleGeneric toPropagate.notSubtypes |> List.partition (fun t -> t.IsInterface)

        // TODO: try to propagate 'nSbtInterfaces'
        let parametersCount = Array.length parameters
        let constraints = Array.init parametersCount (fun _ -> typeConstraints.Empty)
        for notEqualType in notEqual do
            propagateNotEqual constraints typedef notEqualType
        for notSuperType in nSupertypes do
            propagateNotSupertype constraints typedef supertypesDefs notSuperType
        for notSubType in nSubtypes do
            propagateNotSubtype constraints typedef notSubType
        for notISuperType in nSptInterfaces do
            propagateInterface constraints parameters interfaces notISuperType |> ignore
        for c in constraints do
            c.Negate()
        constraints

    let propagateNotConstraints (constraints : typeConstraints) =
        propagateNotConstraints typedef supertypesDefs parameters genericInterfaces constraints

    let types =
        parameterSubstitutions.Substitutions
        |> Seq.map (GenericUtils.substitute typedef)

    static member TryCreate (typedef : Type) depth makeSubstitution =
        if depth <= 0 then None
        else
            option {
                let makeGenericCandidate t d =
                    genericCandidate.TryCreate t d makeSubstitution
                let parameters = typedef.GetGenericArguments()
                let! paramSubsts = makeSubstitution parameters depth makeGenericCandidate
                let selfConstraints = typeConstraints.Empty
                return genericCandidate(typedef, depth, paramSubsts, selfConstraints)
            }

    member x.AddConstraints constraints =
        let selfConstraints = selfConstraints.Copy()
        selfConstraints.Merge constraints |> ignore

        let isSupertypeValid interfacesDefs supertypesDefs (supertype : Type) =
            let supertypeDef = TypeUtils.getTypeDef supertype
            if supertypeDef.IsInterface && not typedef.IsInterface then Array.contains supertypeDef interfacesDefs
            else List.contains supertypeDef supertypesDefs

        let isSubtypeValid (subtype : Type) =
            if subtype.IsInterface && not typedef.IsInterface then false
            else
                let supertypesDefs = TypeUtils.getSupertypes subtype |> List.map TypeUtils.getTypeDef
                isSupertypeValid [||] supertypesDefs typedef

        let newIsEmptied =
            selfConstraints.IsContradicting()
            || List.forall (CommonUtils.potentiallyEquals typedef) constraints.equal |> not
            || List.forall (isSupertypeValid interfacesDefs supertypesDefs) constraints.supertypes |> not
            || List.forall isSubtypeValid constraints.subtypes |> not

        if newIsEmptied then None
        else
            let notConstraints = propagateNotConstraints constraints
            match propagateConstraints constraints with
            | Some constraints ->
                let parameterConstraints = Dictionary<Type, typeConstraints>()
                for i = 0 to parameters.Length - 1 do
                    let constraints = constraints[i]
                    constraints.Merge notConstraints[i] |> ignore
                    parameterConstraints.Add(parameters[i], constraints)
                match parameterSubstitutions.AddConstraints parameterConstraints with
                | Some newParameterSubstitutions ->
                    genericCandidate(typedef, depth, newParameterSubstitutions, selfConstraints) |> Some
                | None -> None
            | None -> None

    member val Typedef = typedef

    member val Types =
        types
        |> Seq.truncate 1000 // TODO: make another way to prevent long iteration through types
        |> Seq.filter (GroundUtils.satisfiesConstraints selfConstraints PersistentDict.empty)

    member x.IsEmpty = Seq.isEmpty x.Types

    member x.Copy() =
        let copiedSelfConstraints = selfConstraints.Copy()
        genericCandidate(typedef, depth, parameterSubstitutions, copiedSelfConstraints)

and arrayKind =
    | OneDimensionalArray
    | MultidimensionalArray of int

and arrayCandidate private(kind, parameterSubstitutions : parameterSubstitutions) =

    let update (parameterSubstitutions : parameterSubstitutions) =
        arrayCandidate(kind, parameterSubstitutions)

    let interfacesDefs =
        match kind with
        | OneDimensionalArray -> typeof<int array>
        | MultidimensionalArray rank -> typeof<int>.MakeArrayType(rank)
        |> TypeUtils.getAllInterfaces
        |> Array.map TypeUtils.getTypeDef

    let genericInterfacesDefs = Array.filter TypeUtils.isGenericType interfacesDefs

    let supertypes = [| typeof<Array>; typeof<obj> |]

    let (|Array|_|) (t : Type) =
        match t with
        | TypeUtils.ArrayType(elementType, dim) ->
            match dim with
            | Vector -> Array(elementType, OneDimensionalArray) |> Some
            | ConcreteDimension rank -> Array(elementType, MultidimensionalArray rank) |> Some
            | SymbolicDimension -> None
        | _ -> None

    let (|GenericInterface|_|) (t : Type) =
        let tDef = TypeUtils.getTypeDef t
        if Array.contains tDef genericInterfacesDefs then
            t.GetGenericArguments()[0] |> Some
        else None

    let (|Interface|_|) (t : Type) =
        if Array.contains t interfacesDefs then Some ()
        else None

    let (|Supertype|_|) (t : Type) =
        if Array.contains t supertypes then Some ()
        else None

    let propagateAll onPropagated typ =
        let mutable isPropagated = true
        let propagatedTyp =
            match typ with
            | GenericInterface(parameter) -> Some parameter
            | Interface -> None
            | Supertype -> None
            | Array(elementType, typKind) when typKind = kind -> Some elementType
            | _ -> isPropagated <- false; None
        Option.iter onPropagated propagatedTyp
        isPropagated

    let propagateStrict onPropagated typ =
        let mutable isPropagated = true
        let propagatedTyp =
            match typ with
            | Supertype -> None
            | Array(typElementType, typKind) when typKind = kind -> Some typElementType
            | _ -> isPropagated <- false; None
        Option.iter onPropagated propagatedTyp
        isPropagated

    let propagateAllNot onPropagated typ =
        let mutable isPropagated = true
        let propagatedTyp =
            match typ with
            | GenericInterface _ -> isPropagated <- false; None
            | Interface -> isPropagated <- false; None
            | Supertype -> isPropagated <- false; None
            | Array(elementType, typKind) when typKind = kind -> Some elementType
            | _ -> None
        Option.iter onPropagated propagatedTyp
        isPropagated

    let propagateStrictNot onPropagated typ =
        let propagatedTyp =
            match typ with
            | Array(elementType, typKind) when typKind = kind -> Some elementType
            | _ -> None
        Option.iter onPropagated propagatedTyp
        true

    let isReferenceArray typ =
        match typ with
        | Array(elementType, _) -> CommonUtils.isValueType elementType |> not
        | _ -> false

    let mkArray (t : Type) =
        match kind with
        | OneDimensionalArray -> t.MakeArrayType()
        | MultidimensionalArray rank -> t.MakeArrayType(rank)

    let decodeSubstitution (substitution : GenericUtils.parameterSubstitution) =
        PersistentDict.values substitution
        |> Seq.head
        |> mkArray

    let types =
        parameterSubstitutions.Substitutions
        |> Seq.map decodeSubstitution

    new (kind, mkParameterSubstitutions) =
        let parameter = typedefof<IEnumerable<int>>.GetGenericArguments()[0]
        arrayCandidate(kind, mkParameterSubstitutions parameter)

    member private x.Parameter = typedefof<IEnumerable<int>>.GetGenericArguments()[0]

    member x.Types = types |> Seq.truncate 1000 // TODO: make another way to prevent long iteration through types

    member x.AddConstraints(constraints : typeConstraints) =
        let propagatedConstraints = typeConstraints.Empty
        if List.forall (propagateAllNot propagatedConstraints.AddSuperType) constraints.notSupertypes
            && List.forall (propagateStrictNot propagatedConstraints.AddSubType) constraints.notSubtypes
            && List.forall (propagateStrictNot propagatedConstraints.AddEqual) constraints.notEqual
        then
            propagatedConstraints.Negate()
            if List.forall (propagateAll propagatedConstraints.AddSuperType) constraints.supertypes
                && List.forall (propagateStrict propagatedConstraints.AddSubType) constraints.subtypes
                && List.forall (propagateStrict propagatedConstraints.AddEqual) constraints.equal
            then
                if List.exists isReferenceArray constraints.supertypes then
                    propagatedConstraints.AddNotSupertype(typeof<ValueType>)
                    propagatedConstraints.AddNotSupertype(typeof<Enum>)

                // need to add value types to equal constraints because of array parameter invariance for value types
                let equalFromSupertypes, supertypes = propagatedConstraints.supertypes |> List.partition CommonUtils.isValueType
                let equalFromSubtypes, subtypes = propagatedConstraints.subtypes |> List.partition (fun t -> t.IsValueType)

                List.iter propagatedConstraints.AddEqual equalFromSupertypes
                List.iter propagatedConstraints.AddEqual equalFromSubtypes

                propagatedConstraints.supertypes <- supertypes
                propagatedConstraints.subtypes <- subtypes

                let parametersConstraints = Dictionary()
                parametersConstraints.Add(x.Parameter, propagatedConstraints)
                parameterSubstitutions.AddConstraints parametersConstraints
                |> Option.map update
            else None
        else None

and candidate =
    | GenericCandidate of genericCandidate
    | Candidate of Type
    | ArrayCandidate of arrayCandidate
with

    member x.IsAbstract =
        match x with
        | Candidate t -> t.IsAbstract
        | GenericCandidate gc -> gc.Typedef.IsAbstract
        | ArrayCandidate _ -> false

    member x.Types =
        match x with
        | Candidate t -> Seq.singleton t
        | GenericCandidate gc -> gc.Types
        | ArrayCandidate ac -> ac.Types

    member x.SpecialConstraintsInfo =
        match x with
        | Candidate t -> typeSpecialConstraintsInfo.FromType t
        | GenericCandidate gc -> typeSpecialConstraintsInfo.FromType gc.Typedef
        | ArrayCandidate _ -> {
            isByRefLike = false
            isReferenceType = true
            isNotNullableValueType = false
            hasDefaultConstructor = false
        }

and CandidateGroups = {
    publicBuiltIn : candidate seq
    publicUser : candidate seq
    privateUser : candidate seq
    rest : candidate seq
}
with
    static member GroupBy (userAssembly : Assembly) (items: _ list) (toCandidate : _ -> candidate) toType =
        let isPublicBuiltIn (t : Type) = TypeUtils.isPublic t && Reflection.isBuiltInType t
        let isPublicUser (t : Type) = TypeUtils.isPublic t && t.Assembly = userAssembly
        let isPrivateUser (t : Type) = not (TypeUtils.isPublic t) && t.Assembly = userAssembly

        let inline getCandidates p items = List.partition (toType >> p) items
        let publicBuiltIn, rest = getCandidates isPublicBuiltIn items
        let publicUser, rest = getCandidates isPublicUser rest
        let privateUser, rest = getCandidates isPrivateUser rest
        {
            publicBuiltIn = publicBuiltIn |> List.map toCandidate
            publicUser = publicUser |> List.map toCandidate
            privateUser = privateUser |> List.map toCandidate
            rest = rest |> List.map toCandidate
        }

    member x.Filter shouldBeTaken =
        {
            publicBuiltIn = Seq.choose shouldBeTaken x.publicBuiltIn
            publicUser = Seq.choose shouldBeTaken x.publicUser
            privateUser = Seq.choose shouldBeTaken x.privateUser
            rest = Seq.choose shouldBeTaken x.rest
        }

    member x.Joined =
        seq {
            yield! x.publicBuiltIn
            yield! x.publicUser
            yield! x.privateUser
            yield! x.rest
        }

    member x.Eval() =
        {
            publicBuiltIn = Seq.toList x.publicBuiltIn
            publicUser = Seq.toList x.publicUser
            privateUser = Seq.toList x.privateUser
            rest = Seq.toList x.rest
        }

    member x.DistinctBy keySelector =
        {
            publicBuiltIn = Seq.distinctBy keySelector x.publicBuiltIn
            publicUser = Seq.distinctBy keySelector x.publicUser
            privateUser = Seq.distinctBy keySelector x.privateUser
            rest = Seq.distinctBy keySelector x.rest
        }

and candidates private(typeGroups : CandidateGroups, genericGroups: CandidateGroups, arrays : candidate list, mock, userAssembly) =

    let orderedCandidates = seq {
        yield! typeGroups.Joined
        yield! arrays
        yield! genericGroups.Joined
    }

    new(cs : seq<candidate>, mock : ITypeMock option, userAssembly : Assembly) =
        let cs = List.ofSeq cs |> List.distinct
        let takeGeneric = function | GenericCandidate c -> Some c | _ -> None
        let takeNonGeneric = function | Candidate t -> Some t | _ -> None
        let takeArrays = function | ArrayCandidate _ as c -> Some c | _ -> None

        let types, cs = cs |> List.mappedPartition takeNonGeneric
        let genericTypes, cs = cs |> List.mappedPartition takeGeneric
        let arrays, cs = cs |> List.mappedPartition takeArrays
        assert (cs = [])

        let typeGroups = CandidateGroups.GroupBy userAssembly types Candidate id
        let getTypeDef (gt : genericCandidate) = gt.Typedef
        let genericGroups = CandidateGroups.GroupBy userAssembly genericTypes GenericCandidate getTypeDef

        candidates(typeGroups, genericGroups, arrays, mock, userAssembly)

    member x.IsEmpty
        with get() =
            match mock with
            | Some _ -> false
            | None -> Seq.isEmpty x.Types

    member x.Types =
        seq {
            yield! orderedCandidates |> Seq.collect (fun c -> c.Types) |> Seq.map ConcreteType
            if mock.IsSome then yield mock.Value |> MockType
        }

    member x.ConcreteTypes = orderedCandidates

    member x.HasMock = Option.isSome mock

    static member Empty() =
        candidates(Seq.empty, None, Reflection.mscorlibAssembly)

    member x.Copy(changeMock: ITypeMock -> ITypeMock) =
        let newMock = Option.map changeMock mock
        candidates(typeGroups, genericGroups, arrays, newMock, userAssembly)

    member x.Pick() = Seq.head x.Types

    member x.Filter shouldBeTaken (refineMock : ITypeMock -> ITypeMock option) =
        let types = typeGroups.Filter shouldBeTaken
        let generics = genericGroups.Filter shouldBeTaken
        let arrays = List.choose shouldBeTaken arrays
        let mock = Option.bind refineMock mock
        candidates(types, generics, arrays, mock, userAssembly)

    member x.KeepOnlyMock() = candidates(Seq.empty, mock, userAssembly)

    member x.DistinctBy(keySelector : candidate -> 'a) =
        let distinctTypeGroups = typeGroups.DistinctBy keySelector
        let distinctGenericGroups = genericGroups.DistinctBy keySelector
        let distinctArrays = List.distinctBy keySelector arrays
        candidates(distinctTypeGroups, distinctGenericGroups, distinctArrays, mock, userAssembly)

    member x.Take(count) =
        let types =
            match mock with
            | Some _ -> Seq.truncate (count - 1) orderedCandidates
            | None -> Seq.truncate count orderedCandidates
        candidates(types, mock, userAssembly)

    member x.Eval() =
        candidates(typeGroups.Eval(), genericGroups.Eval(), arrays, mock, userAssembly)

type typeStorage private (constraints, addressesTypes, typeMocks, classesParams, methodsParams) =
    let mutable classesParams = classesParams
    let mutable methodsParams = methodsParams

    new() =
        let constraints = typesConstraints()
        let addressesTypes = Dictionary<term, candidates>()
        let typeMocks = Dictionary<Type list, ITypeMock>()
        let classesParams : symbolicType[] = Array.empty
        let methodsParams : symbolicType[] = Array.empty
        typeStorage(constraints, addressesTypes, typeMocks, classesParams, methodsParams)

    member x.Constraints with get() = constraints
    member x.AddressesTypes with get() = addressesTypes
    member x.TypeMocks with get() = typeMocks
    member x.ClassesParams
        with get() = classesParams
        and set newClassesParams =
            classesParams <- newClassesParams
    member x.MethodsParams
        with get() = methodsParams
        and set newMethodsParams =
            methodsParams <- newMethodsParams

    member x.Copy() =
        let newConstraints = constraints.Copy()
        let newTypeMocks = Dictionary<Type list, ITypeMock>()
        let newAddressesTypes = Dictionary()
        for entry in addressesTypes do
            let address = entry.Key
            let addressCandidates = entry.Value
            let changeMock (m : ITypeMock) =
                let superTypes = Seq.toList m.SuperTypes
                let mock = ref (EmptyTypeMock() :> ITypeMock)
                if newTypeMocks.TryGetValue(superTypes, mock) then mock.Value
                else
                    let newMock = m.Copy()
                    newTypeMocks.Add(superTypes, newMock)
                    newMock
            let newCandidates = addressCandidates.Copy(changeMock)
            newAddressesTypes.Add(address, newCandidates)
        typeStorage(newConstraints, newAddressesTypes, newTypeMocks, classesParams, methodsParams)

    member x.AddConstraint address typeConstraint =
        constraints.Add address typeConstraint

    member x.Item
        with get (address : term) =
            let t = ref (candidates.Empty())
            if addressesTypes.TryGetValue(address, t) then Some t.Value
            else None
        and set (address : term) (candidates : candidates) =
            assert(candidates.IsEmpty |> not)
            addressesTypes[address] <- candidates

    member x.IsValid with get() = addressesTypes.Count = constraints.Count
