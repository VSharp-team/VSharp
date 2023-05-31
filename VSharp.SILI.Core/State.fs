namespace VSharp.Core

open System
open System.Collections.Generic
open VSharp
open VSharp.Core
open VSharp.Utils

type typeVariables = mappedStack<typeWrapper, Type> * Type list stack

type stackBufferKey = concreteHeapAddress

// TODO: add empty concrete memory class
type IConcreteMemory =
    abstract Copy : unit -> IConcreteMemory
    abstract Contains : concreteHeapAddress -> bool
    abstract VirtToPhys : concreteHeapAddress -> obj
    abstract TryVirtToPhys : concreteHeapAddress -> obj option
    abstract PhysToVirt : obj -> concreteHeapAddress
    abstract TryPhysToVirt : obj -> concreteHeapAddress option
    abstract Allocate : concreteHeapAddress -> obj -> unit
    abstract ReadClassField : concreteHeapAddress -> fieldId -> obj
    abstract ReadArrayIndex : concreteHeapAddress -> int list -> obj
    // TODO: too expensive! use Seq.iter2 instead with lazy indices sequence
    abstract GetAllArrayData : concreteHeapAddress -> seq<int list * obj>
    abstract ReadArrayLowerBound : concreteHeapAddress -> int -> obj
    abstract ReadArrayLength : concreteHeapAddress -> int -> obj
    abstract WriteClassField : concreteHeapAddress -> fieldId -> obj -> unit
    abstract WriteArrayIndex : concreteHeapAddress -> int list -> obj -> unit
    abstract InitializeArray : concreteHeapAddress -> RuntimeFieldHandle -> unit
    abstract FillArray : concreteHeapAddress -> int -> int -> obj -> unit
    abstract CopyArray : concreteHeapAddress -> concreteHeapAddress -> int64 -> int64 -> int64 -> unit
    abstract CopyCharArrayToString : concreteHeapAddress -> concreteHeapAddress -> unit
    abstract Remove : concreteHeapAddress -> unit

type IMethodMock =
    abstract BaseMethod : System.Reflection.MethodInfo
    abstract Call : term -> term list -> term option
    abstract GetImplementationClauses : unit -> term array
    abstract Copy : unit -> IMethodMock

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

// TODO: is it good idea to add new constructor for recognizing cilStates that construct RuntimeExceptions?
type exceptionRegister =
    | Unhandled of term * bool // Exception term * is runtime exception
    | Caught of term
    | NoException
    with
    member x.GetError () =
        match x with
        | Unhandled(error, _) -> error
        | Caught error -> error
        | _ -> internalfail "no error"

    member x.TransformToCaught () =
        match x with
        | Unhandled(e, _) -> Caught e
        | _ -> internalfail "unable TransformToCaught"
    member x.TransformToUnhandled () =
        match x with
        | Caught e -> Unhandled(e, false)
        | _ -> internalfail "unable TransformToUnhandled"
    member x.UnhandledError =
        match x with
        | Unhandled _ -> true
        | _ -> false
    member x.ExceptionTerm =
        match x with
        | Unhandled (error, _)
        | Caught error -> Some error
        | _ -> None
    static member map f x =
        match x with
        | Unhandled(e, isRuntime) -> Unhandled(f e, isRuntime)
        | Caught e -> Caught <| f e
        | NoException -> NoException

type arrayCopyInfo =
    {srcAddress : heapAddress; contents : arrayRegion; srcIndex : term; dstIndex : term; length : term; srcSightType : Type; dstSightType : Type} with
        override x.ToString() =
            sprintf "    source address: %O, from %O ranging %O elements into %O index with cast to %O;\n\r    updates: %O" x.srcAddress x.srcIndex x.length x.dstIndex x.dstSightType (MemoryRegion.toString "        " x.contents)

type model =
    | PrimitiveModel of IDictionary<ISymbolicConstantSource, term>
    | StateModel of state
with
    member x.Complete value =
        match x with
        | StateModel state when state.complete ->
            // TODO: ideally, here should go the full-fledged substitution, but we try to improve the performance a bit...
            match value.term with
            | Constant(_, _, typ) -> makeDefaultValue typ
            | HeapRef({term = Constant _}, t) -> nullRef t
            | _ -> value
        | _ -> value

    static member EvalDict (subst : IDictionary<ISymbolicConstantSource, term>) source term typ complete =
        let value = ref Nop
        if subst.TryGetValue(source, value) then value.Value
        elif complete then makeDefaultValue typ
        else term

    member x.Eval term =
        Substitution.substitute (function
            | { term = Constant(_, (:? IStatedSymbolicConstantSource as source), typ) } as term ->
                match x with
                | StateModel state -> source.Compose state
                | PrimitiveModel subst -> model.EvalDict subst source term typ true
            | { term = Constant(_, source, typ) } as term ->
                let subst, complete =
                    match x with
                    | PrimitiveModel dict -> dict, true
                    | StateModel state ->
                        match state.model with
                        | PrimitiveModel dict -> dict, state.complete
                        | _ -> __unreachable__()
                model.EvalDict subst source term typ complete
            | term -> term) id id term

// TODO: use set instead of list? #type
and typeConstraints =
    {
        mutable supertypes : Type list
        mutable subtypes : Type list
        mutable notSubtypes : Type list
        mutable notSupertypes : Type list
    }
with
    static member Empty() =
        let empty = List.empty
        { subtypes = empty; supertypes = empty; notSubtypes = empty; notSupertypes = empty }

    static member FromSuperTypes (superTypes : Type list) =
        let empty = List.empty
        let superTypes = List.filter (fun t -> t <> typeof<obj>) superTypes |> List.distinct
        { subtypes = empty; supertypes = superTypes; notSubtypes = empty; notSupertypes = empty }

    static member Create supertypes subtypes notSupertypes notSubtypes =
        let supertypes = List.filter (fun t -> t <> typeof<obj>) supertypes |> List.distinct
        let subtypes = List.distinct subtypes
        let notSupertypes = List.distinct notSupertypes
        let notSubtypes = List.distinct notSubtypes
        { subtypes = subtypes; supertypes = supertypes; notSubtypes = notSubtypes; notSupertypes = notSupertypes }

    member x.Merge(other : typeConstraints) : bool =
        let mutable changed = false
        if x.supertypes <> other.supertypes then
            changed <- true
            x.supertypes <- x.supertypes @ other.supertypes |> List.distinct
        if x.subtypes <> other.subtypes then
            changed <- true
            x.subtypes <- x.subtypes @ other.subtypes |> List.distinct
        if x.notSubtypes <> other.notSubtypes then
            changed <- true
            x.notSubtypes <- x.notSubtypes @ other.notSubtypes |> List.distinct
        if x.notSupertypes <> other.notSupertypes then
            changed <- true
            x.notSupertypes <- x.notSupertypes @ other.notSupertypes |> List.distinct
        changed

    member x.IsContradicting() =
        let nonComparable (t : Type) (u : Type) =
            u.IsClass && t.IsClass && (not <| u.IsAssignableTo t) && (not <| u.IsAssignableFrom t)
            || t.IsSealed && u.IsInterface && not (t.IsAssignableTo u)
        // X <: u and u <: t and X </: t
        x.supertypes |> List.exists (fun u -> x.notSupertypes |> List.exists u.IsAssignableTo)
        || // u <: X and t <: u and t </: X
        x.subtypes |> List.exists (fun u -> x.notSubtypes |> List.exists u.IsAssignableFrom)
        || // u <: X and X <: t and u </: t
        x.subtypes |> List.exists (fun u -> x.supertypes |> List.exists (u.IsAssignableTo >> not))
        || // No multiple inheritance -- X <: u and X <: t and u </: t and t </: u and t, u are classes
        x.supertypes |> List.exists (fun u -> x.supertypes |> List.exists (nonComparable u))
        || // u </: X and X <: u when u is sealed
        x.supertypes |> List.exists (fun u -> u.IsSealed && x.notSubtypes |> List.contains u)

    member x.AddSuperType(superType : Type) =
        if superType <> typeof<obj> then
            x.supertypes <- superType :: x.supertypes |> List.distinct

    member x.Copy() =
        {
            supertypes = x.supertypes
            subtypes = x.subtypes
            notSubtypes = x.notSubtypes
            notSupertypes = x.notSupertypes
        }

and typesConstraints private (newAddresses, constraints) =

    new () =
        let newAddresses = HashSet<term>()
        let allConstraints = Dictionary<term, typeConstraints>()
        typesConstraints(newAddresses, allConstraints)

    member x.Copy() =
        let copiedNewAddresses = HashSet<term>(newAddresses)
        let copiedConstraints = Dictionary<term, typeConstraints>()
        for entry in constraints do
            copiedConstraints.Add(entry.Key, entry.Value.Copy())
        typesConstraints(copiedNewAddresses, copiedConstraints)

    member private x.AddNewAddress address =
        newAddresses.Add address |> ignore

    member x.ClearNewAddresses() =
        newAddresses.Clear()

    member x.NewAddresses with get() = newAddresses

    member x.Add (address : term) (typeConstraint : typeConstraints) =
        let current = ref (typeConstraints.Empty())
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
                    if different then unequal.Add(address1, address2) |> ignore
                    else isValid <- false
        isValid, unequal

    interface System.Collections.IEnumerable with
        member this.GetEnumerator() =
          upcast constraints.GetEnumerator()

    interface IEnumerable<KeyValuePair<term, typeConstraints>> with
        override this.GetEnumerator() =
          constraints.GetEnumerator()

    member x.Item(address : term) =
        constraints[address].Copy()

    member x.Count with get() = constraints.Count

and typeStorage private (constraints, addressesTypes, typeMocks, classesParams, methodsParams) =
    let mutable classesParams = classesParams
    let mutable methodsParams = methodsParams

    new() =
        let constraints = typesConstraints()
        let addressesTypes = Dictionary<term, symbolicType seq>()
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
            let types = entry.Value
            let changeType = function
                | ConcreteType _ as t -> t
                | MockType m ->
                    let superTypes = Seq.toList m.SuperTypes
                    let mock = ref (EmptyTypeMock() :> ITypeMock)
                    if newTypeMocks.TryGetValue(superTypes, mock) then MockType mock.Value
                    else
                        let newMock = m.Copy()
                        newTypeMocks.Add(superTypes, newMock)
                        MockType newMock
            let newTypes = Seq.map changeType types
            newAddressesTypes.Add(address, newTypes)
        typeStorage(newConstraints, newAddressesTypes, newTypeMocks, classesParams, methodsParams)

    member x.AddConstraint address typeConstraint =
        constraints.Add address typeConstraint

    member x.Item(address : term) =
        let types = ref null
        if addressesTypes.TryGetValue(address, types) then Some types.Value
        else None

    member x.IsValid with get() = addressesTypes.Count = constraints.Count

and
    [<ReferenceEquality>]
    state = {
        mutable pc : pathCondition
        mutable typeStorage : typeStorage
        mutable evaluationStack : evaluationStack
        mutable stack : callStack                                          // Arguments and local variables
        mutable stackBuffers : pdict<stackKey, stackBufferRegion>          // Buffers allocated via stackAlloc
        mutable classFields : pdict<fieldId, heapRegion>                   // Fields of classes in heap
        mutable arrays : pdict<arrayType, arrayRegion>                     // Contents of arrays in heap
        mutable lengths : pdict<arrayType, vectorRegion>                   // Lengths by dimensions of arrays in heap
        mutable lowerBounds : pdict<arrayType, vectorRegion>               // Lower bounds by dimensions of arrays in heap
        mutable staticFields : pdict<fieldId, staticsRegion>               // Static fields of types without type variables
        mutable boxedLocations : pdict<concreteHeapAddress, term>          // Value types boxed in heap
        mutable initializedTypes : symbolicTypeSet                         // Types with initialized static members
        concreteMemory : IConcreteMemory                                   // Fully concrete objects
        mutable allocatedTypes : pdict<concreteHeapAddress, symbolicType>  // Types of heap locations allocated via new
        mutable typeVariables : typeVariables                              // Type variables assignment in the current state
        mutable delegates : pdict<concreteHeapAddress, term>               // Subtypes of System.Delegate allocated in heap
        mutable currentTime : vectorTime                                   // Current timestamp (and next allocated address as well) in this state
        mutable startingTime : vectorTime                                  // Timestamp before which all allocated addresses will be considered symbolic
        mutable exceptionsRegister : exceptionRegister                     // Heap-address of exception object
        mutable model : model                                              // Concrete valuation of symbolics
        complete : bool                                                    // If true, reading of undefined locations would result in default values
        methodMocks : IDictionary<IMethod, IMethodMock>
    }

and
    IStatedSymbolicConstantSource =
        inherit ISymbolicConstantSource
        abstract Compose : state -> term
