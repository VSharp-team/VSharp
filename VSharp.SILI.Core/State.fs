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
    | StateModel of state * typeModel
with
    member x.Complete value =
        match x with
        | StateModel(state, _) when state.complete ->
            // TODO: ideally, here should go the full-fledged substitution, but we try to improve the performance a bit...
            match value.term with
            | Constant(_, _, typ) -> makeDefaultValue typ
            | HeapRef({term = Constant _}, t) -> nullRef t
            | _ -> value
        | _ -> value

    static member private EvalDict (subst : IDictionary<ISymbolicConstantSource, term>) source term typ complete =
        let value = ref Nop
        if subst.TryGetValue(source, value) then value.Value
        elif complete then makeDefaultValue typ
        else term

    member x.Eval term =
        Substitution.substitute (function
            | { term = Constant(_, (:? IStatedSymbolicConstantSource as source), typ) } as term ->
                match x with
                | StateModel(state, _) -> source.Compose state
                | PrimitiveModel subst -> model.EvalDict subst source term typ true
            | { term = Constant(_, source, typ) } as term ->
                let subst, complete =
                    match x with
                    | PrimitiveModel dict -> dict, true
                    | StateModel(state, _) ->
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

    static member FromSuperTypes(superTypes : Type list) =
        let empty = List.empty
        let superTypes = List.filter (fun t -> t <> typeof<obj>) superTypes |> List.distinct
        { subtypes = empty; supertypes = superTypes; notSubtypes = empty; notSupertypes = empty }

    member x.Merge(other : typeConstraints) =
        if x.supertypes <> other.supertypes then
            x.supertypes <- x.supertypes @ other.supertypes |> List.distinct
        if x.subtypes <> other.subtypes then
            x.subtypes <- x.subtypes @ other.subtypes |> List.distinct
        if x.notSubtypes <> other.notSubtypes then
            x.notSubtypes <- x.notSubtypes @ other.notSubtypes |> List.distinct
        if x.notSupertypes <> other.notSupertypes then
            x.notSupertypes <- x.notSupertypes @ other.notSupertypes |> List.distinct

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
        let newAddresses = ResizeArray<term>()
        let allConstraints = Dictionary<term, typeConstraints>()
        typesConstraints(newAddresses, allConstraints)

    member x.Copy() =
        let copiedNewAddresses = ResizeArray<term>(newAddresses)
        let copiedConstraints = Dictionary<term, typeConstraints>()
        for entry in constraints do
            copiedConstraints.Add(entry.Key, entry.Value.Copy())
        typesConstraints(copiedNewAddresses, copiedConstraints)

    member private x.AddNewAddress address =
        if newAddresses.Contains address |> not then
            newAddresses.Add address

    member x.ClearNewAddresses() =
        newAddresses.Clear()

    member x.NewAddresses with get() = newAddresses

    member x.Add (address : term) (typeConstraint : typeConstraints) =
        x.AddNewAddress address
        let current = ref (typeConstraints.Empty())
        if constraints.TryGetValue(address, current) then
            current.Value.Merge typeConstraint
        else constraints.Add(address, typeConstraint)

    member x.Remove (address : term) =
        newAddresses.Remove address |> ignore
        constraints.Remove address |> ignore

    member x.MergeConstraints (addresses : term seq) =
        let resultConstraints = typeConstraints.Empty()
        for address in addresses do
            let constraints = constraints[address]
            resultConstraints.Merge constraints
            x.AddNewAddress address
        for address in addresses do
            constraints[address] <- resultConstraints

    interface System.Collections.IEnumerable with
        member this.GetEnumerator() =
          upcast constraints.GetEnumerator()

    interface IEnumerable<KeyValuePair<term, typeConstraints>> with
        override this.GetEnumerator() =
          constraints.GetEnumerator()

    member x.Item(address : term) =
        constraints[address].Copy()

and typeModel =
    {
        constraints : typesConstraints
        addressesTypes : Dictionary<term, symbolicType seq>
        mutable classesParams : symbolicType[]
        mutable methodsParams : symbolicType[]
        typeMocks : IDictionary<Type list, ITypeMock>
    }
with
    static member CreateEmpty() =
        {
            constraints = typesConstraints()
            addressesTypes = Dictionary()
            classesParams = Array.empty
            methodsParams = Array.empty
            typeMocks = Dictionary()
        }

    member x.AddConstraint address typeConstraint =
        x.constraints.Add address typeConstraint

    member x.AddSuperType address superType =
        let typeConstraint = List.singleton superType |> typeConstraints.FromSuperTypes
        x.AddConstraint address typeConstraint

    member x.Copy() =
        let newConstraints = x.constraints.Copy()
        let newTypeMocks = Dictionary<Type list, ITypeMock>()
        let newAddressesTypes = Dictionary()
        for entry in x.addressesTypes do
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
        {
            constraints = newConstraints
            addressesTypes = newAddressesTypes
            classesParams = x.classesParams
            methodsParams = x.methodsParams
            typeMocks = newTypeMocks
        }

    member x.Item(address : term) =
        let types = ref null
        if x.addressesTypes.TryGetValue(address, types) then Some types.Value
        else None

and
    [<ReferenceEquality>]
    state = {
        mutable pc : pathCondition
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
