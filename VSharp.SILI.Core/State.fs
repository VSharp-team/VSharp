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
    abstract GetAllArrayData : concreteHeapAddress -> seq<int list * obj>
    abstract ReadArrayLowerBound : concreteHeapAddress -> int -> obj
    abstract ReadArrayLength : concreteHeapAddress -> int -> obj
    abstract WriteClassField : concreteHeapAddress -> fieldId -> obj -> unit
    abstract WriteArrayIndex : concreteHeapAddress -> int list -> obj -> unit
    abstract InitializeArray : concreteHeapAddress -> RuntimeFieldHandle -> unit
    abstract CopyCharArrayToString : concreteHeapAddress -> concreteHeapAddress -> unit
    abstract Remove : concreteHeapAddress -> unit

type IMethodMock =
    abstract BaseMethod : System.Reflection.MethodInfo
    abstract Call : concreteHeapAddress -> term list -> term option
    abstract GetImplementationClauses : unit -> term array

type ITypeMock =
    abstract Name : string
    abstract SuperTypes : Type seq
    abstract MethodMock : IMethod -> IMethodMock
    abstract MethodMocks : IMethodMock seq
    abstract Copy : unit -> ITypeMock

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

    static member private EvalDict (subst : IDictionary<ISymbolicConstantSource, term>) source term typ complete =
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
    typeMocks : IDictionary<Type list, ITypeMock>
}

and
    IStatedSymbolicConstantSource =
        inherit ISymbolicConstantSource
        abstract Compose : state -> term
