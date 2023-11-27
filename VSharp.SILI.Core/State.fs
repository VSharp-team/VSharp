namespace VSharp.Core

open System
open System.Collections.Generic
open System.Reflection
open VSharp
open VSharp.Core
open VSharp.Utils

type typeVariables = mappedStack<typeWrapper, Type> * Type list stack

type stackBufferKey = concreteHeapAddress

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
    abstract FillArray : concreteHeapAddress -> int -> int -> obj -> unit
    abstract CopyArray : concreteHeapAddress -> concreteHeapAddress -> int64 -> int64 -> int64 -> unit
    abstract CopyCharArrayToString : concreteHeapAddress -> concreteHeapAddress -> int -> unit
    abstract CopyCharArrayToStringLen : concreteHeapAddress -> concreteHeapAddress -> int -> int -> unit
    abstract Remove : concreteHeapAddress -> unit

// TODO: is it good idea to add new constructor for recognizing cilStates that construct RuntimeExceptions?
type exceptionRegister =
    | Unhandled of term * bool * string // Exception term * is runtime exception * stack trace
    | Caught of term * bool * string // Exception term * is runtime exception * stack trace
    | NoException
    with
    member x.GetError () =
        match x with
        | Unhandled(error, _, _) -> error
        | Caught(error, _, _) -> error
        | _ -> internalfail "no error"
    member x.TransformToCaught () =
        match x with
        | Unhandled(e, isRuntime, s) -> Caught(e, isRuntime, s)
        | _ -> internalfail "unable TransformToCaught"
    member x.TransformToUnhandled () =
        match x with
        | Caught(e, isRuntime, s) -> Unhandled(e, isRuntime, s)
        | Unhandled _ -> x
        | NoException -> internalfail "unable TransformToUnhandled"
    member x.IsUnhandledError =
        match x with
        | Unhandled _ -> true
        | _ -> false
    member x.ExceptionTerm =
        match x with
        | Unhandled (error, _, _)
        | Caught(error, _, _) -> Some error
        | _ -> None
    member x.StackTrace =
        match x with
        | Unhandled (_, _, s)
        | Caught(_, _, s) -> Some s
        | _ -> None
    static member map f x =
        match x with
        | Unhandled(e, isRuntime, s) -> Unhandled(f e, isRuntime, s)
        | Caught(e, isRuntime, s) -> Caught(f e, isRuntime, s)
        | NoException -> NoException

type exceptionRegisterStack =
    private { stack : exceptionRegister stack }
    member x.GetError() =
        assert(List.isEmpty x.stack |> not)
        let head = List.head x.stack
        head.GetError()
    member x.TransformToCaught() =
        assert(List.isEmpty x.stack |> not)
        match x.stack with
        | head :: tail ->
            { stack = head.TransformToCaught() :: tail }
        | _ -> internalfail "TransformToCaught: exceptionRegisterStack is empty!"
    member x.TransformToUnhandled() =
        assert(List.isEmpty x.stack |> not)
        match x.stack with
        | head :: tail ->
            { stack = head.TransformToUnhandled() :: tail }
        | _ -> internalfail "TransformToUnhandled: exceptionRegisterStack is empty!"
    member x.IsUnhandledError =
        assert(List.isEmpty x.stack |> not)
        let head = List.head x.stack
        head.IsUnhandledError
    member x.ExceptionTerm =
        assert(List.isEmpty x.stack |> not)
        let head = List.head x.stack
        head.ExceptionTerm
    member x.Tail =
        assert(List.isEmpty x.stack |> not)
        let tail = List.tail x.stack
        { stack = tail }
    member x.Size = Stack.size x.stack
    member x.Peek =
        assert(List.isEmpty x.stack |> not)
        List.head x.stack
    member x.Pop() =
        assert(List.isEmpty x.stack |> not)
        match x.stack with
        | head :: tail ->
            head, { stack = tail }
        | _ -> internalfail "Pop: exceptionRegisterStack is empty!"
    member x.Push elem = { stack = Stack.push x.stack elem }
    static member singleton x = { stack = Stack.singleton x }
    static member map f stack = { stack = Stack.map (exceptionRegister.map f) stack.stack }

type arrayCopyInfo =
    {srcAddress : heapAddress; contents : arrayRegion; srcIndex : term; dstIndex : term; length : term; srcSightType : Type; dstSightType : Type} with
        override x.ToString() =
            sprintf "    source address: %O, from %O ranging %O elements into %O index with cast to %O;\n\r    updates: %O" x.srcAddress x.srcIndex x.length x.dstIndex x.dstSightType (MemoryRegion.toString "        " x.contents)

type memoryMode =
    | ConcreteMode
    | SymbolicMode

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
        let value = ref (Nop())
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

and IErrorReporter =
    abstract ConfigureState : state -> unit
    abstract ReportError : string -> term -> bool
    abstract ReportFatalError : string -> term -> bool

and MockingType =
    | Default
    | Extern

and IMethodMock =
    abstract BaseMethod : MethodInfo
    abstract MockingType : MockingType
    abstract Call : state -> term option -> term list -> term option
    abstract GetImplementationClauses : unit -> term array
    abstract GetOutClauses : unit -> term array array
    abstract Copy : unit -> IMethodMock

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
        mutable boxedLocations : pdict<Type, heapRegion>                   // Value types boxed in heap
        mutable initializedTypes : symbolicTypeSet                         // Types with initialized static members
        concreteMemory : IConcreteMemory                                   // Fully concrete objects
        mutable allocatedTypes : pdict<concreteHeapAddress, symbolicType>  // Types of heap locations allocated via new
        mutable initializedAddresses : pset<term>                          // Addresses, which invariants were initialized
        mutable typeVariables : typeVariables                              // Type variables assignment in the current state
        mutable delegates : pdict<concreteHeapAddress, term>               // Subtypes of System.Delegate allocated in heap
        mutable currentTime : vectorTime                                   // Current timestamp (and next allocated address as well) in this state
        mutable startingTime : vectorTime                                  // Timestamp before which all allocated addresses will be considered symbolic
        mutable exceptionsRegister : exceptionRegisterStack                // Heap-address of exception objects, multiple if nested 'try' blocks
        mutable model : model                                              // Concrete valuation of symbolics
        complete : bool                                                    // If true, reading of undefined locations would result in default values
        memoryMode : memoryMode                                            // If 'ConcreteMode', allocating concrete .NET objects inside 'ConcreteMemory'
        methodMocks : IDictionary<IMethod, IMethodMock>
    }
    with override x.ToString() = String.Empty

and IStatedSymbolicConstantSource =
    inherit ISymbolicConstantSource
    abstract Compose : state -> term
