namespace VSharp.Core

open System
open System.Collections.Generic
open System.Reflection
open VSharp
open VSharp.Core.Types.Constructor
open VSharp.Utils

type typeVariables = mappedStack<typeId, symbolicType> * typeId list stack

type stackBufferKey = concreteHeapAddress

type offset = int

[<CustomEquality;NoComparison>]
type physicalAddress = {object : obj}
    with
    override x.GetHashCode() = System.Runtime.CompilerServices.RuntimeHelpers.GetHashCode(x.object)
    override x.Equals(o : obj) =
        match o with
        | :? physicalAddress as other -> x.object.Equals(other.object)
        | _ -> false
    override x.ToString() = PrettyPrinting.printConcrete x.object

type concreteMemory = Dictionary<concreteHeapAddress, physicalAddress>

// last fields are determined by above fields
// TODO: remove it when CFA is gone #Kostya
[<CustomEquality;CustomComparison>]
type callSite = { sourceMethod : System.Reflection.MethodBase; offset : offset
                  calledMethod : System.Reflection.MethodBase; opCode : System.Reflection.Emit.OpCode }
    with
    member x.HasNonVoidResult = Reflection.hasNonVoidResult x.calledMethod
    member x.SymbolicType = x.calledMethod |> Reflection.getMethodReturnType |> fromDotNetType
    override x.GetHashCode() = (x.sourceMethod, x.offset).GetHashCode()
    override x.Equals(o : obj) =
        match o with
        | :? callSite as other -> x.offset = other.offset && x.sourceMethod = other.sourceMethod
        | _ -> false
    interface IComparable with
        override x.CompareTo(other) =
            match other with
            | :? callSite as other when x.sourceMethod.Equals(other.sourceMethod) -> x.offset.CompareTo(other.offset)
            | :? callSite as other -> x.sourceMethod.MetadataToken.CompareTo(other.sourceMethod.MetadataToken)
            | _ -> -1
    override x.ToString() =
        sprintf "sourceMethod = %s\noffset=%x\nopcode=%O\ncalledMethod = %s"
            (Reflection.getFullMethodName x.sourceMethod) x.offset x.opCode (Reflection.getFullMethodName x.calledMethod)

// TODO: is it good idea to add new constructor for recognizing cilStates that construct RuntimeExceptions?
type exceptionRegister =
    | Unhandled of term
    | Caught of term
    | NoException
    with
    member x.GetError () =
        match x with
        | Unhandled error -> error
        | Caught error -> error
        | _ -> internalfail "no error"

    member x.TransformToCaught () =
        match x with
        | Unhandled e -> Caught e
        | _ -> internalfail "unable TransformToCaught"
    member x.TransformToUnhandled () =
        match x with
        | Caught e -> Unhandled e
        | _ -> internalfail "unable TransformToUnhandled"
    member x.UnhandledError =
        match x with
        | Unhandled _ -> true
        | _ -> false
    member x.ExceptionTerm =
        match x with
        | Unhandled error
        | Caught error -> Some error
        | _ -> None
    static member map f x =
        match x with
        | Unhandled e -> Unhandled <| f e
        | Caught e -> Caught <| f e
        | NoException -> NoException

type arrayCopyInfo =
    {srcAddress : heapAddress; contents : arrayRegion; srcIndex : term; dstIndex : term; length : term; srcSightType : symbolicType; dstSightType : symbolicType} with
        override x.ToString() =
            sprintf "    source address: %O, from %O ranging %O elements into %O index with cast to %O;\n\r    updates: %O" x.srcAddress x.srcIndex x.length x.dstIndex x.dstSightType (MemoryRegion.toString "        " x.contents)

type model =
    { state : state; subst : IDictionary<ISymbolicConstantSource, term>; complete : bool }
with
    member x.Complete value =
        if x.complete then
            // TODO: ideally, here should go the full-fledged substitution, but we try to improve the performance a bit...
            match value.term with
            | Constant(_, _, typ) -> makeDefaultValue typ
            | HeapRef({term = Constant _}, _) -> nullRef
            | _ -> value
        else value

    member x.Eval term =
        Substitution.substitute (fun term ->
            match term with
            | { term = Constant(_, (:? IStatedSymbolicConstantSource as source), typ) } ->
                let value = source.Compose x.state
                x.Complete value
            | { term = Constant(_, source, typ) } ->
                let value = ref Nop
                if x.subst.TryGetValue(source, value) then value.Value
                elif x.complete then makeDefaultValue typ
                else term
            | _ -> term) id id term

and
    [<ReferenceEquality>]
    state = {
    id : string
    mutable pc : PC.IPathCondition
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
    concreteMemory : concreteMemory                                    // Fully concrete objects
    mutable physToVirt : pdict<physicalAddress, concreteHeapAddress>   // Map from physical address (obj) to concreteHeapAddress
    mutable allocatedTypes : pdict<concreteHeapAddress, symbolicType>  // Types of heap locations allocated via new
    mutable typeVariables : typeVariables                              // Type variables assignment in the current state
    mutable delegates : pdict<concreteHeapAddress, term>               // Subtypes of System.Delegate allocated in heap
    mutable currentTime : vectorTime                                   // Current timestamp (and next allocated address as well) in this state
    mutable startingTime : vectorTime                                  // Timestamp before which all allocated addresses will be considered symbolic
    mutable exceptionsRegister : exceptionRegister                     // Heap-address of exception object
    mutable model : model option
}

and
    IStatedSymbolicConstantSource =
        inherit ISymbolicConstantSource
        abstract Compose : state -> term
        
module public State =
    let makeEmpty modelState = {
        id = Guid.NewGuid().ToString()
        pc = PC.PathCondition()
        evaluationStack = EvaluationStack.empty
        exceptionsRegister = NoException
        stack = CallStack.empty
        stackBuffers = PersistentDict.empty
        classFields = PersistentDict.empty
        arrays = PersistentDict.empty
        lengths = PersistentDict.empty
        lowerBounds = PersistentDict.empty
        staticFields = PersistentDict.empty
        boxedLocations = PersistentDict.empty
        initializedTypes = SymbolicSet.empty
        concreteMemory = Dictionary<_,_>()
        physToVirt = PersistentDict.empty
        allocatedTypes = PersistentDict.empty
        typeVariables = (MappedStack.empty, Stack.empty)
        delegates = PersistentDict.empty
        currentTime = [1]
        startingTime = VectorTime.zero
        model =
            Option.bind (fun state -> Some {subst = Dictionary<_,_>(); state = state; complete = true}) modelState
    }

