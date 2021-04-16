namespace VSharp.Core

open System
open VSharp
open VSharp.Core.Types.Constructor
open VSharp.Utils

type stack = mappedStack<stackKey, term>
type entry = { key : stackKey; typ : symbolicType }
type stackFrame = { func : IFunctionIdentifier; entries : entry list; isEffect : bool }

// frames of active execution stackTrace;
// that is when searching for exception handler some frames might be temporary dropped
type frames = stackFrame stack // TODO: is it invariant ``there could not be two sequential stackFrames that are effects'' ?

type typeVariables = mappedStack<typeId, symbolicType> * typeId list stack

type stackBufferKey = concreteHeapAddress

type offset = int

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

and state = {
    pc : pathCondition
    opStack : operationStack
    stack : stack                                             // Arguments and local variables
    stackBuffers : pdict<stackKey, stackBufferRegion>         // Buffers allocated via stackAlloc
    frames : frames                                           // Meta-information about stack frames
    classFields : pdict<fieldId, heapRegion>                  // Fields of classes in heap
    arrays : pdict<arrayType, arrayRegion>                    // Contents of arrays in heap
    lengths : pdict<arrayType, vectorRegion>                  // Lengths by dimensions of arrays in heap
    lowerBounds : pdict<arrayType, vectorRegion>              // Lower bounds by dimensions of arrays in heap
    staticFields : pdict<fieldId, staticsRegion>              // Static fields of types without type variables
    boxedLocations : pdict<concreteHeapAddress, term>         // Value types boxed in heap
    initializedTypes : symbolicTypeSet                        // Types with initialized static members
    allocatedTypes : pdict<concreteHeapAddress, symbolicType> // Types of heap locations allocated via new
    typeVariables : typeVariables                             // Type variables assignment in the current state
    delegates : pdict<concreteHeapAddress, term>               // Subtypes of System.Delegate allocated in heap
    currentTime : vectorTime                                   // Current timestamp (and next allocated address as well) in this state
    startingTime : vectorTime                                  // Timestamp before which all allocated addresses will be considered symbolic
    returnRegister : term option // TODO: still need this? - Only for timestamp hack #do delete!
    exceptionsRegister : exceptionRegister                     // Heap-address of exception object
}
