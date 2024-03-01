namespace VSharp.Core

open System
open System.Collections.Generic
open System.Reflection
open System.Text
open VSharp
open VSharp.Core
open VSharp.TypeUtils
open VSharp.Utils

#nowarn "69"

type typeVariables = mappedStack<typeWrapper, Type> * Type list stack

type stackBufferKey = concreteHeapAddress

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

and IMemory =

    abstract EvaluationStack : evaluationStack with get, set

    abstract Stack : callStack

    abstract StackBuffers : pdict<stackKey, stackBufferRegion> with get, set

    abstract ClassFields : pdict<fieldId, heapRegion> with get, set

    abstract Arrays : pdict<arrayType, arrayRegion> with get, set

    abstract Lengths : pdict<arrayType, vectorRegion> with get, set

    abstract LowerBounds : pdict<arrayType, vectorRegion> with get, set

    abstract StaticFields : pdict<fieldId, staticsRegion> with get, set

    abstract BoxedLocations : pdict<Type, heapRegion> with get, set

    abstract ConcreteMemory : ConcreteMemory

    abstract AllocatedTypes : pdict<concreteHeapAddress, symbolicType> with get, set

    abstract InitializedAddresses : pset<term>

    abstract Delegates : pdict<concreteHeapAddress, term>

    abstract MemoryMode : memoryMode with get, set

    abstract NewStackFrame : IMethod option -> (stackKey * term option * Type) list -> unit

    abstract PopFrame : unit -> unit

    abstract ForcePopFrames : int -> unit

    abstract ReadStackLocation : stackKey -> term

    abstract TypeOfHeapLocation : heapAddress -> Type

    abstract MostConcreteTypeOfHeapRef : heapAddress -> Type -> Type

    abstract MostConcreteTypeOfRef : term -> Type

    abstract BaseTypeOfAddress : address -> Type

    abstract ArrayIndicesToOffset : term -> arrayType -> term list -> term

    abstract ReadFieldUnsafe : IErrorReporter -> term -> fieldId -> term

    abstract TryPtrToRef : pointerBase -> Type -> term -> address option

    abstract ReferenceField : term -> fieldId -> term

    abstract ReadLowerBound : term -> term -> arrayType -> term

    abstract ReadLength : term -> term -> arrayType -> term

    abstract ReadArrayIndex : term -> term list -> arrayType -> term

    abstract ReadArrayRange : term -> term list -> term list -> arrayType -> term

    abstract ReadStaticField : Type -> fieldId -> term

    abstract Read : IErrorReporter -> term -> term

    abstract ObjToTerm : Type -> obj -> term

    abstract TryTermToObj : term -> obj option

    abstract TryTermToFullyConcreteObj : term -> obj option

    abstract StringArrayInfo : term -> term option -> term * arrayType

    abstract ReadStruct : IErrorReporter -> term -> fieldId -> term

    abstract MakeSymbolicValue : ISymbolicConstantSource -> string -> Type -> term

    abstract MakeSymbolicThis : IMethod -> term

    abstract FillModelWithParametersAndThis : IMethod -> unit

    abstract AllocateConcreteType : Type -> vectorTime

    abstract AllocateMockType : ITypeMock -> vectorTime

    abstract Unmarshall : concreteHeapAddress -> unit

    abstract InitializeArray : heapAddress -> seq<term list * term> -> arrayType -> unit

    abstract WriteArrayIndex : term -> term list -> arrayType -> term -> unit

    abstract WriteArrayRange : term -> term list -> term list -> arrayType -> term -> unit

    abstract WriteStaticField : Type -> fieldId -> term -> unit

    abstract WriteStackLocation : stackKey -> term -> unit

    abstract WriteClassField : term -> fieldId -> term -> unit

    abstract Write : IErrorReporter -> term -> term -> state

    abstract AllocateOnStack : stackKey -> term -> unit

    abstract AllocateClass : Type -> term

    abstract AllocateArray : Type -> term list -> term list -> term

    abstract AllocateVector : Type -> term -> term

    abstract AllocateConcreteVector<'a> : Type -> term -> seq<'a> -> term

    abstract AllocateEmptyString : term -> term

    abstract AllocateString : string -> term

    abstract CreateStringFromChar : term -> term

    abstract AllocateBoxedLocation : term -> term

    abstract AllocateConcreteObject : obj -> Type -> term

    abstract AllocateTemporaryLocalVariableOfType: string -> int -> Type -> term

    abstract LengthOfString : term -> term

    abstract ReadDelegate : term -> term option

    abstract AllocateDelegate : MethodInfo -> term -> Type -> term

    abstract AllocateCombinedDelegate : concreteHeapAddress -> term list -> Type -> unit

    abstract CombineDelegates : term list -> Type -> term

    abstract RemoveDelegate : term -> term -> Type -> term

    abstract Copy : unit -> IMemory

and
    [<ReferenceEquality>]
    state = {
        mutable pc : pathCondition
        mutable typeStorage : typeStorage
        mutable initializedTypes : symbolicTypeSet                         // Types with initialized static members
        mutable typeVariables : typeVariables                              // Type variables assignment in the current state
        mutable currentTime : vectorTime                                   // Current timestamp (and next allocated address as well) in this state
        mutable startingTime : vectorTime                                  // Timestamp before which all allocated addresses will be considered symbolic
        mutable exceptionsRegister : exceptionRegisterStack                // Heap-address of exception objects, multiple if nested 'try' blocks
        mutable model : model                                              // Concrete valuation of symbolics
        memory : IMemory
        complete : bool                                                    // If true, reading of undefined locations would result in default values
        methodMocks : IDictionary<IMethod, IMethodMock>
    }
    with override x.ToString() = String.Empty

and IStatedSymbolicConstantSource =
    inherit ISymbolicConstantSource
    abstract Compose : state -> term

module internal State =

    let addConstraint (s : state) cond =
        s.pc <- PC.add s.pc cond

// ------------------------------- Types -------------------------------

    let pushTypeVariablesSubstitution state subst =
        assert (subst <> [])
        let oldMappedStack, oldStack = state.typeVariables
        let newStack = subst |> List.unzip |> fst |> Stack.push oldStack
        let newMappedStack = subst |> List.fold (fun acc (k, v) -> MappedStack.push {t=k} v acc) oldMappedStack
        state.typeVariables <- (newMappedStack, newStack)

    let popTypeVariablesSubstitution state =
        let oldMappedStack, oldStack = state.typeVariables
        let toPop, newStack = Stack.pop oldStack
        let newMappedStack = List.fold MappedStack.remove oldMappedStack (List.map (fun t -> {t=t}) toPop)
        state.typeVariables <- (newMappedStack, newStack)

    let commonTypeVariableSubst state (t : Type) noneCase =
        match MappedStack.tryFind {t=t} (fst state.typeVariables) with
        | Some typ -> typ
        | None -> noneCase

    let rec substituteTypeVariables (state : state) typ =
        let substituteTypeVariables = substituteTypeVariables state
        match typ with
        | Bool
        | AddressType
        | Numeric _ -> typ
        | StructType(t, args)
        | ClassType(t, args)
        | InterfaceType(t, args) ->
            let args' = Array.map substituteTypeVariables args
            if args = args' then typ
            else
                t.MakeGenericType args'
        | TypeVariable t -> commonTypeVariableSubst state t typ
        | ArrayType(t, dim) ->
            let t' = substituteTypeVariables t
            if t = t' then typ
            else
                match dim with
                | Vector -> t'.MakeArrayType()
                | ConcreteDimension d -> t'.MakeArrayType(d)
                | SymbolicDimension -> __unreachable__()
        | Pointer t ->
            let t' = substituteTypeVariables t
            if t = t' then typ else t'.MakePointerType()
        | ByRef t ->
            let t' = substituteTypeVariables t
            if t = t' then typ else t'.MakeByRefType()
        | _ -> __unreachable__()

    let substituteTypeVariablesIntoArrayType state ({elemType = et} as arrayType) : arrayType =
        { arrayType with elemType = substituteTypeVariables state et }

    let typeVariableSubst state (t : Type) = commonTypeVariableSubst state t t

    let substituteTypeVariablesIntoField state (f : fieldId) =
        Reflection.concretizeField f (typeVariableSubst state)

    let initializeStaticMembers state typ =
        if typ = typeof<string> then
            let memory = state.memory
            let reference = memory.AllocateString ""
            memory.WriteStaticField typeof<string> Reflection.emptyStringField reference
        state.initializedTypes <- SymbolicSet.add {typ=typ} state.initializedTypes

    let markTypeInitialized state typ =
        state.initializedTypes <- SymbolicSet.add {typ=typ} state.initializedTypes

    let private merge2StatesInternal state1 state2 =
        if state1.memory.Stack <> state2.memory.Stack then None
        else
            // TODO: implement it! See InterpreterBase::interpret::merge
            None

    let merge2States state1 state2 =
        match merge2StatesInternal state1 state2 with
        | Some state -> [state]
        | None -> [state1; state2]

    let merge2Results (term1 : term, state1) (term2, state2) =
        match merge2StatesInternal state1 state2 with
        | Some _ -> __notImplemented__()
        | None -> [(term1, state1); (term2, state2)]

    let mergeStates states =
        // TODO: implement merging by calling merge2StatesInternal one-by-one for each state
        states

    let mergeResults (results : (term * state) list) =
        // TODO
        results

    // ------------------------------- Composition -------------------------------

    let private skipSuffixWhile predicate ys =
        let skipIfNeed y acc k =
            if predicate (y::acc) then k (y::acc)
            else List.take (List.length ys - List.length acc) ys
        Cps.List.foldrk skipIfNeed [] ys (always [])

    let composeTime state time =
        if time = [] then state.currentTime
        elif VectorTime.less VectorTime.zero time |> not then time
        elif state.complete then time
        else state.currentTime @ time

    let rec private fillHole state term =
        match term.term with
        | Constant(_, source, _) ->
            match source with
            | :? IStatedSymbolicConstantSource as source -> source.Compose state
            | :? INonComposableSymbolicConstantSource ->
                match state.model with
                | PrimitiveModel dict ->
                    // Case for model state, so using eval from substitution dict for non composable constants
                    let typ = typeOf term
                    model.EvalDict dict source term typ true
                | _ -> term
            | _ -> internalfail $"fillHole: unexpected term {term}"
        | _ -> term

    and fillHoles state term =
        Substitution.substitute (fillHole state) (substituteTypeVariables state) (composeTime state) term

    let composeRaisedExceptionsOf (state : state) (exceptionRegister : exceptionRegisterStack) =
        let elem, rest = state.exceptionsRegister.Pop()
        match elem with
        | NoException ->
            rest.Push exceptionRegister.Peek
            |> exceptionRegisterStack.map (fillHoles state)
        | _ -> __unreachable__()

    let composeInitializedTypes state initializedTypes =
        let it' = SymbolicSet.map (fun _ -> __unreachable__()) (substituteTypeVariables state) (fun _ -> __unreachable__()) initializedTypes
        SymbolicSet.union state.initializedTypes it'

    [<StructuralEquality;NoComparison>]
    type typeInitialized =
        {typ : Type; matchingTypes : symbolicTypeSet}
        interface IStatedSymbolicConstantSource with
            override x.SubTerms = Seq.empty
            override x.Time = VectorTime.zero
            override x.TypeOfLocation = typeof<bool>

    let isTypeInitialized state (typ : Type) =
        let key : symbolicTypeKey = {typ=typ}
        let matchingTypes = SymbolicSet.matchingElements key state.initializedTypes
        match matchingTypes with
        | [x] when x = key -> True()
        | _ ->
            let name = $"{typ}_initialized"
            let source : typeInitialized = {typ = typ; matchingTypes = SymbolicSet.ofSeq matchingTypes}
            Constant name source typeof<bool>

    type typeInitialized with
        interface IStatedSymbolicConstantSource with
            override x.Compose state =
                let typ = substituteTypeVariables state x.typ
                let newTypes = composeInitializedTypes state x.matchingTypes
                isTypeInitialized {state with initializedTypes = newTypes} typ

    // ------------------------------- Pretty-printing -------------------------------

    let private dumpStack (sb : StringBuilder) stack =
        let stackString = CallStack.toString stack
        if String.IsNullOrEmpty stackString then sb
        else
            let sb = PrettyPrinting.dumpSection "Stack" sb
            PrettyPrinting.appendLine sb stackString

    let private dumpDict section sort keyToString valueToString (sb : StringBuilder) d =
        if PersistentDict.isEmpty d then sb
        else
            let sb = PrettyPrinting.dumpSection section sb
            PersistentDict.dump d sort keyToString valueToString |> PrettyPrinting.appendLine sb

    let private dumpInitializedTypes (sb : StringBuilder) initializedTypes =
        if SymbolicSet.isEmpty initializedTypes then sb
        else sprintf "Initialized types = %s" (SymbolicSet.print initializedTypes) |> PrettyPrinting.appendLine sb

    let private dumpEvaluationStack (sb : StringBuilder) evaluationStack =
        if EvaluationStack.length evaluationStack = 0 then sb
        else
            let sb = PrettyPrinting.dumpSection "Operation stack" sb
            EvaluationStack.toString evaluationStack |> PrettyPrinting.appendLine sb

    let private arrayTypeToString arrayType = (arrayTypeToSymbolicType arrayType).FullName

    let private sortVectorTime<'a> : seq<vectorTime * 'a> -> seq<vectorTime * 'a> =
        Seq.sortWith (fun (k1, _ ) (k2, _ ) -> VectorTime.compare k1 k2)

    let dump (s : state) =
        // TODO: print lower bounds?
        let sortBy sorter = Seq.sortBy (fst >> sorter)
        let memory = s.memory
        let sb = StringBuilder()
        let sb = if PC.isEmpty s.pc then sb else s.pc |> PC.toString |> sprintf "Path condition: %s" |> PrettyPrinting.appendLine sb
        let sb = dumpDict "Fields" (sortBy toString) toString (MemoryRegion.toString "    ") sb memory.ClassFields
        let sb = dumpDict "Array contents" (sortBy arrayTypeToString) arrayTypeToString (MemoryRegion.toString "    ") sb memory.Arrays
        let sb = dumpDict "Array lengths" (sortBy arrayTypeToString) arrayTypeToString (MemoryRegion.toString "    ") sb memory.Lengths
        let sb = dumpDict "Types tokens" sortVectorTime VectorTime.print toString sb memory.AllocatedTypes
        let sb = dumpDict "Static fields" (sortBy toString) toString (MemoryRegion.toString "    ") sb memory.StaticFields
        let sb = dumpDict "Delegates" sortVectorTime VectorTime.print toString sb memory.Delegates
        let sb = dumpStack sb memory.Stack
        let sb = dumpInitializedTypes sb s.initializedTypes
        let sb = dumpEvaluationStack sb memory.EvaluationStack
        if sb.Length = 0 then "<Empty>"
        else
            System.Text.RegularExpressions.Regex.Replace(sb.ToString(), @"@\d+(\+|\-)\d*\[Microsoft.FSharp.Core.Unit\]", "");
