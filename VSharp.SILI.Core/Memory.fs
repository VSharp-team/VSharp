namespace VSharp.Core

open System
open System.Text
open FSharpx.Collections
open VSharp
open VSharp.Core.Types
open VSharp.Core.Types.Constructor
open VSharp.Utils

#nowarn "69"

type stack = mappedStack<stackKey, term>
type entry = { key : stackKey; typ : symbolicType }
type stackFrame = { func : IFunctionIdentifier; entries : entry list; isEffect : bool }
type frames = stackFrame stack // TODO: is it invariant ``there could not be two sequential stackFrames that are effects'' ?

type typeVariables = mappedStack<typeId, symbolicType> * typeId list stack

type stackBufferKey = concreteHeapAddress

type offset = int

// last fields are determined by above fields
[<CustomEquality;CustomComparison>]
type callSite = { sourceMethod : System.Reflection.MethodBase; offset : offset
                  calledMethod : System.Reflection.MethodBase; opCode : System.Reflection.Emit.OpCode }
    with
    member x.HasNonVoidResult = Reflection.GetMethodReturnType x.calledMethod <> typeof<System.Void>
    member x.SymbolicType = x.calledMethod |> Reflection.GetMethodReturnType |> fromDotNetType
    override x.GetHashCode() = (x.sourceMethod, x.offset).GetHashCode()
    override x.Equals(o : obj) =
        match o with
        | :? callSite as other -> x.offset = other.offset && x.sourceMethod = other.sourceMethod
        | _ -> false
    interface System.IComparable with
        override x.CompareTo(other) =
            match other with
            | :? callSite as other when x.sourceMethod.Equals(other.sourceMethod) -> x.offset.CompareTo(other.offset)
            | :? callSite as other -> x.sourceMethod.MetadataToken.CompareTo(other.sourceMethod.MetadataToken)
            | _ -> -1
    override x.ToString() =
        sprintf "sourceMethod = %s\noffset=%x\nopcode=%O\ncalledMethod = %s"
            (Reflection.GetFullMethodName x.sourceMethod) x.offset x.opCode (Reflection.GetFullMethodName x.calledMethod)

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
        | _ -> NoException

type callSiteResults = Map<callSite, term option>

type arrayCopyInfo =
    {srcAddress : heapAddress; contents : arrayRegion; srcIndex : term; dstIndex : term; length : term; dstType : symbolicType} with
        override x.ToString() =
            sprintf "    source address: %O, from %O ranging %O elements into %O index with cast to %O;\n\r    updates: %O" x.srcAddress x.srcIndex x.length x.dstIndex x.dstType (MemoryRegion.toString "        " x.contents)

and state = {
    pc : pathCondition
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
    entireCopies : pdict<concreteHeapAddress, heapAddress * arrayRegion>  // Address and contents of (entirely) copied arrays
    extendedCopies : pdict<concreteHeapAddress, arrayCopyInfo> // Address, contents, source and destination indices and target type of copied arrays
    delegates : pdict<concreteHeapAddress, term>               // Subtypes of System.Delegate allocated in heap
    currentTime : vectorTime                                   // Current timestamp (and next allocated address as well) in this state
    startingTime : vectorTime                                  // Timestamp before which all allocated addresses will be considered symbolic
    returnRegister : term option
    exceptionsRegister : exceptionRegister                     // Heap-address of exception object
    callSiteResults : callSiteResults                          // Computed results of delayed calls
}

type IStatedSymbolicConstantSource =
    inherit ISymbolicConstantSource
    abstract Compose : state -> term

type IMemoryAccessConstantSource =
    inherit IStatedSymbolicConstantSource
    abstract TypeOfLocation : symbolicType

module internal Memory =

// ------------------------------- Primitives -------------------------------

    let empty = {
        pc = PC.empty
        returnRegister = None
        exceptionsRegister = NoException
        callSiteResults = Map.empty
        stack = MappedStack.empty
        stackBuffers = PersistentDict.empty
        frames = Stack.empty
        classFields = PersistentDict.empty
        arrays = PersistentDict.empty
        lengths = PersistentDict.empty
        lowerBounds = PersistentDict.empty
        staticFields = PersistentDict.empty
        boxedLocations = PersistentDict.empty
        initializedTypes = SymbolicSet.empty
        allocatedTypes = PersistentDict.empty
        typeVariables = (MappedStack.empty, Stack.empty)
        entireCopies = PersistentDict.empty
        extendedCopies = PersistentDict.empty
        delegates = PersistentDict.empty
        currentTime = [1u]
        startingTime = VectorTime.zero
    }

    let private isZeroAddress (x : concreteHeapAddress) =
        x = VectorTime.zero
    let composeAddresses (a1 : concreteHeapAddress) (a2 : concreteHeapAddress) : concreteHeapAddress =
        if isZeroAddress a2 then a2 else a1 @ a2

    let withPathCondition (s : state) cond : state = { s with pc = PC.add s.pc cond }
    let removePathCondition (s : state) cond : state = { s with pc = PC.remove s.pc cond }

// ------------------------------- Stack -------------------------------

    let newStackFrame (s : state) funcId frame isEffect : state =
        let pushOne (map : stack) (key, value, typ) =
            match value with
            | Specified term -> { key = key; typ = typ }, MappedStack.push key term map
            | Unspecified -> { key = key; typ = typ }, MappedStack.reserve key map
        let locations, newStack = frame |> List.mapFold pushOne s.stack
        let frames' = Stack.push s.frames { func = funcId; entries = locations; isEffect = isEffect }
        { s with stack = newStack; frames = frames' }

    let pushToCurrentStackFrame (s : state) key value = MappedStack.push key value s.stack
    let popStack (s : state) : state =
        let popOne (map : stack) entry = MappedStack.remove map entry.key
        let entries = (Stack.peek s.frames).entries
        let frames' = Stack.pop s.frames
        { s with stack = List.fold popOne s.stack entries; frames = frames' }

    let inline entriesOfFrame f = f.entries

    let inline private keyOfEntry en = en.key

    let typeOfStackLocation (s : state) key =
        let forMatch = List.tryPick (entriesOfFrame >> List.tryPick (fun { key = l; typ = t } -> if l = key then Some t else None)) s.frames
        match forMatch with
        | Some t -> t
        | None -> internalfailf "stack does not contain key %O!" key

// ------------------------------- Types -------------------------------

    let pushTypeVariablesSubstitution state subst =
        assert (subst <> [])
        let oldMappedStack, oldStack = state.typeVariables
        let newStack = subst |> List.unzip |> fst |> Stack.push oldStack
        let newMappedStack = subst |> List.fold (fun acc (k, v) -> MappedStack.push k v acc) oldMappedStack
        { state with typeVariables = (newMappedStack, newStack) }

    let popTypeVariablesSubstitution state =
        let oldMappedStack, oldStack = state.typeVariables
        let toPop = Stack.peek oldStack
        let newStack = Stack.pop oldStack
        let newMappedStack = List.fold MappedStack.remove oldMappedStack toPop
        { state with typeVariables = (newMappedStack, newStack) }

    let rec substituteTypeVariables (state : state) typ =
        let substituteTypeVariables = substituteTypeVariables state
        let substitute constructor t args = constructor t (List.map substituteTypeVariables args)
        match typ with
        | Void
        | Null
        | Bool
        | AddressType
        | Numeric _ -> typ
        | StructType(t, args) -> substitute Types.StructType t args
        | ClassType(t, args) -> substitute Types.ClassType t args
        | InterfaceType(t, args) -> substitute Types.InterfaceType t args
        | TypeVariable(Id _ as key) ->
            let ms = state.typeVariables |> fst
            if MappedStack.containsKey key ms then MappedStack.find key ms else typ
        | ArrayType(t, dim) -> ArrayType(substituteTypeVariables t, dim)
        | Pointer t -> Pointer(substituteTypeVariables t)

    let private substituteTypeVariablesIntoArrayType state ((et, i, b) : arrayType) : arrayType =
        (substituteTypeVariables state et, i, b)

    let dotNetTypeSubst state (t : System.Type) =
        match MappedStack.tryFind (Id t) (fst state.typeVariables) with
        | Some typ -> typ |> toDotNetType
        | None -> t

    let private substituteTypeVariablesIntoField state (f : fieldId) =
        Reflection.concretizeField f (dotNetTypeSubst state)

    let rec typeOfHeapLocation state (address : heapAddress) =
        match address.term with
        | ConcreteHeapAddress address ->
            if address = VectorTime.zero then Null
            else PersistentDict.find state.allocatedTypes address
        | Constant(_, (:? IMemoryAccessConstantSource as source), AddressType) -> source.TypeOfLocation
        | Union gvs ->
            match gvs |> List.tryPick (fun (_, v) -> let typ = typeOfHeapLocation state v in if typ = Null then None else Some typ) with
            | None -> Null
            | Some result ->
                assert (gvs |> List.forall (fun (_, v) -> let typ = typeOfHeapLocation state v in typ = Null || typ = result))
                result
        | _ -> __unreachable__()

// ------------------------------- Instantiation -------------------------------

    type regionSort =
        | HeapFieldSort of fieldId
        | StaticFieldSort of fieldId
        | ArrayIndexSort of arrayType
        | ArrayLengthSort of arrayType
        | ArrayLowerBoundSort of arrayType
        | StackBufferSort of stackKey
        member x.TypeOfLocation =
            match x with
            | HeapFieldSort field
            | StaticFieldSort field -> field.typ |> fromDotNetType
            | ArrayIndexSort(elementType, _, _)
            | ArrayLengthSort(elementType, _, _)
            | ArrayLowerBoundSort(elementType, _, _) -> elementType
            | StackBufferSort _ -> Numeric typeof<int8>

    [<CustomEquality;NoComparison>]
    type regionPicker<'key, 'reg when 'key : equality and 'key :> IMemoryKey<'key, 'reg> and 'reg : equality and 'reg :> IRegion<'reg>> =
        {sort : regionSort; extract : state -> memoryRegion<'key, 'reg>; mkname : 'key -> string; isDefaultKey : 'key -> bool}
        override x.Equals y =
            match y with
            | :? regionPicker<'key, 'reg> as y -> x.sort = y.sort
            | _ -> false
        override x.GetHashCode() = x.sort.GetHashCode()

    [<StructuralEquality;NoComparison>]
    type private stackReading =
        {key : stackKey; time : vectorTime option}
        interface IMemoryAccessConstantSource  with
            override x.SubTerms = Seq.empty
            override x.Time =
                match x.time with
                | Some time -> time
                | None -> internalfailf "Requesting time of primitive stack location %O" x.key
            override x.TypeOfLocation = x.key.TypeOfLocation

    [<StructuralEquality;NoComparison>]
    type private heapReading<'key, 'reg when 'key : equality and 'key :> IMemoryKey<'key, 'reg> and 'reg : equality and 'reg :> IRegion<'reg>> =
        {picker : regionPicker<'key, 'reg>; key : 'key; memoryObject : memoryRegion<'key, 'reg>; time : vectorTime}
        interface IMemoryAccessConstantSource with
            override x.SubTerms = Seq.empty
            override x.Time = x.time
            override x.TypeOfLocation = x.picker.sort.TypeOfLocation

    [<StructuralEquality;NoComparison>]
    type private structField =
        {baseSource : IMemoryAccessConstantSource; field : fieldId}
        interface IMemoryAccessConstantSource  with
            override x.SubTerms = x.baseSource.SubTerms
            override x.Time = x.baseSource.Time
            override x.TypeOfLocation = x.field.typ |> fromDotNetType

    [<StructuralEquality;NoComparison>]
    type private heapAddressSource =
        {baseSource : IMemoryAccessConstantSource;}
        interface IMemoryAccessConstantSource  with
            override x.SubTerms = x.baseSource.SubTerms
            override x.Time = x.baseSource.Time
            override x.TypeOfLocation = x.baseSource.TypeOfLocation

    [<StructuralEquality;NoComparison>]
    type private typeInitialized =
        {typ : symbolicType; matchingTypes : symbolicTypeSet}
        interface IStatedSymbolicConstantSource  with
            override x.SubTerms = Seq.empty
            override x.Time = VectorTime.zero

    [<StructuralEquality;NoComparison>]
    type private functionResultConstantSource =
        { callSite : callSite; calledTime : vectorTime }
        interface IMemoryAccessConstantSource with
            override x.SubTerms = Seq.empty
            override x.TypeOfLocation = x.callSite.SymbolicType
            override x.Time = x.calledTime

    let private foldFields isStatic folder acc typ =
        let dotNetType = Types.toDotNetType typ
        let fields = Reflection.fieldsOf isStatic dotNetType
        let addField heap (field, typ) =
            let termType = typ |> fromDotNetType
            folder heap field termType
        FSharp.Collections.Array.fold addField acc fields

    let private makeFields isStatic makeField typ =
        let folder fields field termType =
            let value = makeField field termType
            PersistentDict.add field value fields
        foldFields isStatic folder PersistentDict.empty typ

    let private makeStruct isStatic makeField typ =
        let fields = makeFields isStatic makeField typ
        Struct fields typ

    let rec makeSymbolicValue (source : IMemoryAccessConstantSource) name typ =
        match typ with
        | Bool
        | AddressType
        | Numeric _ -> Constant name source typ
        | StructType _ ->
            let makeField field typ =
                let fieldSource = {baseSource = source; field = field}
                makeSymbolicValue fieldSource (toString field) typ
            makeStruct false makeField typ
        | Types.ReferenceType ->
            let addressSource : heapAddressSource = {baseSource = source}
            let address = makeSymbolicValue addressSource name AddressType
            HeapRef address typ
        | Types.ValueType -> __insufficientInformation__ "Can't instantiate symbolic value of unknown value type %O" typ
        | _ -> __insufficientInformation__ "Not sure which value to instantiate, because it's unknown if %O is a reference or a value type" typ

    let private makeSymbolicStackRead key typ time =
        let source = {key = key; time = time}
        let name = toString key
        makeSymbolicValue source name typ

    let private makeSymbolicHeapRead picker key time typ memoryObject =
        let source = {picker = picker; key = key; memoryObject = memoryObject; time = time}
        let name = picker.mkname key
        makeSymbolicValue source name typ

    let makeSymbolicThis (m : System.Reflection.MethodBase) =
        let declaringType = m.DeclaringType |> fromDotNetType
        if Types.isValueType declaringType then __insufficientInformation__ "Can't execute in isolation methods of value types, because we can't be sure where exactly \"this\" is allocated!"
        else HeapRef (Constant "this" {baseSource = {key = ThisKey m; time = Some VectorTime.zero}} AddressType) declaringType

// ------------------------------- Reading -------------------------------

    let private readConcreteDict = PersistentDict.find

    let private accessRegion (dict : pdict<'a, memoryRegion<'key, 'reg>>) key typ =
        match PersistentDict.tryFind dict key with
        | Some value -> value
        | None -> MemoryRegion.empty typ

    let rec extractAddress reference =
        match reference.term with
        | HeapRef(address, _) -> address
        | Union gvs -> gvs |> List.map (fun (g, v) -> (g, extractAddress v)) |> Merging.merge
        | _ -> internalfail "Extracting heap address: expected heap reference, but got %O" reference

    let private isHeapAddressDefault state = term >> function
        | ConcreteHeapAddress addr -> VectorTime.lessOrEqual state.startingTime addr
        | _ -> false

    let readStackLocation (s : state) key =
        match MappedStack.tryFind key s.stack with
        | Some value -> value
        | None -> makeSymbolicStackRead key (typeOfStackLocation s key) (if Types.isValueType key.TypeOfLocation then None else Some s.startingTime)

    let readStruct (structTerm : term) (field : fieldId) =
        match structTerm with
        | { term = Struct(fields, _) } -> fields.[field]
        | _ -> internalfailf "Reading field of structure: expected struct, but got %O" structTerm

    let rec readArrayCopy state arrayType extractor addr indices =
        let emptyReg = MemoryRegion.empty (fst3 arrayType)
        match addr.term with
        | ConcreteHeapAddress concreteAddr ->
            match PersistentDict.tryFind state.entireCopies concreteAddr with
            | None ->
                match PersistentDict.tryFind state.extendedCopies concreteAddr with
                | None -> emptyReg
                | Some copyInfo ->
                    let copiedValue = readArrayRegionExt state arrayType extractor copyInfo.contents copyInfo.srcAddress indices copyInfo.srcIndex copyInfo.dstIndex copyInfo.length copyInfo.dstType
                    let key = {address = addr; indices = indices}
                    MemoryRegion.write emptyReg key copiedValue
            | Some(srcAddr, reg) ->
                let copiedValue = readArrayRegion state arrayType extractor reg srcAddr indices
                let key = {address = addr; indices = indices}
                MemoryRegion.write emptyReg key copiedValue
        | _ -> emptyReg

    and readArrayRegion state arrayType extractor region addr indices =
        let key = {address = addr; indices = indices}
        let isDefault (key : heapArrayIndexKey) = isHeapAddressDefault state key.address
        let instantiate typ memory =
            let copiedMemory = readArrayCopy state arrayType extractor addr indices
            let mkname = fun (key : heapArrayIndexKey) -> sprintf "%O[%s]" key.address (List.map toString key.indices |> join ", ")
            makeSymbolicHeapRead {sort = ArrayIndexSort arrayType; extract = extractor; mkname = mkname; isDefaultKey = isDefault} key state.startingTime typ (MemoryRegion.deterministicCompose copiedMemory memory)
        MemoryRegion.read region key isDefault instantiate

    and readArrayRegionExt state arrayType extractor region addr indices (*copy info follows*) srcIndex dstIndex length dstType =
        __notImplemented__()

    let readArrayIndex state addr indices arrayType =
        let extractor state = accessRegion state.arrays (substituteTypeVariablesIntoArrayType state arrayType) (fst3 arrayType)
        readArrayRegion state arrayType extractor (extractor state) addr indices

    let readClassField state addr (field : fieldId) =
        if field = Reflection.stringFirstCharField then
            readArrayIndex state addr [makeNumber 0] (Char, 1, true)
        else
            let symbolicType = fromDotNetType field.typ
            let extractor state = accessRegion state.classFields (substituteTypeVariablesIntoField state field) (substituteTypeVariables state symbolicType)
            let mkname = fun (key : heapAddressKey) -> sprintf "%O.%O" key.address field
            let isDefault (key : heapAddressKey) = isHeapAddressDefault state key.address
            let key = {address = addr}
            MemoryRegion.read (extractor state) key isDefault
                (makeSymbolicHeapRead {sort = HeapFieldSort field; extract = extractor; mkname = mkname; isDefaultKey = isDefault} key state.startingTime)

    let readStaticField state typ (field : fieldId) =
        let symbolicType = fromDotNetType field.typ
        let extractor state = accessRegion state.staticFields (substituteTypeVariablesIntoField state field) (substituteTypeVariables state symbolicType)
        let mkname = fun (key : symbolicTypeKey) -> sprintf "%O.%O" key.typ field
        let isDefault _ = false // TODO: when statics are allocated? always or never? depends on our exploration strategy
        let key = {typ = typ}
        MemoryRegion.read (extractor state) key isDefault
            (makeSymbolicHeapRead {sort = StaticFieldSort field; extract = extractor; mkname = mkname; isDefaultKey = isDefault} key state.startingTime)

    let readLowerBound state addr dimension arrayType =
        let extractor state = accessRegion state.lowerBounds (substituteTypeVariablesIntoArrayType state arrayType) lengthType
        let mkname = fun (key : heapVectorIndexKey) -> sprintf "LowerBound(%O, %O)" key.address key.index
        let isDefault (key : heapVectorIndexKey) = isHeapAddressDefault state key.address
        let key = {address = addr; index = dimension}
        MemoryRegion.read (extractor state) key isDefault
            (makeSymbolicHeapRead {sort = ArrayLowerBoundSort arrayType; extract = extractor; mkname = mkname; isDefaultKey = isDefault} key state.startingTime)

    let readLength state addr dimension arrayType =
        let extractor state = accessRegion state.lengths (substituteTypeVariablesIntoArrayType state arrayType) lengthType
        let mkname = fun (key : heapVectorIndexKey) -> sprintf "Length(%O, %O)" key.address key.index
        let isDefault (key : heapVectorIndexKey) = isHeapAddressDefault state key.address
        let key = {address = addr; index = dimension}
        MemoryRegion.read (extractor state) key isDefault
            (makeSymbolicHeapRead {sort = ArrayLengthSort arrayType; extract = extractor; mkname = mkname; isDefaultKey = isDefault} key state.startingTime)

    let readStackBuffer state (stackKey : stackKey) index =
        let extractor state = accessRegion state.stackBuffers (stackKey.Map (dotNetTypeSubst state)) (Numeric typeof<int8>)
        let mkname = fun (key : stackBufferIndexKey) -> sprintf "%O[%O]" stackKey key.index
        let isDefault _ = true
        let key = {index = index}
        MemoryRegion.read (extractor state) key isDefault
            (makeSymbolicHeapRead {sort = StackBufferSort stackKey; extract = extractor; mkname = mkname; isDefaultKey = isDefault} key state.startingTime)

    let readBoxedLocation state (addr : concreteHeapAddress) typ =
        match PersistentDict.tryFind state.boxedLocations addr with
        | Some value -> value
        | None -> internalfailf "Boxed location %O [type = %O] was not found in heap: this should not happen!" addr typ

    let rec readDelegate state reference =
        match reference.term with
        | HeapRef(addr, _) ->
            match addr.term with
            | ConcreteHeapAddress addr -> state.delegates.[addr]
            | _ -> __insufficientInformation__ "Can't obtain symbolic delegate %O!" addr
        | Union gvs -> gvs |> List.map (fun (g, v) -> (g, readDelegate state v)) |> Merging.merge
        | _ -> internalfailf "Reading delegate: expected heap reference, but got %O" reference

    let rec readAddress state = function
        | PrimitiveStackLocation key -> readStackLocation state key
        | ClassField(addr, field) -> readClassField state addr field
        | ArrayIndex(addr, indices, typ) -> readArrayIndex state addr indices typ
        | StaticField(typ, field) -> readStaticField state typ field
        | StructField(addr, field) ->
            let structTerm = readAddress state addr
            readStruct structTerm field
        | ArrayLength(addr, dimension, typ) -> readLength state addr dimension typ
        | BoxedLocation(addr, typ) -> readBoxedLocation state addr typ
        | StackBufferIndex(key, index) -> readStackBuffer state key index
        | ArrayLowerBound(addr, dimension, typ) -> readLowerBound state addr dimension typ

    let rec readSafe state reference =
        match reference.term with
        | Ref address -> readAddress state address
        | Ptr (address, _, None) when Option.isSome address -> readAddress state (Option.get address)
        | Union gvs -> gvs |> List.map (fun (g, v) -> (g, readSafe state v)) |> Merging.merge
        | _ -> internalfailf "Safe reading: expected reference, but got %O" reference

    let isTypeInitialized state (typ : symbolicType) =
        let key : symbolicTypeKey = {typ=typ}
        let matchingTypes = SymbolicSet.matchingElements key state.initializedTypes
        match matchingTypes with
        | [x] when x = key -> True
        | _ ->
            let name = sprintf "%O_initialized" typ
            let source : typeInitialized = {typ = typ; matchingTypes = SymbolicSet.ofSeq matchingTypes}
            Constant name source Bool

// ------------------------------- Merging -------------------------------

    let rec findCommonSuffix common pc1 pc2 =
        match pc1, pc2 with
        | [], [] -> [], [], common
        | [], rest2 -> [], rest2, common
        | rest1, [] -> rest1, [], common
        | x :: xs, y :: ys when x = y -> findCommonSuffix (y :: common) xs ys
        | _ -> pc1, pc2, common

    let private merge2StatesInternal state1 state2 =
        if state1.frames <> state2.frames then None
        else
            // TODO: implement it! See InterpreterBase::interpret::merge
            None

    let merge2States state1 state2 =
        match merge2StatesInternal state1 state2 with
        | Some state -> [state]
        | None -> [state1; state2]

    let merge2Results (term1 : term, state1) (term2, state2) =
        match merge2StatesInternal state1 state2 with
        | Some state -> __notImplemented__()
        | None -> [(term1, state1); (term2, state2)]

    let mergeStates states =
        // TODO: implement merging by calling merge2StatesInternal one-by-one for each state
        states

    let mergeResults (results : (term * state) list) =
        // TODO
        results

    let commonGuardedStatedApplyk f state term mergeResults k =
        match term.term with
        | Union gvs ->
            let foldFunc (g, v) k =
                // TODO: this is slow! Instead, rely on PDR engine to throw out reachability facts with unsatisfiable path conditions
                let pc = PC.squashPCWithCondition state.pc g
                match pc with
                | False -> k None
                | _ -> f (withPathCondition state g) v (Some >> k)
            Cps.List.choosek foldFunc gvs (mergeResults >> k)
        | _ -> f state term (List.singleton >> k)
    let guardedStatedApplyk f state term k = commonGuardedStatedApplyk f state term mergeResults k
    let guardedStatedApply f state term = guardedStatedApplyk (Cps.ret2 f) state term id

    let guardedStatedMap mapper state term =
        match term.term with
        | Union gvs -> gvs |> List.map (fun (g, v) -> mapper (withPathCondition state g) v)
        | _ -> [mapper state term]

    let commonStatedConditionalExecutionk (state : state) conditionInvocation thenBranch elseBranch merge2Results k =
        let execution conditionState condition k =
            assert (condition <> True && condition <> False)
            thenBranch (withPathCondition conditionState condition) (fun thenResult ->
            elseBranch (withPathCondition conditionState !!condition) (fun elseResult ->
            merge2Results thenResult elseResult |> k))
        conditionInvocation state (fun (condition, conditionState) ->
        // TODO: this is slow! Instead, rely on PDR engine to throw out reachability facts with unsatisfiable path conditions.
        // TODO: in fact, let the PDR engine decide which branch to pick, i.e. get rid of this function at all
        let thenCondition = PC.squashPCWithCondition conditionState.pc condition
        let elseCondition = PC.squashPCWithCondition conditionState.pc (!!condition)
        match thenCondition, elseCondition with
        | False, _ -> elseBranch conditionState (List.singleton >> k)
        | _, False -> thenBranch conditionState (List.singleton >> k)
        | _ -> execution conditionState condition k)

    let statedConditionalExecutionWithMergek state conditionInvocation thenBranch elseBranch k =
        commonStatedConditionalExecutionk state conditionInvocation thenBranch elseBranch merge2Results k
    let statedConditionalExecutionWithMerge state conditionInvocation thenBranch elseBranch =
        statedConditionalExecutionWithMergek state conditionInvocation thenBranch elseBranch id

// ------------------------------- Writing -------------------------------

    let rec private ensureConcreteType typ =
        if isOpenType typ then __insufficientInformation__ "Cannot write value of generic type %O" typ

    let writeStackLocation (s : state) key value =
        { s with stack = MappedStack.add key value s.stack }

    let writeStruct (structTerm : term) (field : fieldId) value =
        match structTerm with
        | { term = Struct(fields, typ) } -> Struct (PersistentDict.add field value fields) typ
        | _ -> internalfailf "Writing field of structure: expected struct, but got %O" structTerm

    let writeClassField state addr (field : fieldId) value =
        let symbolicType = fromDotNetType field.typ
        ensureConcreteType symbolicType
        let mr = accessRegion state.classFields field symbolicType
        let key = {address = addr}
        let mr' = MemoryRegion.write mr key value
        { state with classFields = PersistentDict.add field mr' state.classFields }

    let writeArrayIndex state addr indices arrayType value =
        let elementType = fst3 arrayType
        ensureConcreteType elementType
        let mr = accessRegion state.arrays arrayType elementType
        let key = {address = addr; indices = indices}
        let mr' = MemoryRegion.write mr key value
        { state with arrays = PersistentDict.add arrayType mr' state.arrays }

    let writeStaticField state typ (field : fieldId) value =
        let symbolicType = fromDotNetType field.typ
        ensureConcreteType symbolicType
        let mr = accessRegion state.staticFields field symbolicType
        let key = {typ = typ}
        let mr' = MemoryRegion.write mr key value
        { state with staticFields = PersistentDict.add field mr' state.staticFields  }

    let writeLowerBound state addr dimension arrayType value =
        ensureConcreteType (fst3 arrayType)
        let mr = accessRegion state.lowerBounds arrayType lengthType
        let key = {address = addr; index = dimension}
        let mr' = MemoryRegion.write mr key value
        { state with lowerBounds = PersistentDict.add arrayType mr' state.lowerBounds }

    let writeLength state addr dimension arrayType value =
        ensureConcreteType (fst3 arrayType)
        let mr = accessRegion state.lengths arrayType lengthType
        let key = {address = addr; index = dimension}
        let mr' = MemoryRegion.write mr key value
        { state with lengths = PersistentDict.add arrayType mr' state.lengths }

    let writeStackBuffer state stackKey index value =
        let mr = accessRegion state.stackBuffers stackKey (Numeric typeof<int8>)
        let key = {index = index}
        let mr' = MemoryRegion.write mr key value
        { state with stackBuffers = PersistentDict.add stackKey mr' state.stackBuffers }

    let writeBoxedLocation state (addr : concreteHeapAddress) value =
        { state with
            boxedLocations = PersistentDict.add addr value state.boxedLocations
            allocatedTypes = PersistentDict.add addr (typeOf value) state.allocatedTypes
        }

    let rec writeAddress state address value =
        match address with
        | PrimitiveStackLocation key -> writeStackLocation state key value
        | ClassField(addr, field) -> writeClassField state addr field value
        | ArrayIndex(addr, indices, typ) -> writeArrayIndex state addr indices typ value
        | StaticField(typ, field) -> writeStaticField state typ field value
        | StructField(addr, field) ->
            let oldStruct = readAddress state addr
            let newStruct = writeStruct oldStruct field value
            writeAddress state addr newStruct
        | ArrayLength(addr, dimension, typ) -> writeLength state addr dimension typ value
        | BoxedLocation(addr, _) -> writeBoxedLocation state addr value
        | StackBufferIndex(key, index) -> writeStackBuffer state key index value
        | ArrayLowerBound(addr, dimension, typ) -> writeLowerBound state addr dimension typ value

    let rec writeSafe state reference value =
        guardedStatedMap
            (fun state reference ->
                match reference.term with
                | Ref address -> writeAddress state address value
                | _ -> internalfailf "Writing: expected reference, but got %O" reference)
            state reference

// ------------------------------- Allocation -------------------------------

    let freshAddress state =
        let state = {state with currentTime = VectorTime.advance state.currentTime}
        state.currentTime, state

    let allocateOnStack state key term =
        let oldFrame = Stack.peek state.frames
        let newStack = pushToCurrentStackFrame state key term
        let newEntries = { key = key; typ = typeOf term }
        let stackFrames = Stack.updateHead state.frames { oldFrame with entries = newEntries :: oldFrame.entries }
        { state with stack = newStack; frames = stackFrames }

    // Strings and delegates should be allocated using the corresponding functions (see allocateString and allocateDelegate)!
    let allocateClass state typ =
        assert (not <| (toDotNetType typ).IsSubclassOf typeof<System.String>)
        assert (not <| (toDotNetType typ).IsSubclassOf typeof<System.Delegate>)
        let concreteAddress, state = freshAddress state
        let address = ConcreteHeapAddress concreteAddress
        let state = {state with allocatedTypes = PersistentDict.add concreteAddress typ state.allocatedTypes}
        HeapRef address typ, state

    let allocateArray state typ lowerBounds lengths =
        let concreteAddress, state = freshAddress state
        let address = ConcreteHeapAddress concreteAddress
        let state = {state with allocatedTypes = PersistentDict.add concreteAddress typ state.allocatedTypes}
        let arrayType = symbolicTypeToArrayType typ
        let state =
            let d = List.length lengths
            assert(d = snd3 arrayType)
            let state = List.fold2 (fun state l i -> writeLength state address (Concrete i lengthType) arrayType l) state lengths [0 .. d-1]
            match lowerBounds with
            | None -> state
            | Some lowerBounds ->
                assert(List.length lowerBounds = d)
                List.fold2 (fun state l i -> writeLowerBound state address (Concrete i lengthType) arrayType l) state lowerBounds [0 .. d-1]
        address, state

    let allocateVector state typ length =
        let address, state = allocateArray state typ None [length]
        HeapRef address typ, state

    let private allocateConcreteVector state elementType length contents =
        let address, state = allocateArray state (ArrayType(elementType, Vector)) None [makeNumber length]
        // TODO: optimize this for large concrete arrays (like images)!
        address, Seq.foldi (fun state i value -> writeArrayIndex state address [Concrete i lengthType] (elementType, 1, true) (Concrete value elementType)) state contents

    let allocateString state (str : string) =
        let address, state = allocateConcreteVector state Char (str.Length + 1) (Seq.append str (Seq.singleton '\000'))
        let heapAddress = getConcreteHeapAddress address
        let state = writeClassField state address Reflection.stringLengthField (Concrete str.Length lengthType)
        HeapRef address Types.String, {state with allocatedTypes = PersistentDict.add heapAddress Types.String state.allocatedTypes}

    let allocateDelegate state delegateTerm =
        let concreteAddress, state = freshAddress state
        let address = ConcreteHeapAddress concreteAddress
        HeapRef address (typeOf delegateTerm), {state with delegates = PersistentDict.add concreteAddress delegateTerm state.delegates}

    let rec lengthOfString state heapRef =
        match heapRef.term with
        | HeapRef(address, typ) ->
            assert(typ = String)
            readClassField state address Reflection.stringLengthField
        | Union gvs -> gvs |> List.map (fun (g, v) -> (g, lengthOfString state v)) |> Merging.merge
        | _ -> internalfail "Getting length of string: expected heap reference, but got %O" heapRef

    let initializeStaticMembers state typ =
        let state =
            if typ = Types.String then
                let reference, state = allocateString state ""
                writeStaticField state Types.String Reflection.emptyStringField reference
            else state
        { state with initializedTypes = SymbolicSet.add {typ=typ} state.initializedTypes }

    let makeFunctionResultConstant time (callSite : callSite) =
        let name = sprintf "FunctionResult(%O)" callSite
        let typ = callSite.SymbolicType
        makeSymbolicValue {callSite = callSite; calledTime = time} name typ

// ------------------------------- Copying -------------------------------

    let copyArray state srcAddress dstAddress =
        let arrayType = typeOfHeapLocation state srcAddress |> symbolicTypeToArrayType
        let contents = MemoryRegion.localizeArray srcAddress (snd3 arrayType) state.arrays.[arrayType]
        {state with entireCopies = PersistentDict.add dstAddress (srcAddress, contents) state.entireCopies}

    let copyArrayExt state srcAddress srcIndices srcType dstAddress dstIndices dstType =
        let arrayType = typeOfHeapLocation state srcAddress |> symbolicTypeToArrayType
        // TODO: implement memmove for multidimensional arrays, i.e. consider the case of overlapping src and dest arrays.
        // TODO: See Array.Copy documentation for detalis.
        __notImplemented__()

    let copyCharArrayToString state arrayAddress dstAddress =
        let arrayType = (Char, 1, true)
        let length = readLength state arrayAddress (makeNumber 0) arrayType
        let lengthPlus1 = Arithmetics.add length (makeNumber 1)
        let state = copyArray state arrayAddress dstAddress
        let dstAddress = ConcreteHeapAddress dstAddress
        let state = writeLength state dstAddress (makeNumber 0) arrayType lengthPlus1
        let state = writeArrayIndex state dstAddress [length] arrayType (Concrete '\000' Char)
        let state = writeClassField state dstAddress Reflection.stringLengthField length
        state

// ------------------------------- Composition -------------------------------

    let private composeTime (state : state) time =
        match state.returnRegister with
        | Some(ConcreteT((:? vectorTime as startingTime), _)) when VectorTime.lessOrEqual time startingTime -> time
        | _ -> composeAddresses state.currentTime time

    let rec private fillHole state term =
        match term.term with
        | Constant(_, source, _) ->
            match source with
            | :? IStatedSymbolicConstantSource as source -> source.Compose state
            | :? INonComposableSymbolicConstantSource -> term
            | _ -> __notImplemented__()
        | _ -> term

    and fillHoles state term =
        Substitution.substitute (fillHole state) (substituteTypeVariables state) (composeTime state) term

    type heapReading<'key, 'reg when 'key : equality and 'key :> IMemoryKey<'key, 'reg> and 'reg : equality and 'reg :> IRegion<'reg>> with
        interface IMemoryAccessConstantSource with
            override x.Compose state =
                // TODO: do nothing if state is empty!
                let substTerm = fillHoles state
                let substType = substituteTypeVariables state
                let substTime = composeTime state
                let key = x.key.Map substTerm substType substTime x.key.Region |> snd
                let effect = MemoryRegion.map substTerm substType substTime x.memoryObject
                let before = x.picker.extract state
                let afters = MemoryRegion.compose before effect
                afters |> List.map (fun (g, after) -> (g, MemoryRegion.read after key x.picker.isDefaultKey (makeSymbolicHeapRead x.picker key state.startingTime))) |> Merging.merge

    type stackReading with
        interface IMemoryAccessConstantSource with
            override x.Compose state =
                let key = x.key.Map (dotNetTypeSubst state)
                readStackLocation state key

    type structField with
        interface IMemoryAccessConstantSource with
            override x.Compose state =
                let structTerm = x.baseSource.Compose state
                readStruct structTerm x.field

    type private heapAddressSource with
        interface IMemoryAccessConstantSource  with
            override x.Compose state =
                x.baseSource.Compose state |> extractAddress

    let private fillAndMutateStackLocation state stack k v =
        let v' = fillHoles state v
        writeStackLocation stack k v'

    let composeRaisedExceptionsOf (state : state) (error : exceptionRegister) =
        error |> exceptionRegister.map (fillHoles state)

    let composeCallSiteResultsOf (state : state) (callSiteResults : callSiteResults) =
        Map.fold (fun (acc : callSiteResults) key value ->
            Prelude.releaseAssert(not <| Map.exists (fun k _ -> k = key) acc)
            match value with
            | None -> acc
            | Some v -> Map.add key (fillHoles state v |> Some) acc
        ) state.callSiteResults callSiteResults

    let private composeStacksAndFramesOf state state' : stack * frames =
        let composeFramesOf state frames' : frames =
            // TODO: do we really need to substitute type variables?
            let frames' = frames' |> List.map (fun f -> {f with entries = f.entries |> List.map (fun e -> {e with typ = substituteTypeVariables state e.typ})})
            List.append frames' state.frames

        let bottomAndRestFrames (s : state) : (stack option * stack * frames) =
            let bottomFrame, restFrames =
                let bottomFrame, restFrames = Stack.bottomAndRest s.frames
                if bottomFrame.isEffect then (Some bottomFrame), restFrames
                else None, s.frames
            let getStackFrame locations =
                let pushOne stack (entry : entry) =
                    match MappedStack.tryFind entry.key s.stack with
                    | Some v -> MappedStack.push entry.key v stack
                    | None -> MappedStack.reserve entry.key stack
                let stack = List.fold pushOne MappedStack.empty locations
                stack
            let bottom = bottomFrame |> Option.map (entriesOfFrame >> getStackFrame)
            let rest = restFrames |> List.collect entriesOfFrame |> getStackFrame
            bottom, rest, restFrames


        let state'Bottom, state'RestStack, state'RestFrames = bottomAndRestFrames state'
        let state2 = Option.fold (MappedStack.fold (fillAndMutateStackLocation state)) state state'Bottom  // apply effect of bottom frame
        let state3 = {state2 with frames = composeFramesOf state2 state'RestFrames}                            // add rest frames
        let finalState = MappedStack.fold (fillAndMutateStackLocation state) state3 state'RestStack                         // fill and copy effect of rest frames
        finalState.stack, finalState.frames
//        // TODO: still, for artificial frames we should mutate it? For instance, consider composition of a state with the effect of a function in the middle of some branch
//        let stack' = MappedStack.map (fun _ -> fillHoles ctx state) state'.stack
//        MappedStack.concat state.stack stack'

    let private fillHolesInMemoryRegion state mr =
        let substTerm = fillHoles state
        let substType = substituteTypeVariables state
        let substTime = composeTime state
        MemoryRegion.map substTerm substType substTime mr

    let private composeMemoryRegions state dict dict' =
        // TODO: somehow get rid of this copy-paste?
        let substTerm = fillHoles state
        let substType = substituteTypeVariables state
        let substTime = composeTime state
        let composeOneRegion dicts k (mr' : memoryRegion<_, _>) =
            list {
                let! (g, dict) = dicts
                let! (g', mr) =
                    let mr =
                        match PersistentDict.tryFind dict k with
                        | Some mr -> mr
                        | None -> MemoryRegion.empty mr'.typ
                    MemoryRegion.compose mr mr' |> List.map (fun (g, mr) -> (g, PersistentDict.add k mr dict))
                return (g &&& g', mr)
            }
        dict'
            |> PersistentDict.map id (MemoryRegion.map substTerm substType substTime)
            |> PersistentDict.fold composeOneRegion [(True, dict)]

    let private composeBoxedLocations state state' =
        state'.boxedLocations |> PersistentDict.fold (fun acc k v -> PersistentDict.add (composeTime state k) (fillHoles state v) acc) state.boxedLocations

    let private composeTypeVariablesOf state state' =
        let (ms, s) = state.typeVariables
        let (ms', s') = state'.typeVariables
        let ms' = MappedStack.map (fun _ v -> substituteTypeVariables state v) ms'
        (MappedStack.concat ms ms', List.append s' s)

    let private composeInitializedTypes state initializedTypes =
        let it' = SymbolicSet.map (fun _ -> __unreachable__()) (substituteTypeVariables state) (fun _ -> __unreachable__()) initializedTypes
        SymbolicSet.union state.initializedTypes it'

    let private composeConcreteDictionaries state dict dict' mapValue =
        dict' |> PersistentDict.fold (fun acc k v ->
                        let k = composeTime state k
                        if (PersistentDict.contains k dict) then
                            assert (PersistentDict.find dict k = mapValue v)
                            acc
                        else PersistentDict.add k (mapValue v) acc
                 ) dict

    let private composeArrayCopyInfo state (addr, reg) =
        let addr = fillHoles state addr
        let reg = fillHolesInMemoryRegion state reg
        (addr, reg)

    let private composeArrayCopyInfoExt state info =
        let srcAddress = fillHoles state info.srcAddress
        let contents = fillHolesInMemoryRegion state info.contents
        let srcIndex = fillHoles state info.srcIndex
        let dstIndex = fillHoles state info.dstIndex
        let length = fillHoles state info.length
        let dstType = substituteTypeVariables state info.dstType
        {srcAddress=srcAddress; contents=contents; srcIndex=srcIndex; dstIndex=dstIndex; length=length; dstType=dstType}

    let composeStates state state' =
        assert(not <| VectorTime.isEmpty state.currentTime)
        // TODO: do nothing if state is empty!
        list {
            let prefix, suffix = List.splitAt (List.length state.currentTime - List.length state'.startingTime) state.currentTime
            let prefix = if VectorTime.lessOrEqual suffix state'.startingTime then prefix else state.currentTime
            // Hacking return register to propagate starting time of state' into composeTime
            let state = {state with currentTime = prefix; returnRegister = Some(Concrete state'.startingTime (fromDotNetType typeof<vectorTime>))}
            let pc = PC.mapPC (fillHoles state) state'.pc |> PC.union state.pc
            let returnRegister = Option.map (fillHoles state) state'.returnRegister
            let exceptionRegister = composeRaisedExceptionsOf state state.exceptionsRegister
            let callSiteResults = composeCallSiteResultsOf state state'.callSiteResults
            let stack, frames = composeStacksAndFramesOf state state'
            let! g1, stackBuffers = composeMemoryRegions state state.stackBuffers state'.stackBuffers
            let! g2, classFields = composeMemoryRegions state state.classFields state'.classFields
            let! g3, arrays = composeMemoryRegions state state.arrays state'.arrays
            let! g4, lengths = composeMemoryRegions state state.lengths state'.lengths
            let! g5, lowerBounds = composeMemoryRegions state state.lowerBounds state'.lowerBounds
            let! g6, staticFields = composeMemoryRegions state state.staticFields state'.staticFields
            let boxedLocations = composeBoxedLocations state state'
            let initializedTypes = composeInitializedTypes state state'.initializedTypes
            let allocatedTypes = composeConcreteDictionaries state state.allocatedTypes state'.allocatedTypes (substituteTypeVariables state)
            let typeVariables = composeTypeVariablesOf state state'
            let entireCopies = composeConcreteDictionaries state state.entireCopies state'.entireCopies (composeArrayCopyInfo state)
            let extendedCopies = composeConcreteDictionaries state state.extendedCopies state'.extendedCopies (composeArrayCopyInfoExt state)
            let delegates = composeConcreteDictionaries state state.delegates state'.delegates id
            let currentTime = prefix @ state'.currentTime
            let g = g1 &&& g2 &&& g3 &&& g4 &&& g5 &&& g6
            if not <| isFalse g then
                return {
                    pc = if isTrue g then pc else PC.add pc g
                    returnRegister = returnRegister
                    exceptionsRegister = exceptionRegister
                    callSiteResults = callSiteResults
                    stack = stack
                    frames = frames
                    stackBuffers = stackBuffers
                    classFields = classFields
                    arrays = arrays
                    lengths = lengths
                    lowerBounds = lowerBounds
                    staticFields = staticFields
                    boxedLocations = boxedLocations
                    initializedTypes = initializedTypes
                    allocatedTypes = allocatedTypes
                    typeVariables = typeVariables
                    entireCopies = entireCopies
                    extendedCopies = extendedCopies
                    delegates = delegates
                    currentTime = currentTime
                    startingTime = state.startingTime
                }
        }

    type typeInitialized with
        interface IStatedSymbolicConstantSource with
            override x.Compose state =
                let typ = substituteTypeVariables state x.typ
                let newTypes = composeInitializedTypes state x.matchingTypes
                isTypeInitialized {state with initializedTypes = newTypes} typ

    type functionResultConstantSource with
        interface IMemoryAccessConstantSource with
            override x.Compose state =
                if Map.containsKey x.callSite state.callSiteResults then
                    let value = state.callSiteResults.[x.callSite]
                    Prelude.releaseAssert (Option.isSome value)
                    Option.get value
                else
                    let newTime = composeAddresses state.currentTime x.calledTime
                    makeFunctionResultConstant newTime x.callSite

// ------------------------------- Pretty-printing -------------------------------

    let private appendLine (sb : StringBuilder) (str : string) =
        sb.Append(str).Append('\n')

    let private dumpSection section (sb : StringBuilder) =
        sprintf "--------------- %s: ---------------" section |> appendLine sb

    let private dumpStack (sb : StringBuilder) stack =
        let print (sb : StringBuilder) k v =
            sprintf "key = %O, value = %O" k v |> appendLine sb
        let sb1 = MappedStack.fold print (StringBuilder()) stack
        if sb1.Length = 0 then sb
        else
            let sb = dumpSection "Stack" sb
            sb.Append(sb1)

    let private dumpDict section keyToString valueToString (sb : StringBuilder) d =
        if PersistentDict.isEmpty d then sb
        else
            let sb = dumpSection section sb
            PersistentDict.dump d keyToString keyToString valueToString |> appendLine sb

    let private arrayTypeToString (elementType, dimension, isVector) =
        if isVector then ArrayType(elementType, Vector)
        else ArrayType(elementType, ConcreteDimension dimension)
        |> toString

    let dump (s : state) =
        // TODO: print stack and lower bounds?
        let sb = StringBuilder()
        let sb = if PC.isEmpty s.pc then sb else s.pc |> PC.mapSeq toString |> Seq.sort |> join " /\ " |> sprintf ("Path condition: %s") |> appendLine sb
        let sb = dumpDict "Fields" toString (MemoryRegion.toString "    ") sb s.classFields
        let sb = dumpDict "Array contents" arrayTypeToString (MemoryRegion.toString "    ") sb s.arrays
        let sb = dumpDict "Array lengths" arrayTypeToString (MemoryRegion.toString "    ") sb s.lengths
        let sb = dumpDict "Boxed items" VectorTime.print toString sb s.boxedLocations
        let sb = dumpDict "Types tokens" VectorTime.print toString sb s.allocatedTypes
        let sb = dumpDict "Static fields" toString (MemoryRegion.toString "    ") sb s.staticFields
        let sb = dumpDict "Array copies" VectorTime.print (fun (addr, mr) -> sprintf "from %O with updates %O" addr (MemoryRegion.toString "    " mr)) sb s.entireCopies
        let sb = dumpDict "Array copies (ext)" VectorTime.print toString sb s.extendedCopies
        let sb = dumpDict "Delegates" VectorTime.print toString sb s.delegates
        let sb = dumpStack sb s.stack
        let sb = if SymbolicSet.isEmpty s.initializedTypes then sb else sprintf "Initialized types = %s" (SymbolicSet.print s.initializedTypes) |> appendLine sb
        if sb.Length = 0 then "<Empty>" else sb.ToString()
