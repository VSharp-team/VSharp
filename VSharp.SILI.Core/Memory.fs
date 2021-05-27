namespace VSharp.Core

open System
open System.Text
open FSharpx.Collections
open VSharp
open VSharp.Core.Types
open VSharp.Core.Types.Constructor
open VSharp.Utils

#nowarn "69"

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
        evaluationStack = EvaluationStack.empty
        returnRegister = None
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
        allocatedTypes = PersistentDict.empty
        typeVariables = (MappedStack.empty, Stack.empty)
        delegates = PersistentDict.empty
        currentTime = [1u]
        startingTime = VectorTime.zero
    }

    let private isZeroAddress (x : concreteHeapAddress) =
        x = VectorTime.zero

    let withPathCondition (s : state) cond : state = { s with pc = PC.add s.pc cond }

// ------------------------------- Stack -------------------------------

    let newStackFrame (s : state) funcId frame : state =
        let stack = CallStack.newStackFrame s.stack funcId frame
        let evaluationStack = EvaluationStack.newStackFrame s.evaluationStack
        { s with stack = stack; evaluationStack = evaluationStack }

    let popFrame (s : state) : state =
        let stack = CallStack.popFrame s.stack
        let evaluationStack = EvaluationStack.popStackFrame s.evaluationStack
        { s with stack = stack; evaluationStack = evaluationStack }

    let typeOfStackLocation (s : state) key = CallStack.typeOfStackLocation s.stack key

// ------------------------------- Types -------------------------------

    let pushTypeVariablesSubstitution state subst =
        assert (subst <> [])
        let oldMappedStack, oldStack = state.typeVariables
        let newStack = subst |> List.unzip |> fst |> Stack.push oldStack
        let newMappedStack = subst |> List.fold (fun acc (k, v) -> MappedStack.push k v acc) oldMappedStack
        { state with typeVariables = (newMappedStack, newStack) }

    let popTypeVariablesSubstitution state =
        let oldMappedStack, oldStack = state.typeVariables
        let toPop, newStack = Stack.pop oldStack
        let newMappedStack = List.fold MappedStack.remove oldMappedStack toPop
        { state with typeVariables = (newMappedStack, newStack) }

    let commonTypeVariableSubst state (t : Type) someCase noneCase =
        match MappedStack.tryFind (Id t) (fst state.typeVariables) with
        | Some typ -> someCase typ
        | None -> noneCase

    let rec substituteTypeVariables (state : state) typ =
        let substituteTypeVariables = substituteTypeVariables state
        let substitute constructor t args = constructor t (List.map substituteTypeVariables args)
        match typ with
        | Void
        | Null
        | Bool
        | AddressType
        | Numeric _ -> typ
        | StructType(t, args) -> substitute StructType t args
        | ClassType(t, args) -> substitute ClassType t args
        | InterfaceType(t, args) -> substitute InterfaceType t args
        | TypeVariable(Id t as key) -> commonTypeVariableSubst state t id typ
        | ArrayType(t, dim) -> ArrayType(substituteTypeVariables t, dim)
        | Pointer t -> Pointer(substituteTypeVariables t)
        | ByRef t -> ByRef(substituteTypeVariables t)

    let private substituteTypeVariablesIntoArrayType state ((et, i, b) : arrayType) : arrayType =
        (substituteTypeVariables state et, i, b)

    let typeVariableSubst state (t : Type) = commonTypeVariableSubst state t toDotNetType t

    let private substituteTypeVariablesIntoField state (f : fieldId) =
        Reflection.concretizeField f (typeVariableSubst state)

    let private typeOfConcreteHeapAddress state address =
        if address = VectorTime.zero then Null
        else PersistentDict.find state.allocatedTypes address

    // TODO: use only mostConcreteTypeOfHeapRef someday
    let rec typeOfHeapLocation state (address : heapAddress) =
        let getTypeOfAddress = term >> function
            | ConcreteHeapAddress address -> typeOfConcreteHeapAddress state address
            | Constant(_, (:? IMemoryAccessConstantSource as source), AddressType) -> source.TypeOfLocation
            | _ -> __unreachable__()
        commonTypeOf getTypeOfAddress address

    let mostConcreteTypeOfHeapRef state address sightType =
        let locationType = typeOfHeapLocation state address
        if isConcreteSubtype locationType sightType then locationType
        else
            assert(isConcreteSubtype sightType locationType)
            sightType

    let baseTypeOfAddress state address =
        match address with
        | BoxedLocation(addr, _) -> typeOfConcreteHeapAddress state addr
        | _ -> typeOfAddress address

// ------------------------------- Instantiation -------------------------------

    [<CustomEquality;NoComparison>]
    type regionPicker<'key, 'reg when 'key : equality and 'key :> IMemoryKey<'key, 'reg> and 'reg : equality and 'reg :> IRegion<'reg>> =
        {sort : regionSort; extract : state -> memoryRegion<'key, 'reg>; mkname : 'key -> string; isDefaultKey : state -> 'key -> bool}
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

    let (|HeapReading|_|) (src : IMemoryAccessConstantSource) =
        match src with
        | :? heapReading<heapAddressKey, vectorTime intervals> as hr -> Some(hr.key, hr.memoryObject)
        | _ -> None

    let (|ArrayIndexReading|_|) (src : IMemoryAccessConstantSource) =
        match src with
        | :? heapReading<heapArrayIndexKey, productRegion<vectorTime intervals, int points listProductRegion>> as ar ->
            Some(isConcreteHeapAddress ar.key.address, ar.key, ar.memoryObject)
        | _ -> None

    // VectorIndexKey is used for length and lower bounds
    // We suppose, that lower bounds will always be default -- 0
    let (|VectorIndexReading|_|) (src : IMemoryAccessConstantSource) =
        let isLowerBoundKey = function
            | ArrayLowerBoundSort _ -> true
            | _ -> false
        match src with
        | :? heapReading<heapVectorIndexKey, productRegion<vectorTime intervals, int points>> as vr ->
            Some(isConcreteHeapAddress vr.key.address || isLowerBoundKey vr.picker.sort, vr.key, vr.memoryObject)
        | _ -> None

    let (|StackBufferReading|_|) (src : IMemoryAccessConstantSource) =
        match src with
        | :? heapReading<stackBufferIndexKey, int points> as sbr -> Some(sbr.key, sbr.memoryObject)
        | _ -> None

    let (|StaticsReading|_|) (src : IMemoryAccessConstantSource) =
        match src with
        | :? heapReading<symbolicTypeKey, freeRegion<symbolicType>> as sr -> Some(sr.key, sr.memoryObject)
        | _ -> None

    let getHeapReadingRegionSort (src : IMemoryAccessConstantSource) =
        match src with
        | :? heapReading<heapAddressKey, vectorTime intervals>                                                 as hr -> hr.picker.sort
        | :? heapReading<heapArrayIndexKey, productRegion<vectorTime intervals, int points listProductRegion>> as hr -> hr.picker.sort
        | :? heapReading<heapVectorIndexKey, productRegion<vectorTime intervals, int points>>                  as hr -> hr.picker.sort
        | :? heapReading<stackBufferIndexKey, int points>                                                      as hr -> hr.picker.sort
        | :? heapReading<symbolicTypeKey, freeRegion<symbolicType>>                                            as hr -> hr.picker.sort
        | _ -> __unreachable__()

    [<StructuralEquality;NoComparison>]
    type private structField =
        {baseSource : IMemoryAccessConstantSource; field : fieldId}
        interface IMemoryAccessConstantSource  with
            override x.SubTerms = x.baseSource.SubTerms
            override x.Time = x.baseSource.Time
            override x.TypeOfLocation = fromDotNetType x.field.typ

    let (|StructFieldSource|_|) (src : IMemoryAccessConstantSource) =
        match src with
        | :? structField as sf -> Some(sf.baseSource, sf.field)
        | _ -> None

    [<StructuralEquality;NoComparison>]
    type private heapAddressSource =
        {baseSource : IMemoryAccessConstantSource;}
        interface IMemoryAccessConstantSource  with
            override x.SubTerms = x.baseSource.SubTerms
            override x.Time = x.baseSource.Time
            override x.TypeOfLocation = x.baseSource.TypeOfLocation

    let (|HeapAddressSource|_|) (src : IMemoryAccessConstantSource) =
        match src with
        | :? heapAddressSource as heapAddress -> Some(heapAddress.baseSource)
        | _ -> None

    [<StructuralEquality;NoComparison>]
    type private typeInitialized =
        {typ : symbolicType; matchingTypes : symbolicTypeSet}
        interface IStatedSymbolicConstantSource  with
            override x.SubTerms = Seq.empty
            override x.Time = VectorTime.zero

    let (|TypeInitializedSource|_|) (src : IStatedSymbolicConstantSource) =
        match src with
        | :? typeInitialized as ti -> Some(ti.typ, ti.matchingTypes)
        | _ -> None

    let private foldFields isStatic folder acc typ =
        let dotNetType = toDotNetType typ
        let fields = Reflection.fieldsOf isStatic dotNetType
        let addField heap (field, typ) =
            let termType = fromDotNetType typ
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
        | ReferenceType ->
            let addressSource : heapAddressSource = {baseSource = source}
            let address = makeSymbolicValue addressSource name AddressType
            HeapRef address typ
        | ValueType -> __insufficientInformation__ "Can't instantiate symbolic value of unknown value type %O" typ
        | ByRef _ -> __insufficientInformation__ "Can't instantiate symbolic value of ByRef type %O" typ
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
        let declaringType = fromDotNetType m.DeclaringType
        if isValueType declaringType then __insufficientInformation__ "Can't execute in isolation methods of value types, because we can't be sure where exactly \"this\" is allocated!"
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

    let isConcreteHeapAddress = term >> function
        | ConcreteHeapAddress _ -> true
        | _ -> false

    let private isHeapAddressDefault state = term >> function
        | ConcreteHeapAddress addr -> VectorTime.less state.startingTime addr
        | _ -> false

    let readStackLocation (s : state) key =
        let makeSymbolic typ =
            let time = if isValueType typ then None else Some s.startingTime
            makeSymbolicStackRead key typ time
        CallStack.readStackLocation s.stack key makeSymbolic

    let readStruct (structTerm : term) (field : fieldId) =
        match structTerm with
        | { term = Struct(fields, _) } -> fields.[field]
        | _ -> internalfailf "Reading field of structure: expected struct, but got %O" structTerm

    let readLowerBound state addr dimension arrayType =
        let extractor state = accessRegion state.lowerBounds (substituteTypeVariablesIntoArrayType state arrayType) lengthType
        let mkname = fun (key : heapVectorIndexKey) -> sprintf "LowerBound(%O, %O)" key.address key.index
        let isDefault state (key : heapVectorIndexKey) = isHeapAddressDefault state key.address || thd3 arrayType
        let key = {address = addr; index = dimension}
        MemoryRegion.read (extractor state) key (isDefault state)
            (makeSymbolicHeapRead {sort = ArrayLowerBoundSort arrayType; extract = extractor; mkname = mkname; isDefaultKey = isDefault} key state.startingTime)

    let readLength state addr dimension arrayType =
        let extractor state = accessRegion state.lengths (substituteTypeVariablesIntoArrayType state arrayType) lengthType
        let mkname = fun (key : heapVectorIndexKey) -> sprintf "Length(%O, %O)" key.address key.index
        let isDefault state (key : heapVectorIndexKey) = isHeapAddressDefault state key.address
        let key = {address = addr; index = dimension}
        MemoryRegion.read (extractor state) key (isDefault state)
            (makeSymbolicHeapRead {sort = ArrayLengthSort arrayType; extract = extractor; mkname = mkname; isDefaultKey = isDefault} key state.startingTime)

    let readArrayRegion state arrayType extractor region addr indices =
        let key = {address = addr; indices = indices}
        let isDefault state (key : heapArrayIndexKey) = isHeapAddressDefault state key.address
        let instantiate typ memory =
            let mkname = fun (key : heapArrayIndexKey) -> sprintf "%O[%s]" key.address (List.map toString key.indices |> join ", ")
            let picker = {sort = ArrayIndexSort arrayType; extract = extractor; mkname = mkname; isDefaultKey = isDefault}
            makeSymbolicHeapRead picker key state.startingTime typ memory
        MemoryRegion.read region key (isDefault state) instantiate

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
            let isDefault state (key : heapAddressKey) = isHeapAddressDefault state key.address
            let key = {address = addr}
            MemoryRegion.read (extractor state) key (isDefault state)
                (makeSymbolicHeapRead {sort = HeapFieldSort field; extract = extractor; mkname = mkname; isDefaultKey = isDefault} key state.startingTime)

    let readStaticField state typ (field : fieldId) =
        let symbolicType = fromDotNetType field.typ
        let extractor state = accessRegion state.staticFields (substituteTypeVariablesIntoField state field) (substituteTypeVariables state symbolicType)
        let mkname = fun (key : symbolicTypeKey) -> sprintf "%O.%O" key.typ field
        let isDefault _ _ = false // TODO: when statics are allocated? always or never? depends on our exploration strategy
        let key = {typ = typ}
        MemoryRegion.read (extractor state) key (isDefault state)
            (makeSymbolicHeapRead {sort = StaticFieldSort field; extract = extractor; mkname = mkname; isDefaultKey = isDefault} key state.startingTime)

    let readStackBuffer state (stackKey : stackKey) index =
        let extractor state = accessRegion state.stackBuffers (stackKey.Map (typeVariableSubst state)) (Numeric typeof<int8>)
        let mkname = fun (key : stackBufferIndexKey) -> sprintf "%O[%O]" stackKey key.index
        let isDefault _ _ = true
        let key = {index = index}
        MemoryRegion.read (extractor state) key (isDefault state)
            (makeSymbolicHeapRead {sort = StackBufferSort stackKey; extract = extractor; mkname = mkname; isDefaultKey = isDefault} key state.startingTime)

    let readBoxedLocation state (addr : concreteHeapAddress) =
        match PersistentDict.tryFind state.boxedLocations addr with
        | Some value -> value
        | None -> internalfailf "Boxed location %O was not found in heap: this should not happen!" addr

    let rec readDelegate state reference =
        match reference.term with
        | HeapRef(addr, _) ->
            match addr.term with
            | ConcreteHeapAddress addr -> state.delegates.[addr]
            | _ -> __insufficientInformation__ "Can't obtain symbolic delegate %O!" addr
        | Union gvs -> gvs |> List.map (fun (g, v) -> (g, readDelegate state v)) |> Merging.merge
        | _ -> internalfailf "Reading delegate: expected heap reference, but got %O" reference

    let rec private readAddress state = function
        | PrimitiveStackLocation key -> readStackLocation state key
        | ClassField(addr, field) -> readClassField state addr field
        | ArrayIndex(addr, indices, typ) -> readArrayIndex state addr indices typ // TODO: what if typ if not the most concrete? #do
        | StaticField(typ, field) -> readStaticField state typ field
        | StructField(addr, field) ->
            let structTerm = readAddress state addr
            readStruct structTerm field
        | ArrayLength(addr, dimension, typ) -> readLength state addr dimension typ
        | BoxedLocation(addr, _) -> readBoxedLocation state addr
        | StackBufferIndex(key, index) -> readStackBuffer state key index
        | ArrayLowerBound(addr, dimension, typ) -> readLowerBound state addr dimension typ

    let rec readSafe state reference =
        match reference.term with
        | Ref address -> readAddress state address
        | Ptr (Some address, viewType, None) when typeOfAddress address = viewType -> readAddress state address
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
        if state1.stack <> state2.stack then None
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

    let commonGuardedStatedApplyk f state term mergeResults k =
        match term.term with
        | Union gvs ->
            let foldFunc (g, v) k =
                // TODO: this is slow! Instead, rely on PDR engine to throw out reachability facts with unsatisfiable path conditions
                let pc = PC.add state.pc g
                if PC.isFalse pc then k None
                else f (withPathCondition state g) v (Some >> k)
            Cps.List.choosek foldFunc gvs (mergeResults >> k)
        | _ -> f state term (List.singleton >> k)
    let guardedStatedApplyk f state term k = commonGuardedStatedApplyk f state term mergeResults k
    let guardedStatedApply f state term = guardedStatedApplyk (Cps.ret2 f) state term id

    let guardedStatedMap mapper state term =
        match term.term with
        | Union gvs -> gvs |> List.map (fun (g, v) -> mapper (withPathCondition state g) v)
        | _ -> [mapper state term]

    let commonStatedConditionalExecutionk (state : state) conditionInvocation thenBranch elseBranch merge2Results k =
        let execution thenState elseState condition k =
            assert (condition <> True && condition <> False)
            thenBranch thenState (fun thenResult ->
            elseBranch elseState (fun elseResult ->
            merge2Results thenResult elseResult |> k))
        conditionInvocation state (fun (condition, conditionState) ->
        // TODO: this is slow! Instead, rely on PDR engine to throw out reachability facts with unsatisfiable path conditions.
        // TODO: in fact, let the PDR engine decide which branch to pick, i.e. get rid of this function at all
        let thenState = withPathCondition conditionState condition
        let elseState = withPathCondition conditionState (!!condition)
        if PC.isFalse thenState.pc then elseBranch conditionState (List.singleton >> k)
        elif PC.isFalse elseState.pc then thenBranch conditionState (List.singleton >> k)
        else
            match SolverInteraction.isValid thenState with
            | SolverInteraction.SmtUnsat _ -> elseBranch conditionState (List.singleton >> k)
            | _ ->
                match SolverInteraction.isValid elseState with
                | SolverInteraction.SmtUnsat _ -> thenBranch conditionState (List.singleton >> k)
                | _ -> execution thenState elseState condition k)

    let statedConditionalExecutionWithMergek state conditionInvocation thenBranch elseBranch k =
        commonStatedConditionalExecutionk state conditionInvocation thenBranch elseBranch merge2Results k
    let statedConditionalExecutionWithMerge state conditionInvocation thenBranch elseBranch =
        statedConditionalExecutionWithMergek state conditionInvocation thenBranch elseBranch id

// ------------------------------- Writing -------------------------------

    let rec private ensureConcreteType typ =
        if isOpenType typ then __insufficientInformation__ "Cannot write value of generic type %O" typ

    let writeStackLocation (s : state) key value =
        { s with stack = CallStack.writeStackLocation s.stack key value }

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
                | Ptr (Some address, viewType, None) when typeOfAddress address = viewType -> writeAddress state address value
                | _ -> internalfailf "Writing: expected reference, but got %O" reference)
            state reference

// ------------------------------- Allocation -------------------------------

    let freshAddress state =
        let state = {state with currentTime = VectorTime.advance state.currentTime}
        state.currentTime, state

    let allocateType state typ =
        let concreteAddress, state = freshAddress state
        assert(not <| PersistentDict.contains concreteAddress state.allocatedTypes)
        ConcreteHeapAddress concreteAddress, {state with allocatedTypes = PersistentDict.add concreteAddress typ state.allocatedTypes}

    let allocateOnStack state key term =
        { state with stack = CallStack.allocate state.stack key term}

    // Strings and delegates should be allocated using the corresponding functions (see allocateString and allocateDelegate)!
    let allocateClass state typ =
        assert (not <| (toDotNetType typ).IsSubclassOf typeof<String>)
        assert (not <| (toDotNetType typ).IsSubclassOf typeof<Delegate>)
        let address, state = allocateType state typ
        HeapRef address typ, state

    let allocateArray state typ lowerBounds lengths =
        let address, state = allocateType state typ
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

    let allocateVector state elementType length =
        let lbs = Some [makeNumber 0]
        let typ = ArrayType(elementType, Vector)
        allocateArray state typ lbs [length]

    let private allocateConcreteVector state elementType length contents =
        let address, state = allocateVector state elementType length
        // TODO: optimize this for large concrete arrays (like images)!
        address, Seq.foldi (fun state i value -> writeArrayIndex state address [Concrete i lengthType] (elementType, 1, true) (Concrete value elementType)) state contents

    let commonAllocateString state length contents =
        let arrayLength = add length (Concrete 1 lengthType)
        let address, state = allocateConcreteVector state Char arrayLength contents
        let state = writeArrayIndex state address [length] (Char, 1, true) (Concrete '\000' Char)
        let heapAddress = getConcreteHeapAddress address
        let state = writeClassField state address Reflection.stringLengthField length
        HeapRef address String, {state with allocatedTypes = PersistentDict.add heapAddress String state.allocatedTypes}

    let allocateEmptyString state length = commonAllocateString state length Seq.empty
    let allocateString state (str : string) = commonAllocateString state (Concrete str.Length lengthType) str

    let allocateDelegate state delegateTerm =
        let concreteAddress, state = freshAddress state
        let address = ConcreteHeapAddress concreteAddress
        let state =
            { state with
                delegates = PersistentDict.add concreteAddress delegateTerm state.delegates
                allocatedTypes = PersistentDict.add concreteAddress (typeOf delegateTerm) state.allocatedTypes
            }
        HeapRef address (typeOf delegateTerm), state

    let rec lengthOfString state heapRef =
        match heapRef.term with
        | HeapRef(address, typ) ->
            assert(typ = String)
            readClassField state address Reflection.stringLengthField
        | Union gvs -> gvs |> List.map (fun (g, v) -> (g, lengthOfString state v)) |> Merging.merge
        | _ -> internalfail "Getting length of string: expected heap reference, but got %O" heapRef

    let initializeStaticMembers state typ =
        let state =
            if typ = String then
                let reference, state = allocateString state ""
                writeStaticField state String Reflection.emptyStringField reference
            else state
        { state with initializedTypes = SymbolicSet.add {typ=typ} state.initializedTypes }

// ------------------------------- Composition -------------------------------

    let private skipSuffixWhile predicate ys =
        let skipIfNeed y acc k =
            if predicate (y::acc) then k (y::acc)
            else List.take (List.length ys - List.length acc) ys
        Cps.List.foldrk skipIfNeed [] ys (always [])

    // compose currentTime:
    // time = 2.1; state.currentTime = 2; result = 2.1 #mbwrong mbresult = 2.2.1

    // addr = 2 => time = 2
    // currentTime = 2; result = 2.2
    let private composeTime state time =
//        let prefix = skipSuffixWhile (fun currentTimeSuffix -> VectorTime.less currentTimeSuffix time) state.currentTime
//        prefix @ time
        state.currentTime @ time

    let private composeConcreteHeapAddress (state : state) addr =
        match state.returnRegister with
        // this is needed only for heapAddressKey.Map when composing
        | _ when VectorTime.isEmpty addr -> state.currentTime
        // address from other block
        | Some(ConcreteT(:? (vectorTime * vectorTime) as interval, _)) when VectorTime.lessOrEqual addr (fst interval) -> addr
        // address of called function
        | Some(ConcreteT(:? (vectorTime * vectorTime) as interval, _)) when VectorTime.isEmpty (snd interval) -> state.currentTime @ addr
        // default case
        | _ -> composeTime state addr

    let rec private fillHole state term =
        match term.term with
        | Constant(_, source, _) ->
            match source with
            | :? IStatedSymbolicConstantSource as source -> source.Compose state
            | :? INonComposableSymbolicConstantSource -> term
            | _ -> __notImplemented__()
        | _ -> term

    and fillHoles state term =
        Substitution.substitute (fillHole state) (substituteTypeVariables state) (composeConcreteHeapAddress state) term

    type heapReading<'key, 'reg when 'key : equality and 'key :> IMemoryKey<'key, 'reg> and 'reg : equality and 'reg :> IRegion<'reg>> with
        interface IMemoryAccessConstantSource with
            override x.Compose state =
                // TODO: do nothing if state is empty!
                let substTerm = fillHoles state
                let substType = substituteTypeVariables state
                let substTime = composeConcreteHeapAddress state
                let key = x.key.Map substTerm substType substTime x.key.Region |> snd
                let effect = MemoryRegion.map substTerm substType substTime x.memoryObject
                let before = x.picker.extract state
                let afters = MemoryRegion.compose before effect
                let read region = MemoryRegion.read region key (x.picker.isDefaultKey state) (makeSymbolicHeapRead x.picker key state.startingTime)
                afters |> List.map (mapsnd read) |> Merging.merge

    type stackReading with
        interface IMemoryAccessConstantSource with
            override x.Compose state =
                let key = x.key.Map (typeVariableSubst state)
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

    // state is untouched. It is needed because of this situation:
    // Effect: x' <- y + 5, y' <- x + 10
    // Left state: x <- 0, y <- 0
    // After composition: {x <- 5, y <- 15} OR {y <- 10, x <- 15}
    // but expected result is {x <- 5, y <- 10}
    let private fillHolesInStack state stack =
        let keyMapper (k : stackKey) = k.Map (typeVariableSubst state)
        CallStack.map keyMapper (fillHoles state) (substituteTypeVariables state) stack

    let composeRaisedExceptionsOf (state : state) (error : exceptionRegister) =
        match state.exceptionsRegister, error with
        | NoException, _ -> error |> exceptionRegister.map (fillHoles state)
        | _ -> __unreachable__()

    let private composeStacksOf state state' : callStack =
        let stack' = fillHolesInStack state state'.stack
        CallStack.applyEffect state.stack stack'

    let private fillHolesInMemoryRegion state mr =
        let substTerm = fillHoles state
        let substType = substituteTypeVariables state
        let substTime = composeConcreteHeapAddress state
        MemoryRegion.map substTerm substType substTime mr

    let private composeMemoryRegions state dict dict' =
        // TODO: somehow get rid of this copy-paste?
        let substTerm = fillHoles state
        let substType = substituteTypeVariables state
        let substTime = composeConcreteHeapAddress state
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
        state'.boxedLocations |> PersistentDict.fold (fun acc k v -> PersistentDict.add (composeConcreteHeapAddress state k) (fillHoles state v) acc) state.boxedLocations

    let private composeTypeVariablesOf state state' =
        let (ms, s) = state.typeVariables
        let (ms', s') = state'.typeVariables
        let ms' = MappedStack.map (fun _ v -> substituteTypeVariables state v) ms'
        (MappedStack.concat ms ms', List.append s' s)

    let private composeInitializedTypes state initializedTypes =
        let it' = SymbolicSet.map (fun _ -> __unreachable__()) (substituteTypeVariables state) (fun _ -> __unreachable__()) initializedTypes
        SymbolicSet.union state.initializedTypes it'

    let private composeConcreteDictionaries state dict dict' mapValue =
        let fillAndMutate acc k v =
            let k = composeConcreteHeapAddress state k
            if (PersistentDict.contains k acc) then
                if (PersistentDict.find acc k = mapValue v) |> not then __unreachable__()
                assert (PersistentDict.find acc k = mapValue v)
                acc
            else PersistentDict.add k (mapValue v) acc
        PersistentDict.fold fillAndMutate dict dict'

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
        let srcSightType = substituteTypeVariables state info.srcSightType
        let dstSightType = substituteTypeVariables state info.dstSightType
        {srcAddress=srcAddress; contents=contents; srcIndex=srcIndex; dstIndex=dstIndex; length=length; dstSightType=dstSightType; srcSightType = srcSightType}

    let composeEvaluationStacksOf state evaluationStack =
        EvaluationStack.map (fillHoles state) evaluationStack

    let composeStates state state' =
        assert(VectorTime.isDescending state.currentTime)
        assert(VectorTime.isDescending state'.currentTime)
        assert(not <| VectorTime.isEmpty state.currentTime)
        // TODO: do nothing if state is empty!
        list {
            // Hacking return register to propagate starting and current time of state' into composeTime
            let state = {state with returnRegister = Some(Concrete (state'.startingTime, state'.currentTime) (fromDotNetType typeof<vectorTime * vectorTime>))}
            let pc = PC.mapPC (fillHoles state) state'.pc |> PC.union state.pc
            // Note: this is not final evaluationStack of resulting cilState, here we forget left state's opStack at all
            let evaluationStack = composeEvaluationStacksOf state state'.evaluationStack
            let returnRegister = Option.map (fillHoles state) state'.returnRegister
            let exceptionRegister = composeRaisedExceptionsOf state state'.exceptionsRegister
            let stack = composeStacksOf state state'
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
            let delegates = composeConcreteDictionaries state state.delegates state'.delegates id
            let currentTime = composeTime state state'.currentTime // TODO: hack #do
//                let computeMax (acc, max) y = // TODO: hack? #do
//                    let acc' = acc @ [y]
//                    let kek = composeTime state acc'
//                    if VectorTime.less max kek then (acc', kek)
//                    else (acc', max)
//                List.fold computeMax ([], []) state'.currentTime |> snd
            let g = g1 &&& g2 &&& g3 &&& g4 &&& g5 &&& g6
            if not <| isFalse g then
                return {
                    pc = if isTrue g then pc else PC.add pc g
                    evaluationStack = evaluationStack
                    returnRegister = returnRegister
                    exceptionsRegister = exceptionRegister
                    stack = stack
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

// ------------------------------- Pretty-printing -------------------------------

    let private dumpStack (sb : StringBuilder) stack =
        let stackString = CallStack.toString stack
        if System.String.IsNullOrEmpty stackString then sb
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

    let private arrayTypeToString (elementType, dimension, isVector) =
        if isVector then ArrayType(elementType, Vector)
        else ArrayType(elementType, ConcreteDimension dimension)
        |> toString

    let private sortVectorTime<'a> : seq<vectorTime * 'a> -> seq<vectorTime * 'a> =
        Seq.sortWith (fun (k1, _ ) (k2, _ ) -> VectorTime.compare k1 k2)

    let dump (s : state) =
        // TODO: print lower bounds?
        let sortBy sorter = Seq.sortBy (fst >> sorter)
        let sb = StringBuilder()
        let sb = if PC.isEmpty s.pc then sb else s.pc |> PC.toString |> sprintf ("Path condition: %s") |> PrettyPrinting.appendLine sb
        let sb = dumpDict "Fields" (sortBy toString) toString (MemoryRegion.toString "    ") sb s.classFields
        let sb = dumpDict "Array contents" (sortBy arrayTypeToString) arrayTypeToString (MemoryRegion.toString "    ") sb s.arrays
        let sb = dumpDict "Array lengths" (sortBy arrayTypeToString) arrayTypeToString (MemoryRegion.toString "    ") sb s.lengths
        let sb = dumpDict "Boxed items" sortVectorTime VectorTime.print toString sb s.boxedLocations
        let sb = dumpDict "Types tokens" sortVectorTime VectorTime.print toString sb s.allocatedTypes
        let sb = dumpDict "Static fields" (sortBy toString) toString (MemoryRegion.toString "    ") sb s.staticFields
        let sb = dumpDict "Delegates" sortVectorTime VectorTime.print toString sb s.delegates
        let sb = dumpStack sb s.stack
        let sb = dumpInitializedTypes sb s.initializedTypes
        let sb = dumpEvaluationStack sb s.evaluationStack
        if sb.Length = 0 then "<Empty>" else sb.ToString()
