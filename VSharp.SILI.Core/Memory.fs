namespace VSharp.Core

open System
open System.Collections.Generic
open System.Reflection
open System.Text
open FSharpx.Collections
open VSharp
open VSharp.Core
open VSharp.TypeUtils
open VSharp.Utils

#nowarn "69"

type IMemoryAccessConstantSource =
    inherit IStatedSymbolicConstantSource

module internal Memory =

// ------------------------------- Primitives -------------------------------

    let makeEmpty complete = {
        pc = PC.empty
        typeStorage = typeStorage()
        evaluationStack = EvaluationStack.empty
        exceptionsRegister = exceptionRegisterStack.singleton NoException
        stack = CallStack.empty
        stackBuffers = PersistentDict.empty
        classFields = PersistentDict.empty
        arrays = PersistentDict.empty
        lengths = PersistentDict.empty
        lowerBounds = PersistentDict.empty
        staticFields = PersistentDict.empty
        boxedLocations = PersistentDict.empty
        initializedTypes = SymbolicSet.empty
        concreteMemory = ConcreteMemory()
        allocatedTypes = PersistentDict.empty
        initializedAddresses = PersistentSet.empty
        typeVariables = (MappedStack.empty, Stack.empty)
        delegates = PersistentDict.empty
        currentTime = [1]
        startingTime = VectorTime.zero
        model = PrimitiveModel (Dictionary())
        complete = complete
        memoryMode = ConcreteMode
        methodMocks = Dictionary()
    }

    let copy (state : state) newPc =
        let cm = state.concreteMemory.Copy()
        let methodMocks = Dictionary()
        for entry in state.methodMocks do
            let method = entry.Key
            let newMock = entry.Value.Copy()
            methodMocks.Add(method, newMock)
        { state with pc = newPc; concreteMemory = cm; methodMocks = methodMocks }

    let private isZeroAddress (x : concreteHeapAddress) =
        x = VectorTime.zero

    let addConstraint (s : state) cond =
        s.pc <- PC.add s.pc cond

    let delinearizeArrayIndex ind lens lbs =
        let detachOne (acc, lens) lb =
            let lensTail = List.tail lens
            let lensProd = List.fold mul (makeNumber 1) lensTail
            let curOffset = div acc lensProd
            let curIndex = add curOffset lb
            let rest = rem acc lensProd
            curIndex, (rest, lensTail)
        List.mapFold detachOne (ind, lens) lbs |> fst

    let linearizeArrayIndex (lens : term list) (lbs : term list) (indices : term list) =
        let length = List.length indices - 1
        let attachOne acc i =
            let relOffset = sub indices[i] lbs[i]
            let prod acc j = mul acc lens[j]
            let lensProd = List.fold prod (makeNumber 1) [i .. length - 1]
            let absOffset = mul relOffset lensProd
            add acc absOffset
        List.fold attachOne (makeNumber 0) [0 .. length]

// -------------------------- Error reporter --------------------------

    type internal EmptyErrorReporter() =
        let report failCondition =
            if failCondition = False() then true
            else internalfail "using 'EmptyErrorReporter'"
        interface IErrorReporter with
            override x.ReportError _ failCondition = report failCondition
            override x.ReportFatalError _ failCondition = report failCondition
            override x.ConfigureState _ = ()

    let emptyReporter = EmptyErrorReporter()

// ------------------------------- Stack -------------------------------

    let newStackFrame (s : state) m frame =
        let stack = CallStack.newStackFrame s.stack m frame
        let evaluationStack = EvaluationStack.newStackFrame s.evaluationStack
        s.stack <- stack
        s.evaluationStack <- evaluationStack

    let popFrame (s : state) =
        let stack = CallStack.popFrame s.stack
        let evaluationStack = EvaluationStack.popStackFrame s.evaluationStack
        s.stack <- stack
        s.evaluationStack <- evaluationStack

    let forcePopFrames (count : int) (s : state) =
        let stack = CallStack.popFrames s.stack count
        let evaluationStack = EvaluationStack.forcePopStackFrames count s.evaluationStack
        s.stack <- stack
        s.evaluationStack <- evaluationStack

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

    let private substituteTypeVariablesIntoArrayType state ((et, i, b) : arrayType) : arrayType =
        (substituteTypeVariables state et, i, b)

    let typeVariableSubst state (t : Type) = commonTypeVariableSubst state t t

    let private substituteTypeVariablesIntoField state (f : fieldId) =
        Reflection.concretizeField f (typeVariableSubst state)

    let private typeOfConcreteHeapAddress state address =
        if address = VectorTime.zero then typeof<obj>
        else
            match PersistentDict.find state.allocatedTypes address with
            | ConcreteType t -> t
            | MockType _ -> __unreachable__() // Mock types may appear only in models

    // TODO: use only mostConcreteTypeOfHeapRef someday
    let rec typeOfHeapLocation state (address : heapAddress) =
        let getTypeOfAddress = term >> function
            | ConcreteHeapAddress address -> typeOfConcreteHeapAddress state address
            | Constant(_, (:? IMemoryAccessConstantSource as source), AddressType) -> source.TypeOfLocation
            | _ -> __unreachable__()
        commonTypeOf getTypeOfAddress address

    let mostConcreteTypeOfHeapRef state address sightType =
        let locationType = typeOfHeapLocation state address
        if isAssignable locationType sightType then locationType
        else
            if isAssignable sightType locationType |> not then
                Logger.trace "mostConcreteTypeOfHeapRef: Sight type (%O) of address %O differs from type in heap (%O)" sightType address locationType
            sightType

    let mostConcreteTypeOfRef state ref =
        let getType ref =
            match ref.term with
            | HeapRef(address, sightType) -> mostConcreteTypeOfHeapRef state address sightType
            | Ref address -> address.TypeOfLocation
            | Ptr(_, t, _) -> t
            | _ -> internalfailf "reading type token: expected heap reference, but got %O" ref
        commonTypeOf getType ref

    let baseTypeOfAddress state address =
        match address with
        | BoxedLocation(address, _) -> typeOfHeapLocation state address
        | _ -> address.TypeOfLocation

// -------------------------------- GetHashCode --------------------------------

    [<StructuralEquality;NoComparison>]
    type private hashCodeSource =
        {object : term}
        interface INonComposableSymbolicConstantSource with
            override x.SubTerms = Seq.empty
            override x.Time = VectorTime.zero
            override x.TypeOfLocation = typeof<int32>

    let (|GetHashCodeSource|_|) (src : ISymbolicConstantSource) =
        match src with
        | :? hashCodeSource as { object = object } -> Some(object)
        | _ -> None

    let hashConcreteAddress (address : concreteHeapAddress) =
        let hashValue = VectorTime.hash address
        hashValue |> makeNumber

    let createHashCodeConstant (term : term) =
        let name = $"HashCode({term})"
        let source = {object = term}
        Constant name source typeof<Int32>

    let getHashCode object =
        // TODO: implement GetHashCode() for value type (it's boxed)
        // TODO: use 'termToObj' for valid hash
        match object.term with
        | ConcreteHeapAddress address
        | HeapRef({term = ConcreteHeapAddress address}, _) -> hashConcreteAddress address
        | HeapRef(address, _) -> createHashCodeConstant address
        | _ -> createHashCodeConstant object

// ------------------------------- Instantiation -------------------------------

    [<CustomEquality;NoComparison>]
    type regionPicker<'key, 'reg when 'key : equality and 'key :> IMemoryKey<'key, 'reg> and 'reg : equality and 'reg :> IRegion<'reg>> =
        {
            sort : regionSort
            extract : state -> memoryRegion<'key, 'reg>
            mkName : 'key -> string
            isDefaultKey : state -> 'key -> bool
            isDefaultRegion : bool
        }
        override x.Equals y =
            match y with
            | :? regionPicker<'key, 'reg> as y -> x.sort = y.sort
            | _ -> false
        override x.GetHashCode() = x.sort.GetHashCode()

    [<StructuralEquality;NoComparison>]
    type private stackReading =
        {key : stackKey; time : vectorTime}
        interface IMemoryAccessConstantSource with
            override x.SubTerms = Seq.empty
            override x.Time = x.time
            override x.TypeOfLocation = x.key.TypeOfLocation

    [<StructuralEquality;NoComparison>]
    type private heapReading<'key, 'reg when 'key : equality and 'key :> IMemoryKey<'key, 'reg> and 'reg : equality and 'reg :> IRegion<'reg>> =
        {picker : regionPicker<'key, 'reg>; key : 'key; memoryObject : memoryRegion<'key, 'reg>; time : vectorTime}
        interface IMemoryAccessConstantSource with
            override x.SubTerms = Seq.empty
            override x.Time = x.time
            override x.TypeOfLocation = x.picker.sort.TypeOfLocation

    [<StructuralEquality;NoComparison>]
    type private arrayReading =
        {
            picker : regionPicker<heapArrayKey, productRegion<vectorTime intervals, int points listProductRegion>>
            key : heapArrayKey
            memoryObject : memoryRegion<heapArrayKey, productRegion<vectorTime intervals, int points listProductRegion>>
            time : vectorTime
        }
        interface IMemoryAccessConstantSource with
            override x.SubTerms = Seq.empty
            override x.Time = x.time
            override x.TypeOfLocation = x.picker.sort.TypeOfLocation

    let (|HeapReading|_|) (src : ISymbolicConstantSource) =
        match src with
        | :? heapReading<heapAddressKey, vectorTime intervals> as hr -> Some(hr.key, hr.memoryObject)
        | _ -> None

    let (|ArrayIndexReading|_|) (src : ISymbolicConstantSource) =
        match src with
        | :? heapReading<heapArrayKey, productRegion<vectorTime intervals, int points listProductRegion>> ->
            internalfail "unexpected array index reading via 'heapReading' source"
        | :? arrayReading as ar -> Some(isConcreteHeapAddress ar.key.Address, ar.key, ar.memoryObject)
        | _ -> None

    let (|ArrayRangeReading|_|) (src : ISymbolicConstantSource) =
        match src with
        | :? arrayReading as { memoryObject = mo; key = RangeArrayIndexKey(address, fromIndices, toIndices); picker = picker; time = time } ->
            Some(mo, address, fromIndices, toIndices, picker, time)
        | _ -> None

    // VectorIndexKey is used for length and lower bounds
    // We suppose, that lower bounds will always be default -- 0
    let (|VectorIndexReading|_|) (src : ISymbolicConstantSource) =
        let isLowerBoundKey = function
            | ArrayLowerBoundSort _ -> true
            | _ -> false
        match src with
        | :? heapReading<heapVectorIndexKey, productRegion<vectorTime intervals, int points>> as vr ->
            Some(isConcreteHeapAddress vr.key.address || isLowerBoundKey vr.picker.sort, vr.key, vr.memoryObject)
        | _ -> None

    let (|StackBufferReading|_|) (src : ISymbolicConstantSource) =
        match src with
        | :? heapReading<stackBufferIndexKey, int points> as sbr -> Some(sbr.key, sbr.memoryObject)
        | _ -> None

    let (|StaticsReading|_|) (src : ISymbolicConstantSource) =
        match src with
        | :? heapReading<symbolicTypeKey, freeRegion<typeWrapper>> as sr -> Some(sr.key, sr.memoryObject)
        | _ -> None

    let getHeapReadingRegionSort (src : ISymbolicConstantSource) =
        match src with
        | :? heapReading<heapAddressKey, vectorTime intervals>                                as hr -> hr.picker.sort
        | :? heapReading<heapVectorIndexKey, productRegion<vectorTime intervals, int points>> as hr -> hr.picker.sort
        | :? heapReading<stackBufferIndexKey, int points>                                     as hr -> hr.picker.sort
        | :? heapReading<symbolicTypeKey, freeRegion<typeWrapper>>                            as hr -> hr.picker.sort
        | :? heapReading<heapArrayKey, productRegion<vectorTime intervals, int points listProductRegion>> ->
            internalfail "unexpected array index reading via 'heapReading' source"
        | :? arrayReading as ar -> ar.picker.sort
        | _ -> __unreachable__()

    [<StructuralEquality;NoComparison>]
    type private structField =
        {baseSource : ISymbolicConstantSource; field : fieldId}
        interface IMemoryAccessConstantSource with
            override x.SubTerms = x.baseSource.SubTerms
            override x.Time = x.baseSource.Time
            override x.TypeOfLocation = x.field.typ

    let (|StructFieldSource|_|) (src : ISymbolicConstantSource) =
        match src with
        | :? structField as sf -> Some(sf.baseSource, sf.field)
        | _ -> None

    [<StructuralEquality;NoComparison>]
    type private heapAddressSource =
        {baseSource : ISymbolicConstantSource}
        interface IMemoryAccessConstantSource with
            override x.SubTerms = x.baseSource.SubTerms
            override x.Time = x.baseSource.Time
            override x.TypeOfLocation = x.baseSource.TypeOfLocation

    let (|HeapAddressSource|_|) (src : ISymbolicConstantSource) =
        match src with
        | :? heapAddressSource as heapAddress -> Some(heapAddress.baseSource)
        | _ -> None

    [<StructuralEquality;NoComparison>]
    type private pointerAddressSource =
        {baseSource : ISymbolicConstantSource; locationType : Type}
        interface IMemoryAccessConstantSource with
            override x.SubTerms = x.baseSource.SubTerms
            override x.Time = x.baseSource.Time
            override x.TypeOfLocation = x.locationType

    let (|PointerAddressSource|_|) (src : ISymbolicConstantSource) =
        match src with
        | :? pointerAddressSource as address -> Some(address.baseSource)
        | _ -> None

    [<StructuralEquality;NoComparison>]
    type private pointerOffsetSource =
        {baseSource : ISymbolicConstantSource}
        interface IMemoryAccessConstantSource with
            override x.SubTerms = x.baseSource.SubTerms
            override x.Time = x.baseSource.Time
            override x.TypeOfLocation = typeof<int>

    let (|PointerOffsetSource|_|) (src : ISymbolicConstantSource) =
        match src with
        | :? pointerOffsetSource as address -> Some(address.baseSource)
        | _ -> None

    [<StructuralEquality;NoComparison>]
    type private typeInitialized =
        {typ : Type; matchingTypes : symbolicTypeSet}
        interface IStatedSymbolicConstantSource  with
            override x.SubTerms = Seq.empty
            override x.Time = VectorTime.zero
            override x.TypeOfLocation = typeof<bool>

    let (|TypeInitializedSource|_|) (src : IStatedSymbolicConstantSource) =
        match src with
        | :? typeInitialized as ti -> Some(ti.typ, ti.matchingTypes)
        | _ -> None

    let rec makeSymbolicValue (source : ISymbolicConstantSource) name typ =
        match typ with
        | Bool
        | AddressType
        | Numeric _ -> Constant name source typ
        | StructType _ ->
            let makeField _ field typ =
                let fieldSource = {baseSource = source; field = field}
                makeSymbolicValue fieldSource $"{name}.{field}" typ
            makeStruct false makeField typ
        | ReferenceType ->
            let addressSource : heapAddressSource = {baseSource = source}
            let address = makeSymbolicValue addressSource name addressType
            HeapRef address typ
        | Pointer t ->
            let locationType =
                if t = typeof<Void> then typeof<byte>.MakeArrayType()
                else t.MakeArrayType()
            let addressSource : pointerAddressSource = {baseSource = source; locationType = locationType}
            let address = makeSymbolicValue addressSource $"address of {name}" addressType
            let offsetSource : pointerOffsetSource = {baseSource = source}
            let offset = makeSymbolicValue offsetSource $"offset of {name}" typeof<int>
            let pointerBase = HeapLocation(address, locationType)
            Ptr pointerBase t offset
        | ValueType -> __insufficientInformation__ $"Can't instantiate symbolic value of unknown value type {typ}"
        | ByRef _ -> __insufficientInformation__ $"Can't instantiate symbolic value of ByRef type {typ}"
        | _ -> __insufficientInformation__ $"Not sure which value to instantiate, because it's unknown if {typ} is a reference or a value type"

    let rec extractAddress reference =
        match reference.term with
        | HeapRef(address, _) -> address
        | Ptr(HeapLocation(address, _), _, _) -> address
        | Union gvs -> Merging.guardedMap extractAddress gvs
        | _ -> internalfail $"Extracting heap address: expected heap reference or pointer, but got {reference}"

    let rec extractPointerOffset ptr =
        match ptr.term with
        | Ptr(_, _, offset) -> offset
        | Ref address -> Pointers.addressToBaseAndOffset address |> snd
        | HeapRef _ -> makeNumber 0
        | Union gvs -> Merging.guardedMap extractPointerOffset gvs
        | _ -> internalfail $"Extracting pointer offset: expected reference or pointer, but got {ptr}"

    let specializeWithKey constant (key : heapArrayKey) (writeKey : heapArrayKey) =
        match constant.term with
        | HeapRef({term = Constant(_, HeapAddressSource(ArrayRangeReading(mo, srcAddress, srcFrom, srcTo, picker, time)), typ)}, _)
        | Constant(_, ArrayRangeReading(mo, srcAddress, srcFrom, srcTo, picker, time), typ) ->
            let key = key.Specialize writeKey srcAddress srcFrom srcTo
            let source : arrayReading = {picker = picker; key = key; memoryObject = mo; time = time}
            let name = picker.mkName key
            Constant name source typ
        | _ -> constant

    let private makeSymbolicStackRead key typ time =
        let source = {key = key; time = time}
        let name = toString key
        makeSymbolicValue source name typ

    let rec private makeSymbolicHeapRead _ picker key time typ memoryObject _ =
        let source : heapReading<'key, 'reg> = {picker = picker; key = key; memoryObject = memoryObject; time = time}
        let name = picker.mkName key
        makeSymbolicValue source name typ

    let rec private makeArraySymbolicHeapRead state picker (key : heapArrayKey) time typ memoryObject (singleValue : updateTreeKey<heapArrayKey, term> option) =
        match singleValue with
        | Some {key = key'; value = {term = HeapRef({term = Constant(_, HeapAddressSource(ArrayRangeReading(mo, srcA, srcF, srcT, p, _)), _)}, _)}}
        | Some {key = key'; value = {term = Constant(_, ArrayRangeReading(mo, srcA, srcF, srcT, p, _), _)}} when key'.Includes key ->
            let key = key.Specialize key' srcA srcF srcT
            let inst = makeArraySymbolicHeapRead state p key state.startingTime
            MemoryRegion.read mo key (p.isDefaultKey state) inst
        | Some { key = key'; value = value } when key'.Includes key -> value
        | _ ->
            let source : arrayReading = {picker = picker; key = key; memoryObject = memoryObject; time = time}
            let name = picker.mkName key
            makeSymbolicValue source name typ

    // This function is used only for creating 'this' of reference types
    let makeSymbolicThis (m : IMethod) =
        let declaringType = m.DeclaringType
        assert(isValueType declaringType |> not)
        let source : heapAddressSource = {baseSource = {key = ThisKey m; time = VectorTime.zero}}
        let address = Constant "this" source addressType
        HeapRef address declaringType

    let fillModelWithParametersAndThis state (method : IMethod) =
        let parameters = method.Parameters |> Seq.map (fun param ->
            (ParameterKey param, None, param.ParameterType)) |> List.ofSeq
        let parametersAndThis =
            if method.HasThis then
                let t = method.DeclaringType
                if not t.IsValueType then
                    let thisRef = HeapRef (ConcreteHeapAddress [-1]) t
                    state.startingTime <- [-2]
                    (ThisKey method, Some thisRef, t) :: parameters
                else (ThisKey method, None, t) :: parameters
            else parameters
        newStackFrame state None parametersAndThis

// -------------------------- Allocation helpers --------------------------

    let private freshAddress state =
        state.currentTime <- VectorTime.advance state.currentTime
        state.currentTime

    let private allocateType state symbolicType =
        let concreteAddress = freshAddress state
        assert(not <| PersistentDict.contains concreteAddress state.allocatedTypes)
        state.allocatedTypes <- PersistentDict.add concreteAddress symbolicType state.allocatedTypes
        concreteAddress

    let allocateConcreteType state (typ : Type) =
        assert(not typ.IsAbstract || isDelegate typ)
        allocateType state (ConcreteType typ)

    let allocateMockType state mock =
        allocateType state (MockType mock)


// =============== Marshalling/unmarshalling without state changing ===============

    // ------------------ Object to term ------------------

    let private allocateObjectIfNeed state (obj : obj) t =
        assert(state.memoryMode = ConcreteMode)
        let cm = state.concreteMemory
        let address =
            match cm.TryPhysToVirt obj with
            | Some address -> address
            | None when obj = null -> VectorTime.zero
            | None ->
                let typ = mostConcreteType (obj.GetType()) t
                let concreteAddress = allocateConcreteType state typ
                cm.Allocate concreteAddress obj
                concreteAddress
        ConcreteHeapAddress address

    let private referenceTypeToTerm state (obj : obj) t =
        let address = allocateObjectIfNeed state obj t
        let objType = typeOfHeapLocation state address
        HeapRef address objType

    let rec objToTerm (state : state) (t : Type) (obj : obj) =
        match obj with
        | _ when isNullable t -> nullableToTerm state t obj
        | null -> nullRef t
        | :? bool as b -> makeBool b
        | _ when isNumeric t -> makeNumber obj
        // TODO: need pointer?
        | _ when isPointer t -> Concrete obj t
        | _ when t.IsValueType -> structToTerm state obj t
        | _ -> referenceTypeToTerm state obj t

    and private structToTerm state (obj : obj) t =
        let makeField (fieldInfo : FieldInfo) _ _ =
           fieldInfo.GetValue(obj) |> objToTerm state fieldInfo.FieldType
        makeStruct false makeField t

    and private nullableToTerm state t (obj : obj) =
        let nullableType = Nullable.GetUnderlyingType t
        let valueField, hasValueField = Reflection.fieldsOfNullable t
        let value, hasValue =
            if box obj <> null then objToTerm state nullableType obj, True()
            else objToTerm state nullableType (Reflection.createObject nullableType), False()
        let fields = PersistentDict.ofSeq <| seq [(valueField, value); (hasValueField, hasValue)]
        Struct fields t

    // ---------------- Try term to object ----------------

    let tryAddressToObj (state : state) address =
        if address = VectorTime.zero then Some null
        else state.concreteMemory.TryVirtToPhys address

    let tryPointerToObj state address (offset : int) =
        let cm = state.concreteMemory
        match cm.TryVirtToPhys address with
        | Some obj ->
            let gch = Runtime.InteropServices.GCHandle.Alloc(obj, Runtime.InteropServices.GCHandleType.Pinned)
            let pObj = gch.AddrOfPinnedObject() + (nativeint offset)
            Some (pObj :> obj)
        | None -> None

    let rec tryTermToObj (state : state) term =
        match term.term with
        | ConcreteDelegate _
        | CombinedDelegate _ -> None
        | Concrete(obj, _) -> Some obj
        | Struct(fields, typ) when isNullable typ -> tryNullableTermToObj state fields typ
        | Struct(fields, typ) when not typ.IsByRefLike -> tryStructTermToObj state fields typ
        | HeapRef({term = ConcreteHeapAddress a}, _) -> tryAddressToObj state a
        | Ptr(HeapLocation({term = ConcreteHeapAddress a}, _), _, ConcreteT (:? int as offset, _)) ->
            tryPointerToObj state a offset
        | _ -> None

    and private castAndSet (fieldInfo : FieldInfo) structObj v =
        let v =
            if v <> null && v.GetType() <> fieldInfo.FieldType && fieldInfo.FieldType = typeof<IntPtr> then
                let gcHandle = System.Runtime.InteropServices.GCHandle.Alloc(v)
                System.Runtime.InteropServices.GCHandle.ToIntPtr(gcHandle) :> obj
            else v
        fieldInfo.SetValue(structObj, v)

    and tryStructTermToObj (state : state) fields typ =
        let structObj = Reflection.createObject typ
        let addField _ (fieldId, value) k =
            let fieldInfo = Reflection.getFieldInfo fieldId
            // field was not found in the structure, skipping it
            if fieldInfo = null then k ()
            else
                match tryTermToObj state value with
                // field can be converted to obj, so continue
                | Some v -> castAndSet fieldInfo structObj v |> k
                // field can not be converted to obj, so break and return None
                | None -> None
        Cps.Seq.foldlk addField () (PersistentDict.toSeq fields) (fun _ -> Some structObj)

    and tryNullableTermToObj (state : state) fields typ =
        let valueField, hasValueField = Reflection.fieldsOfNullable typ
        let value = PersistentDict.find fields valueField
        let hasValue = PersistentDict.find fields hasValueField
        match tryTermToObj state value with
        | Some obj when hasValue = True() -> Some obj
        | _ when hasValue = False() -> Some null
        | _ -> None

    let tryTermListToObjects state (terms : term list) =
        let toObj (t : term) acc k =
            match tryTermToObj state t with
            | Some o -> o :: acc |> k
            | None -> None
        Cps.List.foldrk toObj List.empty terms Some

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

// ------------------------------- Safe reading -------------------------------

    let private accessRegion (dict : pdict<'a, memoryRegion<'key, 'reg>>) key typ =
        match PersistentDict.tryFind dict key with
        | Some value -> value
        | None -> MemoryRegion.empty typ

    let isConcreteHeapAddress = term >> function
        | ConcreteHeapAddress _ -> true
        | _ -> false

    let private isHeapAddressDefault state address =
        state.complete ||
        match address.term with
        | ConcreteHeapAddress address -> VectorTime.less state.startingTime address
        | _ -> false

    let readStackLocation (s : state) key =
        let makeSymbolic typ =
            if s.complete then makeDefaultValue typ
            else makeSymbolicStackRead key typ s.startingTime
        CallStack.readStackLocation s.stack key makeSymbolic

    let private readLowerBoundSymbolic (state : state) address dimension arrayType =
        let extractor (state : state) = accessRegion state.lowerBounds (substituteTypeVariablesIntoArrayType state arrayType) lengthType
        let mkName (key : heapVectorIndexKey) = $"LowerBound({key.address}, {key.index})"
        let isDefault state (key : heapVectorIndexKey) = isHeapAddressDefault state key.address || thd3 arrayType
        let key = {address = address; index = dimension}
        let inst typ memoryRegion =
            let sort = ArrayLowerBoundSort arrayType
            let picker =
                {
                    sort = sort; extract = extractor; mkName = mkName
                    isDefaultKey = isDefault; isDefaultRegion = false
                }
            makeSymbolicHeapRead state picker key state.startingTime typ memoryRegion
        MemoryRegion.read (extractor state) key (isDefault state) inst

    let readLowerBound state address dimension arrayType =
        let cm = state.concreteMemory
        match address.term, dimension.term with
        | ConcreteHeapAddress address, Concrete(:? int as dim, _) when cm.Contains address ->
            cm.ReadArrayLowerBound address dim |> objToTerm state typeof<int>
        | _ -> readLowerBoundSymbolic state address dimension arrayType

    let private readLengthSymbolic state address dimension arrayType =
        let extractor (state : state) = accessRegion state.lengths (substituteTypeVariablesIntoArrayType state arrayType) lengthType
        let mkName = fun (key : heapVectorIndexKey) -> $"Length({key.address}, {key.index})"
        let isDefault state (key : heapVectorIndexKey) = isHeapAddressDefault state key.address
        let key = {address = address; index = dimension}
        let inst typ memoryRegion =
            let sort = ArrayLengthSort arrayType
            let picker =
                {
                    sort = sort; extract = extractor; mkName = mkName
                    isDefaultKey = isDefault; isDefaultRegion = false
                }
            makeSymbolicHeapRead state picker key state.startingTime typ memoryRegion
        MemoryRegion.read (extractor state) key (isDefault state) inst

    let readLength state address dimension arrayType =
        let cm = state.concreteMemory
        match address.term, dimension.term with
        | ConcreteHeapAddress address, Concrete(:? int as dim, _) when cm.Contains address ->
            cm.ReadArrayLength address dim |> objToTerm state typeof<int>
        | _ -> readLengthSymbolic state address dimension arrayType

    let private readArrayRegion state arrayType extractor region (isDefaultRegion : bool) (key : heapArrayKey) =
        let isDefault state (key : heapArrayKey) = isHeapAddressDefault state key.Address
        let instantiate typ memory singleValue =
            let sort = ArrayIndexSort arrayType
            let picker =
                {
                    sort = sort; extract = extractor; mkName = toString
                    isDefaultKey = isDefault; isDefaultRegion = isDefaultRegion
                }
            let time =
                if isValueType typ then state.startingTime
                else MemoryRegion.maxTime region.updates state.startingTime
            makeArraySymbolicHeapRead state picker key time typ memory singleValue
        MemoryRegion.read region key (isDefault state) instantiate

    let private readArrayKeySymbolic state key arrayType =
        let extractor state =
            let arrayType = substituteTypeVariablesIntoArrayType state arrayType
            accessRegion state.arrays arrayType (fst3 arrayType)
        readArrayRegion state arrayType extractor (extractor state) false key

    let private readArrayIndexSymbolic state address indices arrayType =
        let indices = List.map (fun i -> primitiveCast i typeof<int>) indices
        let key = OneArrayIndexKey(address, indices)
        readArrayKeySymbolic state key arrayType

    let private readArrayRangeSymbolic state address fromIndices toIndices arrayType =
        let fromIndices = List.map (fun i -> primitiveCast i typeof<int>) fromIndices
        let toIndices = List.map (fun i -> primitiveCast i typeof<int>) toIndices
        let key = RangeArrayIndexKey(address, fromIndices, toIndices)
        readArrayKeySymbolic state key arrayType

    let private arrayRegionMemsetData state concreteAddress data regionType region =
        let address = ConcreteHeapAddress concreteAddress
        let prepareData (index, value) =
            let key = OneArrayIndexKey(address, List.map (int >> makeNumber) index)
            let value = objToTerm state regionType value
            key, value
        Seq.map prepareData data |> MemoryRegion.memset region

    let private arrayRegionFromData state concreteAddress data regionType =
        let region = MemoryRegion.empty regionType
        arrayRegionMemsetData state concreteAddress data regionType region

    let private readRangeFromConcreteArray state concreteAddress arrayData fromIndices toIndices arrayType =
        let address = ConcreteHeapAddress concreteAddress
        let fromIndices = List.map (fun i -> primitiveCast i typeof<int>) fromIndices
        let toIndices = List.map (fun i -> primitiveCast i typeof<int>) toIndices
        let region = arrayRegionFromData state concreteAddress arrayData (fst3 arrayType)
        let key = RangeArrayIndexKey(address, fromIndices, toIndices)
        readArrayRegion state arrayType (always region) region true key

    let readArrayRange state address fromIndices toIndices arrayType =
        let cm = state.concreteMemory
        match address.term with
        | ConcreteHeapAddress concreteAddress when cm.Contains concreteAddress ->
            let data = cm.GetAllArrayData concreteAddress
            readRangeFromConcreteArray state concreteAddress data fromIndices toIndices arrayType
        | _ -> readArrayRangeSymbolic state address fromIndices toIndices arrayType

    let private readSymbolicIndexFromConcreteArray state concreteAddress arrayData indices arrayType =
        let address = ConcreteHeapAddress concreteAddress
        let region = arrayRegionFromData state concreteAddress arrayData (fst3 arrayType)
        let indices = List.map (fun i -> primitiveCast i typeof<int>) indices
        let key = OneArrayIndexKey(address, indices)
        readArrayRegion state arrayType (always region) region true key

    let readArrayIndex state address indices arrayType =
        let cm = state.concreteMemory
        let concreteIndices = tryIntListFromTermList indices
        match address.term, concreteIndices with
        | ConcreteHeapAddress address, Some concreteIndices when cm.Contains address ->
            cm.ReadArrayIndex address concreteIndices |> objToTerm state (fst3 arrayType)
        | ConcreteHeapAddress concreteAddress, None when cm.Contains concreteAddress ->
            let data = cm.GetAllArrayData concreteAddress
            readSymbolicIndexFromConcreteArray state concreteAddress data indices arrayType
        | _ -> readArrayIndexSymbolic state address indices arrayType

// ------------------------------- Array writing -------------------------------

    let private ensureConcreteType typ =
        if isOpenType typ then __insufficientInformation__ $"Cannot write value of generic type {typ}"

    let private writeLowerBoundSymbolic (state : state) address dimension arrayType value =
        ensureConcreteType (fst3 arrayType)
        let mr = accessRegion state.lowerBounds arrayType lengthType
        let key = {address = address; index = dimension}
        let mr' = MemoryRegion.write mr key value
        state.lowerBounds <- PersistentDict.add arrayType mr' state.lowerBounds

    let writeLengthSymbolic (state : state) address dimension arrayType value =
        ensureConcreteType (fst3 arrayType)
        let mr = accessRegion state.lengths arrayType lengthType
        let key = {address = address; index = dimension}
        let mr' = MemoryRegion.write mr key value
        state.lengths <- PersistentDict.add arrayType mr' state.lengths

    let private writeArrayKeySymbolic state key arrayType value =
        let elementType = fst3 arrayType
        ensureConcreteType elementType
        let mr = accessRegion state.arrays arrayType elementType
        let mr' = MemoryRegion.write mr key value
        let newArrays = PersistentDict.add arrayType mr' state.arrays
        state.arrays <- newArrays

    let private writeArrayIndexSymbolic state address indices arrayType value =
        let indices = List.map (fun i -> primitiveCast i typeof<int>) indices
        let key = OneArrayIndexKey(address, indices)
        writeArrayKeySymbolic state key arrayType value

// ------------------------------- Safe reading -------------------------------

    let commonReadClassFieldSymbolic state address (field : fieldId) =
        let symbolicType = field.typ
        let extractor state =
            let field = substituteTypeVariablesIntoField state field
            let typ = substituteTypeVariables state symbolicType
            accessRegion state.classFields field typ
        let region = extractor state
        let mkName = fun (key : heapAddressKey) -> $"{key.address}.{field}"
        let isDefault state (key : heapAddressKey) = isHeapAddressDefault state key.address
        let key = {address = address}
        let instantiate typ memory =
            let sort = HeapFieldSort field
            let picker =
                {
                    sort = sort; extract = extractor; mkName = mkName
                    isDefaultKey = isDefault; isDefaultRegion = false
                }
            let time =
                if isValueType typ then state.startingTime
                else MemoryRegion.maxTime region.updates state.startingTime
            makeSymbolicHeapRead state picker key time typ memory
        MemoryRegion.read region key (isDefault state) instantiate

    let stringArrayInfo state stringAddress length =
        let arrayType = typeof<char>, 1, true
        if PersistentSet.contains stringAddress state.initializedAddresses then
            stringAddress, arrayType
        else
            state.initializedAddresses <- PersistentSet.add state.initializedAddresses stringAddress
            match stringAddress.term with
            | ConcreteHeapAddress cha when state.concreteMemory.Contains cha -> stringAddress, arrayType
            | _ ->
                let zero = makeNumber 0
                let stringLength =
                    match length with
                    | Some len -> len
                    | None -> commonReadClassFieldSymbolic state stringAddress Reflection.stringLengthField
                let greaterZero = simplifyGreaterOrEqual stringLength zero id
                addConstraint state greaterZero
                let arrayLength = add stringLength (makeNumber 1)
                writeLengthSymbolic state stringAddress zero arrayType arrayLength
                writeLowerBoundSymbolic state stringAddress zero arrayType zero
                let zeroChar = makeNumber '\000'
                writeArrayIndexSymbolic state stringAddress [stringLength] arrayType zeroChar
                stringAddress, arrayType

    let private readClassFieldSymbolic state address (field : fieldId) =
        if field = Reflection.stringFirstCharField then
            let arrayAddress, arrayType = stringArrayInfo state address None
            readArrayIndexSymbolic state arrayAddress [makeNumber 0] arrayType
        else commonReadClassFieldSymbolic state address field

    let readClassField (state : state) address (field : fieldId) =
        let cm = state.concreteMemory
        match address.term with
        | ConcreteHeapAddress address when cm.Contains address ->
            cm.ReadClassField address field |> objToTerm state field.typ
        | _ -> readClassFieldSymbolic state address field

    let readStaticField state typ (field : fieldId) =
        let extractor state = accessRegion state.staticFields (substituteTypeVariablesIntoField state field) (substituteTypeVariables state field.typ)
        let mkName = fun (key : symbolicTypeKey) -> $"{key.typ}.{field}"
        let isDefault state _ = state.complete // TODO: when statics are allocated? always or never? depends on our exploration strategy
        let key = {typ = typ}
        let inst typ memoryRegion =
            let sort = StaticFieldSort field
            let picker =
                {
                    sort = sort; extract = extractor; mkName = mkName
                    isDefaultKey = isDefault; isDefaultRegion = false
                }
            makeSymbolicHeapRead state picker key state.startingTime typ memoryRegion
        MemoryRegion.read (extractor state) key (isDefault state) inst

    let readStackBuffer state (stackKey : stackKey) index =
        let extractor state = accessRegion state.stackBuffers (stackKey.Map (typeVariableSubst state)) typeof<int8>
        let mkName (key : stackBufferIndexKey) = $"{stackKey}[{key.index}]"
        let isDefault _ _ = true
        let key : stackBufferIndexKey = {index = index}
        let inst typ memoryRegion =
            let sort = StackBufferSort stackKey
            let picker =
                {
                    sort = sort; extract = extractor; mkName = mkName
                    isDefaultKey = isDefault; isDefaultRegion = false
                }
            makeSymbolicHeapRead state picker key state.startingTime typ memoryRegion
        MemoryRegion.read (extractor state) key (isDefault state) inst

    let private readBoxedSymbolic state address typ =
        let extractor state = accessRegion state.boxedLocations typ typ
        let region = extractor state
        let mkName (key : heapAddressKey) = $"boxed {key.address} of {typ}"
        let isDefault state (key : heapAddressKey) = isHeapAddressDefault state key.address
        let key = {address = address}
        let instantiate typ memory =
            let sort = BoxedSort typ
            let picker =
                {
                    sort = sort; extract = extractor; mkName = mkName
                    isDefaultKey = isDefault; isDefaultRegion = false
                }
            let time = state.startingTime
            makeSymbolicHeapRead state picker key time typ memory
        MemoryRegion.read region key (isDefault state) instantiate

    let readBoxedLocation state (address : term) sightType =
        assert(isBoxedType sightType)
        let cm = state.concreteMemory
        let typeFromMemory = typeOfHeapLocation state address
        let typ = mostConcreteType typeFromMemory sightType
        match state.memoryMode, address.term with
        | ConcreteMode, ConcreteHeapAddress address when cm.Contains address ->
            let value = cm.VirtToPhys address
            objToTerm state typ value
        | _ -> readBoxedSymbolic state address typ

    let rec readStruct reporter state (structTerm : term) (field : fieldId) =
        match structTerm.term with
        | Struct(fields, _) -> fields[field]
        | Combined _ -> readFieldUnsafe reporter state structTerm field
        | _ -> internalfail $"Reading field of structure: expected struct, but got {structTerm}"

    and private readSafe reporter state = function
        | PrimitiveStackLocation key -> readStackLocation state key
        | ClassField(address, field) -> readClassField state address field
        // [NOTE] ref must be the most concrete, otherwise region will be not found
        | ArrayIndex(address, indices, typ) -> readArrayIndex state address indices typ
        | StaticField(typ, field) -> readStaticField state typ field
        | StructField(address, field) ->
            let structTerm = readSafe reporter state address
            readStruct reporter state structTerm field
        | ArrayLength(address, dimension, typ) -> readLength state address dimension typ
        | BoxedLocation(address, typ) -> readBoxedLocation state address typ
        | StackBufferIndex(key, index) -> readStackBuffer state key index
        | ArrayLowerBound(address, dimension, typ) -> readLowerBound state address dimension typ

// ------------------------------- Unsafe reading -------------------------------

    and private checkBlockBounds (reporter : IErrorReporter) blockSize startByte endByte =
        let zero = makeNumber 0
        let failCondition =
            simplifyLess startByte zero id
            ||| simplifyGreaterOrEqual startByte blockSize id
            ||| simplifyLessOrEqual endByte zero id
            ||| simplifyGreater endByte blockSize id
        reporter.ReportFatalError "reading out of block bounds" failCondition

    and private readAddressUnsafe (reporter : IErrorReporter) address startByte endByte =
        let size = sizeOf address
        match startByte.term, endByte.term with
        | Concrete(:? int as s, _), Concrete(:? int as e, _) when s = 0 && size = e -> List.singleton address
        | _ ->
            let failCondition =
                simplifyGreater startByte (makeNumber 0) id
                ||| simplifyLess endByte (makeNumber size) id
            let isAlive = reporter.ReportFatalError "address reinterpretation" failCondition
            if isAlive then List.singleton address
            else List.empty

    and sliceTerm term startByte endByte pos stablePos =
        match term.term with
        | Slice(term, cuts) when stablePos ->
            assert(List.isEmpty cuts |> not)
            let _, _, p = List.head cuts
            createSlice term ((startByte, endByte, p) :: cuts)
        | Slice(term, cuts) ->
            assert(List.isEmpty cuts |> not)
            let _, _, p = List.head cuts
            createSlice term ((startByte, endByte, add pos p) :: cuts)
        | _ -> createSlice term (List.singleton (startByte, endByte, pos))

    // NOTE: returns list of slices
    // TODO: return empty if every slice is invalid
    and private commonReadTermUnsafe (reporter : IErrorReporter) term startByte endByte pos stablePos sightType =
        match term.term, sightType with
        | Slice _, _ ->
            sliceTerm term startByte endByte pos stablePos |> List.singleton
        | _, Some sightType when
            startByte = makeNumber 0 &&
            let typ = typeOf term
            let size = internalSizeOf typ
            endByte = makeNumber size && typ = sightType ->
                List.singleton term
        | Struct(fields, t), _ -> commonReadStructUnsafe reporter fields t startByte endByte pos stablePos sightType
        | HeapRef _, _
        | Ref _, _
        | Ptr _, _ -> readAddressUnsafe reporter term startByte endByte
        | Combined([t], _), _ -> commonReadTermUnsafe reporter t startByte endByte pos stablePos sightType
        | Combined(slices, _), _ ->
            let readSlice part = commonReadTermUnsafe reporter part startByte endByte pos stablePos sightType
            List.collect readSlice slices
        | Concrete _, _
        | Constant _, _
        | Expression _, _ ->
            sliceTerm term startByte endByte pos stablePos |> List.singleton
        | _ -> internalfailf $"readTermUnsafe: unexpected term {term}"

    and private readTermUnsafe reporter term startByte endByte sightType =
        commonReadTermUnsafe reporter term startByte endByte (neg startByte) false sightType

    and private readTermPartUnsafe reporter term startByte endByte sightType =
        commonReadTermUnsafe reporter term startByte endByte startByte true sightType

    and private commonReadStructUnsafe reporter fields structType startByte endByte pos stablePos sightType =
        let readField fieldId = fields[fieldId]
        commonReadFieldsUnsafe reporter readField false structType startByte endByte pos stablePos sightType

    and private readStructUnsafe reporter fields structType startByte endByte sightType =
        commonReadStructUnsafe reporter fields structType startByte endByte (neg startByte) false sightType

    and private getAffectedFields reporter readField isStatic (blockType : Type) startByte endByte =
        // TODO: incorrect in case of static field
        let blockSize = Reflection.blockSize blockType
        let inBlock =
            isValueType blockType
            || checkBlockBounds reporter (makeNumber blockSize) startByte endByte
        if inBlock then
            let fields = Reflection.fieldsOf isStatic blockType
            let getOffsetAndSize (fieldId, fieldInfo : FieldInfo) =
                fieldId, Reflection.getFieldOffset fieldInfo, internalSizeOf fieldInfo.FieldType
            let fieldIntervals = Array.map getOffsetAndSize fields |> Array.sortBy snd3
            let betweenField = {name = ""; declaringType = blockType; typ = typeof<byte>}
            let addZerosBetween (_, offset, size as field) (allFields, nextOffset) =
                let curEnd = offset + size
                let between = nextOffset - curEnd
                // TODO: add there is enough space, insert short, int or long
                let zeros = if between > 0 then List.init between (fun i -> betweenField, curEnd + i, 1) else List.empty
                let fieldsWithZeros = List.foldBack (fun zero fields -> zero :: fields) zeros allFields
                field :: fieldsWithZeros, offset
            let fieldsWithZeros, fstOffset = Array.foldBack addZerosBetween fieldIntervals (List.empty, blockSize)
            let zeros = if fstOffset > 0 then List.init fstOffset (fun i -> betweenField, i, 1) else List.empty
            let allFields = List.foldBack (fun zero fields -> zero :: fields) zeros fieldsWithZeros
            let readFieldOrZero fieldId =
                if fieldId.name = "" then makeDefaultValue fieldId.typ
                else readField fieldId
            let getField (fieldId, fieldOffset, _) =
                let fieldValue = readFieldOrZero fieldId
                let fieldOffset = makeNumber fieldOffset
                let startByte = sub startByte fieldOffset
                let endByte = sub endByte fieldOffset
                fieldId, fieldOffset, fieldValue, startByte, endByte
            match startByte.term, endByte.term with
            | Concrete(:? int as s, _), Concrete(:? int as e, _) ->
                let concreteGetField (_, fieldOffset, fieldSize as field) affectedFields =
                    if (e > fieldOffset && s < fieldOffset + fieldSize) then
                        getField field :: affectedFields
                    else affectedFields
                List.foldBack concreteGetField allFields List.empty
            | _ -> List.map getField allFields
        else List.empty

    and private commonReadFieldsUnsafe reporter readField isStatic (blockType : Type) startByte endByte pos stablePos sightType =
        let affectedFields = getAffectedFields reporter readField isStatic blockType startByte endByte
        let readField (_, o, v, s, e) =
            commonReadTermUnsafe reporter v s e (add pos o) stablePos sightType
        List.collect readField affectedFields

    and private readFieldsUnsafe reporter readField isStatic (blockType : Type) startByte endByte sightType =
        commonReadFieldsUnsafe reporter readField isStatic blockType startByte endByte (neg startByte) false sightType

    // TODO: Add undefined behaviour:
    // TODO: 1. when reading info between fields
    // TODO: 3. when reading info outside block
    // TODO: 3. reinterpreting ref or ptr should return symbolic ref or ptr
    and private readClassUnsafe reporter state address classType offset (viewSize : int) sightType =
        let endByte = makeNumber viewSize |> add offset
        let readField fieldId = readClassField state address fieldId
        readFieldsUnsafe reporter readField false classType offset endByte sightType

    and arrayIndicesToOffset state address (elementType, dim, _ as arrayType) indices =
        let lens = List.init dim (fun dim -> readLength state address (makeNumber dim) arrayType)
        let lbs = List.init dim (fun dim -> readLowerBound state address (makeNumber dim) arrayType)
        let linearIndex = linearizeArrayIndex lens lbs indices
        mul linearIndex (internalSizeOf elementType |> makeNumber)

    and private getAffectedIndices reporter state address (elementType, dim, _ as arrayType) offset viewSize =
        let concreteElementSize = internalSizeOf elementType
        let elementSize = makeNumber concreteElementSize
        let lens = List.init dim (fun dim -> readLength state address (makeNumber dim) arrayType)
        let lbs = List.init dim (fun dim -> readLowerBound state address (makeNumber dim) arrayType)
        let arraySize = List.fold mul elementSize lens
        let inBlock = checkBlockBounds reporter arraySize offset (makeNumber viewSize |> add offset)
        if inBlock then
            let firstElement = div offset elementSize
            let elementOffset = rem offset elementSize
            let countToRead =
                match elementOffset.term with
                | Concrete(:? int as i, _) when (i + viewSize) % concreteElementSize = 0 -> (i + viewSize) / concreteElementSize
                // NOTE: if offset inside element > 0 then one more element is needed
                | _ -> (viewSize / concreteElementSize) + 1
            let getElement currentOffset i =
                let linearIndex = makeNumber i |> add firstElement
                let indices = delinearizeArrayIndex linearIndex lens lbs
                let element = readArrayIndex state address indices arrayType
                let startByte = sub offset currentOffset
                let endByte = makeNumber viewSize |> add startByte
                (indices, element, startByte, endByte), add currentOffset elementSize
            List.mapFold getElement (mul firstElement elementSize) [0 .. countToRead - 1] |> fst
        else List.empty

    and private readArrayUnsafe reporter state address arrayType offset viewSize sightType =
        let indices = getAffectedIndices reporter state address (symbolicTypeToArrayType arrayType) offset viewSize
        let readIndex (_, elem, s, e) =
            readTermUnsafe reporter elem s e sightType
        List.collect readIndex indices

    and private readStringUnsafe reporter state address offset viewSize sightType =
         // TODO: handle case, when reading string length
        let address, arrayType = stringArrayInfo state address None
        let indices = getAffectedIndices reporter state address arrayType offset viewSize
        let readChar (_, elem, s, e) =
            readTermUnsafe reporter elem s e sightType
        List.collect readChar indices

    and private readStaticUnsafe reporter state t offset (viewSize : int) sightType =
        let endByte = makeNumber viewSize |> add offset
        let readField fieldId = readStaticField state t fieldId
        readFieldsUnsafe reporter readField true t offset endByte sightType

    and private readStackUnsafe reporter state loc offset (viewSize : int) sightType =
        let term = readStackLocation state loc
        let locSize = sizeOf term |> makeNumber
        let endByte = makeNumber viewSize |> add offset
        let inBlock = checkBlockBounds reporter locSize offset endByte
        if inBlock then readTermUnsafe reporter term offset endByte sightType
        else List.empty

    and private readBoxedUnsafe reporter state loc typ offset viewSize sightType =
        let address = BoxedLocation(loc, typ)
        let endByte = makeNumber viewSize |> add offset
        match readSafe reporter state address with
        | {term = Struct(fields, _)} -> readStructUnsafe reporter fields typ offset endByte sightType
        | term when isPrimitive typ || typ.IsEnum -> readTermUnsafe reporter term offset endByte sightType
        | term -> internalfail $"readUnsafe: reading struct resulted in term {term}"

    and private readUnsafe reporter state baseAddress offset sightType =
        let viewSize = internalSizeOf sightType
        let slices =
            let sightType = Some sightType
            match baseAddress with
            | HeapLocation(loc, t) ->
                let typ = mostConcreteTypeOfHeapRef state loc t
                match typ with
                | StringType -> readStringUnsafe reporter state loc offset viewSize sightType
                | ClassType _ -> readClassUnsafe reporter state loc typ offset viewSize sightType
                | ArrayType _ -> readArrayUnsafe reporter state loc typ offset viewSize sightType
                | _ when typ = typeof<Void> -> internalfail $"readUnsafe: reading from 'Void' by reference {baseAddress}"
                | StructType _ -> readBoxedUnsafe reporter state loc typ offset viewSize sightType
                | _ when isPrimitive typ || typ.IsEnum ->
                    readBoxedUnsafe reporter state loc typ offset viewSize sightType
                | _ -> internalfailf $"Expected complex type, but got {typ}"
            | StackLocation loc -> readStackUnsafe reporter state loc offset viewSize sightType
            | StaticLocation loc -> readStaticUnsafe reporter state loc offset viewSize sightType
        combine slices sightType

    and readFieldUnsafe (reporter : IErrorReporter) (state : state) (block : term) (field : fieldId) =
        let declaringType = field.declaringType
        match block.term with
        | Combined(_, t) when declaringType.IsAssignableFrom t || sizeOf block = internalSizeOf field.declaringType ->
            assert(sizeOf block = internalSizeOf field.declaringType)
            let fieldType = field.typ
            let startByte = Reflection.getFieldIdOffset field
            let endByte = startByte + internalSizeOf fieldType
            let sightType = Some fieldType
            let slices = readTermUnsafe reporter block (makeNumber startByte) (makeNumber endByte) sightType
            combine slices fieldType
        | Combined(slices, _) ->
            let isSuitableRef slice =
                match slice.term with
                | _ when isReference slice -> mostConcreteTypeOfRef state slice |> declaringType.IsAssignableFrom
                | Ptr(pointerBase, sightType, offset) ->
                    match tryPtrToRef state pointerBase sightType offset with
                    | Some address -> declaringType.IsAssignableFrom(address.TypeOfLocation)
                    | None -> false
                | _ -> false
            let refs = List.filter isSuitableRef slices |> List.distinct
            if List.length refs = 1 then
                let ref = List.head refs
                referenceField state ref field |> read reporter state
            else internalfail $"readFieldUnsafe: unexpected block {block}"
        | _ -> internalfail $"readFieldUnsafe: unexpected block {block}"

// -------------------------------- Pointer helpers --------------------------------

    and tryPtrToRef state pointerBase sightType offset : address option =
        assert(typeOf offset = typeof<int>)
        let zero = makeNumber 0
        let mutable sightType = sightType
        let suitableType t =
            if sightType = typeof<Void> then
                sightType <- t
                true
            else t = sightType
        match pointerBase with
        | HeapLocation(address, t) when address <> zeroAddress() ->
            let typ = typeOfHeapLocation state address |> mostConcreteType t
            let isArray() =
                typ.IsSZArray && suitableType (typ.GetElementType())
                || typ = typeof<string> && suitableType typeof<char>
            if typ.ContainsGenericParameters then None
            elif isArray() then
                let mutable elemSize = Nop()
                let checkOffset() =
                    elemSize <- makeNumber (internalSizeOf sightType)
                    rem offset elemSize = zero
                if checkOffset() then
                    let index = div offset elemSize
                    let address, arrayType =
                        if typ = typeof<string> then stringArrayInfo state address None
                        else address, (sightType, 1, true)
                    ArrayIndex(address, [index], arrayType) |> Some
                else None
            elif isValueType typ && suitableType typ && offset = zero then
                BoxedLocation(address, t) |> Some
            else None
        | StackLocation stackKey when suitableType stackKey.TypeOfLocation && offset = zero ->
            PrimitiveStackLocation stackKey |> Some
        | _ -> None

    and heapReferenceToBoxReference reference =
        match reference.term with
        | HeapRef(address, typ) ->
            assert(isBoxedType typ)
            Ref (BoxedLocation(address, typ))
        | Union gvs -> Merging.guardedMap heapReferenceToBoxReference gvs
        | _ -> internalfailf $"Unboxing: expected heap reference, but got {reference}"

    and referenceField state reference fieldId =
        let declaringType = fieldId.declaringType
        let isSuitableField address typ =
            let typ = mostConcreteTypeOfHeapRef state address typ
            declaringType.IsAssignableFrom typ
        match reference.term with
        | HeapRef(address, typ) when isSuitableField address typ |> not ->
            // TODO: check this case with casting via "is"
            Logger.trace $"[WARNING] unsafe cast of term {reference} in safe context"
            let offset = Reflection.getFieldIdOffset fieldId |> makeNumber
            Ptr (HeapLocation(address, typ)) fieldId.typ offset
        | HeapRef(address, typ) when typ = typeof<string> && fieldId = Reflection.stringFirstCharField ->
            let address, arrayType = stringArrayInfo state address None
            ArrayIndex(address, [makeNumber 0], arrayType) |> Ref
        | HeapRef(address, typ) when declaringType.IsValueType ->
            // TODO: Need to check mostConcreteTypeOfHeapRef using pathCondition?
            assert(isSuitableField address typ)
            let ref = heapReferenceToBoxReference reference
            referenceField state ref fieldId
        | HeapRef(address, typ) ->
            // TODO: Need to check mostConcreteTypeOfHeapRef using pathCondition?
            assert(isSuitableField address typ)
            ClassField(address, fieldId) |> Ref
        | Ref address when declaringType.IsAssignableFrom(address.TypeOfLocation) ->
            assert declaringType.IsValueType
            StructField(address, fieldId) |> Ref
        | Ref address ->
            assert declaringType.IsValueType
            let pointerBase, offset = Pointers.addressToBaseAndOffset address
            let fieldOffset = Reflection.getFieldIdOffset fieldId |> makeNumber
            Ptr pointerBase fieldId.typ (add offset fieldOffset)
        | Ptr(baseAddress, _, offset) ->
            let fieldOffset = Reflection.getFieldIdOffset fieldId |> makeNumber
            Ptr baseAddress fieldId.typ (add offset fieldOffset)
        | Union gvs ->
            let referenceField term = referenceField state term fieldId
            Merging.guardedMap referenceField gvs
        | _ -> internalfailf $"Referencing field: expected reference, but got {reference}"

// --------------------------- General reading ---------------------------

    // TODO: take type of heap address
    and read (reporter : IErrorReporter) state reference =
        match reference.term with
        | Ref address -> readSafe reporter state address
        | DetachedPtr _ ->
            reporter.ReportFatalError "reading by detached pointer" (True()) |> ignore
            Nop()
        | Ptr(baseAddress, sightType, offset) ->
            readUnsafe reporter state baseAddress offset sightType
        | Union gvs -> Merging.guardedMap (read reporter state) gvs
        | _ when typeOf reference |> isNative ->
            reporter.ReportFatalError "reading by detached pointer" (True()) |> ignore
            Nop()
        | _ -> internalfailf $"Reading: expected reference, but got {reference}"

    let isTypeInitialized state (typ : Type) =
        let key : symbolicTypeKey = {typ=typ}
        let matchingTypes = SymbolicSet.matchingElements key state.initializedTypes
        match matchingTypes with
        | [x] when x = key -> True()
        | _ ->
            let name = $"{typ}_initialized"
            let source : typeInitialized = {typ = typ; matchingTypes = SymbolicSet.ofSeq matchingTypes}
            Constant name source typeof<bool>

// ------------------------------- Writing -------------------------------

    let writeStackLocation (s : state) key value =
        s.stack <- CallStack.writeStackLocation s.stack key value

    let writeStruct (structTerm : term) (field : fieldId) value =
        match structTerm with
        | { term = Struct(fields, typ) } -> Struct (PersistentDict.add field value fields) typ
        | _ -> internalfailf $"Writing field of structure: expected struct, but got {structTerm}"

    let private writeClassFieldSymbolic state address (field : fieldId) value =
        ensureConcreteType field.typ
        let mr = accessRegion state.classFields field field.typ
        let key = {address = address}
        let mr' = MemoryRegion.write mr key value
        state.classFields <- PersistentDict.add field mr' state.classFields

    let private writeArrayRangeSymbolic state address fromIndices toIndices arrayType value =
        let fromIndices = List.map (fun i -> primitiveCast i typeof<int>) fromIndices
        let toIndices = List.map (fun i -> primitiveCast i typeof<int>) toIndices
        let key = RangeArrayIndexKey(address, fromIndices, toIndices)
        writeArrayKeySymbolic state key arrayType value

    let private arrayMemsetData state concreteAddress data arrayType =
        let arrayType = substituteTypeVariablesIntoArrayType state arrayType
        let elemType = fst3 arrayType
        ensureConcreteType elemType
        let region = accessRegion state.arrays arrayType elemType
        let region' = arrayRegionMemsetData state concreteAddress data elemType region
        state.arrays <- PersistentDict.add arrayType region' state.arrays

    let initializeArray state address indicesAndValues arrayType =
        let elementType = fst3 arrayType
        ensureConcreteType elementType
        let mr = accessRegion state.arrays arrayType elementType
        let keysAndValues = Seq.map (fun (i, v) -> OneArrayIndexKey(address, i), v) indicesAndValues
        let mr' = MemoryRegion.memset mr keysAndValues
        state.arrays <- PersistentDict.add arrayType mr' state.arrays

    let writeStaticField state typ (field : fieldId) value =
        ensureConcreteType field.typ
        let fieldType =
            if isImplementationDetails typ then typeof<byte>.MakeArrayType()
            else field.typ
        let mr = accessRegion state.staticFields field fieldType
        let key = {typ = typ}
        let mr' = MemoryRegion.write mr key value
        state.staticFields <- PersistentDict.add field mr' state.staticFields

    let private fillArrayBoundsSymbolic state address lengths lowerBounds arrayType =
        let d = List.length lengths
        assert(d = snd3 arrayType)
        assert(List.length lowerBounds = d)
        let writeLengths state l i = writeLengthSymbolic state address (Concrete i lengthType) arrayType l
        let writeLowerBounds state l i = writeLowerBoundSymbolic state address (Concrete i lengthType) arrayType l
        List.iter2 (writeLengths state) lengths [0 .. d-1]
        List.iter2 (writeLowerBounds state) lowerBounds [0 .. d-1]

    let writeStackBuffer state stackKey index value =
        let mr = accessRegion state.stackBuffers stackKey typeof<int8>
        let key : stackBufferIndexKey = {index = index}
        let mr' = MemoryRegion.write mr key value
        state.stackBuffers <- PersistentDict.add stackKey mr' state.stackBuffers

    let writeBoxedLocationSymbolic state (address : term) value typ =
        ensureConcreteType typ
        let mr = accessRegion state.boxedLocations typ typ
        let key = {address = address}
        let mr' = MemoryRegion.write mr key value
        state.boxedLocations <- PersistentDict.add typ mr' state.boxedLocations

    let writeBoxedLocation state (address : term) value =
        let cm = state.concreteMemory
        match state.memoryMode, address.term, tryTermToObj state value with
        | ConcreteMode, ConcreteHeapAddress a, Some value when cm.Contains(a) ->
            cm.Remove a
            cm.Allocate a value
        | ConcreteMode, ConcreteHeapAddress a, Some value ->
            cm.Allocate a value
        | ConcreteMode, ConcreteHeapAddress a, None when cm.Contains(a) ->
            cm.Remove a
            typeOf value |> writeBoxedLocationSymbolic state address value
        | _ -> typeOf value |> writeBoxedLocationSymbolic state address value

// ----------------- Unmarshalling: from concrete to symbolic memory -----------------

    let private unmarshallClass (state : state) concreteAddress obj =
        let address = ConcreteHeapAddress concreteAddress
        let writeField state (fieldId, fieldInfo : FieldInfo) =
            let value = fieldInfo.GetValue obj |> objToTerm state fieldInfo.FieldType
            writeClassFieldSymbolic state address fieldId value
        let fields = obj.GetType() |> Reflection.fieldsOf false
        Array.iter (writeField state) fields

    let private unmarshallArray (state : state) concreteAddress (array : Array) =
        let address = ConcreteHeapAddress concreteAddress
        let _, dim, _ as arrayType = array.GetType() |> symbolicTypeToArrayType
        let lbs = List.init dim array.GetLowerBound
        let lens = List.init dim array.GetLength
        let indicesWithValues = Array.getArrayIndicesWithValues array
        arrayMemsetData state concreteAddress indicesWithValues arrayType
        let lbToObj lb = objToTerm state typeof<int> lb
        let lenToObj len = objToTerm state typeof<int> len
        let termLBs = List.map lbToObj lbs
        let termLens = List.map lenToObj lens
        fillArrayBoundsSymbolic state address termLens termLBs arrayType

    let private unmarshallString (state : state) concreteAddress (string : string) =
        let address = ConcreteHeapAddress concreteAddress
        let concreteStringLength = string.Length
        let stringLength = makeNumber concreteStringLength
        let address, _ = stringArrayInfo state address (Some stringLength)
        writeClassFieldSymbolic state address Reflection.stringLengthField stringLength
        unmarshallArray state concreteAddress (string.ToCharArray())

    let unmarshall (state : state) concreteAddress =
        let cm = state.concreteMemory
        let obj = cm.VirtToPhys concreteAddress
        assert(box obj <> null)
        cm.Remove concreteAddress
        match obj with
        | :? Array as array -> unmarshallArray state concreteAddress array
        | :? String as string -> unmarshallString state concreteAddress string
        | _ -> unmarshallClass state concreteAddress obj

// ------------------------------- Writing -------------------------------

    let writeClassField state address (field : fieldId) value =
        let cm = state.concreteMemory
        let concreteValue = tryTermToObj state value
        match address.term, concreteValue with
        | ConcreteHeapAddress concreteAddress, Some obj when cm.Contains concreteAddress ->
            cm.WriteClassField concreteAddress field obj
        | ConcreteHeapAddress concreteAddress, None when cm.Contains concreteAddress ->
            unmarshall state concreteAddress
            writeClassFieldSymbolic state address field value
        | _ -> writeClassFieldSymbolic state address field value

    let writeArrayIndex state address indices arrayType value =
        let cm = state.concreteMemory
        let concreteValue = tryTermToObj state value
        let concreteIndices = tryIntListFromTermList indices
        match address.term, concreteValue, concreteIndices with
        | ConcreteHeapAddress a, Some obj, Some concreteIndices when cm.Contains a ->
            cm.WriteArrayIndex a concreteIndices obj
        | ConcreteHeapAddress a, _, None
        | ConcreteHeapAddress a, None, _ when cm.Contains a ->
            unmarshall state a
            writeArrayIndexSymbolic state address indices arrayType value
        | _ -> writeArrayIndexSymbolic state address indices arrayType value

    let writeArrayRange state address fromIndices toIndices arrayType value =
        let cm = state.concreteMemory
        let concreteValue = tryTermToObj state value
        let concreteFromIndices = tryIntListFromTermList fromIndices
        let concreteToIndices = tryIntListFromTermList toIndices
        match address.term, concreteValue, concreteFromIndices, concreteToIndices with
        | ConcreteHeapAddress a, Some v, Some l, Some r when cm.Contains a && List.length l = 1 ->
            assert(List.length r = 1)
            let l = List.head l
            let r = List.head r
            cm.FillArray a l (r - l) v
        | ConcreteHeapAddress a, _, _, _ when cm.Contains a ->
            unmarshall state a
            writeArrayRangeSymbolic state address fromIndices toIndices arrayType value
        | _ -> writeArrayRangeSymbolic state address fromIndices toIndices arrayType value

// ------------------------------- Unsafe writing -------------------------------

    let private writeAddressUnsafe (reporter : IErrorReporter) address startByte value =
        let addressSize = sizeOf address
        let valueSize = sizeOf value
        match startByte.term with
        | Concrete(:? int as s, _) when s = 0 && addressSize = valueSize -> value
        | _ ->
            let failCondition =
                simplifyNotEqual startByte (makeNumber 0) id
                ||| makeBool (valueSize <> addressSize)
            reporter.ReportFatalError "address reinterpretation" failCondition |> ignore
            value

    let rec writeTermUnsafe reporter term startByte value =
        let termType = typeOf term
        let valueType = typeOf value
        match term.term with
        | _ when startByte = makeNumber 0 && termType = valueType -> value
        | Struct(fields, t) -> writeStructUnsafe reporter term fields t startByte value
        | HeapRef _
        | Ref _
        | Ptr _ -> writeAddressUnsafe reporter term startByte value
        | Concrete _
        | Constant _
        | Expression _ ->
            let termSize = internalSizeOf termType
            let valueSize = internalSizeOf valueType
            match startByte.term with
            | Concrete(:? int as startByte, _) when startByte = 0 && valueSize = termSize ->
                combine (List.singleton value) termType
            | _ ->
                let zero = makeNumber 0
                let termSize = makeNumber termSize
                let valueSize = makeNumber valueSize
                let left = readTermPartUnsafe reporter term zero startByte None
                let valueSlices = readTermUnsafe reporter value (neg startByte) (sub termSize startByte) None
                let right = readTermPartUnsafe reporter term (add startByte valueSize) termSize None
                combine (left @ valueSlices @ right) termType
        | _ -> internalfailf $"writeTermUnsafe: unexpected term {term}"

    and private writeStructUnsafe reporter structTerm fields structType startByte value =
        let readField fieldId = fields[fieldId]
        let updatedFields = writeFieldsUnsafe reporter readField false structType startByte value
        let writeField structTerm (fieldId, value) = writeStruct structTerm fieldId value
        List.fold writeField structTerm updatedFields

    and private writeFieldsUnsafe reporter readField isStatic (blockType : Type) startByte value =
        let endByte = sizeOf value |> makeNumber |> add startByte
        let affectedFields = getAffectedFields reporter readField isStatic blockType startByte endByte
        let writeField (id, _, v, s, _) = id, writeTermUnsafe reporter v s value
        List.map writeField affectedFields

    let writeClassUnsafe reporter state address typ offset value =
        let readField fieldId = readClassField state address fieldId
        let updatedFields = writeFieldsUnsafe reporter readField false typ offset value
        let writeField (fieldId, value) = writeClassField state address fieldId value
        List.iter writeField updatedFields

    let writeArrayUnsafe reporter state address arrayType offset value =
        let size = sizeOf value
        let arrayType = symbolicTypeToArrayType arrayType
        let affectedIndices = getAffectedIndices reporter state address arrayType offset size
        let writeElement (index, element, startByte, _) =
            let updatedElement = writeTermUnsafe reporter element startByte value
            writeArrayIndex state address index arrayType updatedElement
        List.iter writeElement affectedIndices

    let private writeStringUnsafe reporter state address offset value =
        let size = sizeOf value
        let address, arrayType = stringArrayInfo state address None
        let affectedIndices = getAffectedIndices reporter state address arrayType offset size
        let writeElement (index, element, startByte, _) =
            let updatedElement = writeTermUnsafe reporter element startByte value
            writeArrayIndex state address index arrayType updatedElement
        List.iter writeElement affectedIndices

    let writeStaticUnsafe reporter state staticType offset value =
        let readField fieldId = readStaticField state staticType fieldId
        let updatedFields = writeFieldsUnsafe reporter readField true staticType offset value
        let writeField (fieldId, value) = writeStaticField state staticType fieldId value
        List.iter writeField updatedFields

    let writeStackUnsafe reporter state loc offset value =
        let term = readStackLocation state loc
        let locSize = sizeOf term |> makeNumber
        let endByte = sizeOf value |> makeNumber |> add offset
        let inBlock = checkBlockBounds reporter locSize offset endByte
        if inBlock then
            let updatedTerm = writeTermUnsafe reporter term offset value
            writeStackLocation state loc updatedTerm

    let private writeUnsafe reporter state baseAddress offset value =
        match baseAddress with
        | HeapLocation(loc, sightType) ->
            let typ = mostConcreteTypeOfHeapRef state loc sightType
            match typ with
            | StringType -> writeStringUnsafe reporter state loc offset value
            | ClassType _ -> writeClassUnsafe reporter state loc typ offset value
            | ArrayType _ -> writeArrayUnsafe reporter state loc typ offset value
            | StructType _ -> internalfail "writeUnsafe: unsafe writing is not implemented for structs" // TODO: boxed location?
            | _ -> internalfailf $"expected complex type, but got {typ}"
        | StackLocation loc -> writeStackUnsafe reporter state loc offset value
        | StaticLocation loc -> writeStaticUnsafe reporter state loc offset value

// ------------------------------- General writing -------------------------------

    let isSafeContextWrite actualType neededType =
        assert(neededType <> typeof<Void>)
        neededType = actualType
        || canCastImplicitly neededType actualType
        && internalSizeOf actualType = internalSizeOf neededType

    // NOTE: using unsafe write instead of safe, when field intersects,
    // because need to write to all fields, which intersects with 'field'
    let private writeIntersectingField reporter state address (field : fieldId) value =
        let baseAddress, offset = Pointers.addressToBaseAndOffset address
        let ptr = Ptr baseAddress field.typ offset
        match ptr.term with
        | Ptr(baseAddress, _, offset) -> writeUnsafe reporter state baseAddress offset value
        | _ -> internalfailf $"expected to get ptr, but got {ptr}"

    let rec private writeSafe reporter state address value =
        match address with
        | PrimitiveStackLocation key -> writeStackLocation state key value
        | ClassField(_, field)
        | StructField(_, field) when Reflection.fieldIntersects field ->
            writeIntersectingField reporter state address field value
        | ClassField(address, field) -> writeClassField state address field value
        | ArrayIndex(address, indices, typ) -> writeArrayIndex state address indices typ value
        | StaticField(typ, field) -> writeStaticField state typ field value
        | StructField(address, field) ->
            let oldStruct = readSafe reporter state address
            let newStruct = writeStruct oldStruct field value
            writeSafe reporter state address newStruct
        // TODO: need concrete memory for BoxedLocation?
        | BoxedLocation(address, _) -> writeBoxedLocation state address value
        | StackBufferIndex(key, index) -> writeStackBuffer state key index value
        // NOTE: Cases below is needed to construct a model
        | ArrayLength(address, dimension, typ) -> writeLengthSymbolic state address dimension typ value
        | ArrayLowerBound(address, dimension, typ) -> writeLowerBoundSymbolic state address dimension typ value

    let write (reporter : IErrorReporter) state reference value =
        match reference.term with
        | Ref address -> writeSafe reporter state address value
        | DetachedPtr _ -> reporter.ReportFatalError "writing by detached pointer" (True()) |> ignore
        | Ptr(address, _, offset) -> writeUnsafe reporter state address offset value
        | _ when typeOf reference |> isNative ->
            reporter.ReportFatalError "writing by detached pointer" (True()) |> ignore
        | _ -> internalfail $"Writing: expected reference, but got {reference}"
        state

// ------------------------------- Allocation -------------------------------

    let allocateOnStack state key term =
        state.stack <- CallStack.allocate state.stack key term

    // Strings and delegates should be allocated using the corresponding functions (see allocateString and allocateDelegate)!
    let allocateClass state typ =
        assert (not <| isSubtypeOrEqual typ typeof<String>)
        assert (not <| isSubtypeOrEqual typ typeof<Delegate>)
        let concreteAddress = allocateConcreteType state typ
        match state.memoryMode with
        // TODO: it's hack for reflection, remove it after concolic will be implemented
        | _ when isSubtypeOrEqual typ typeof<Type> -> ()
        | ConcreteMode ->
            let object = Reflection.createObject typ
            state.concreteMemory.Allocate concreteAddress object
        | SymbolicMode -> ()
        HeapRef (ConcreteHeapAddress concreteAddress) typ

    // TODO: unify allocation with unmarshalling
    let allocateArray state typ lowerBounds lengths =
        assert (isSubtypeOrEqual typ typeof<Array>)
        let concreteAddress = allocateConcreteType state typ
        let arrayType = symbolicTypeToArrayType typ
        let address = ConcreteHeapAddress concreteAddress
        let concreteLengths = tryIntListFromTermList lengths
        let concreteLowerBounds = tryIntListFromTermList lowerBounds
        match state.memoryMode, concreteLengths, concreteLowerBounds with
        | ConcreteMode, Some concreteLengths, Some concreteLBs ->
            let elementDotNetType = elementType typ
            let array = Array.CreateInstance(elementDotNetType, Array.ofList concreteLengths, Array.ofList concreteLBs) :> obj
            state.concreteMemory.Allocate concreteAddress array
        | _ -> fillArrayBoundsSymbolic state address lengths lowerBounds arrayType
        address

    let allocateVector state (elementType : Type) length =
        let typ = elementType.MakeArrayType()
        allocateArray state typ [makeNumber 0] [length]

    let allocateConcreteVector state (elementType : Type) length contents =
        match state.memoryMode, length.term with
        | ConcreteMode, Concrete(:? int as intLength, _) ->
            let concreteAddress = allocateConcreteType state (elementType.MakeArrayType())
            let array = Array.CreateInstance(elementType, intLength)
            Seq.iteri (fun i value -> array.SetValue(value, i)) contents
            state.concreteMemory.Allocate concreteAddress (array :> obj)
            ConcreteHeapAddress concreteAddress
        | _ ->
            let address = allocateVector state elementType length
            let arrayType : arrayType = (elementType, 1, true)
            let mr = accessRegion state.arrays arrayType elementType
            let keysAndValues = Seq.mapi (fun i v -> OneArrayIndexKey(address, [makeNumber i]), Concrete v elementType) contents
            let mr' = MemoryRegion.memset mr keysAndValues
            state.arrays <- PersistentDict.add arrayType mr' state.arrays
            address

    // TODO: unify allocation with unmarshalling
    let private commonAllocateString state length contents =
        match state.memoryMode, length.term with
        | ConcreteMode, Concrete(:? int as intLength, _) ->
            // TODO: implement interning (for String.Empty)
            let charArray : char array = Array.create intLength '\000'
            Seq.iteri (fun i char -> charArray.SetValue(char, i)) contents
            let string = new string(charArray) :> obj
            allocateObjectIfNeed state string typeof<string>
        | _ ->
            let arrayLength = add length (Concrete 1 lengthType)
            let address = allocateConcreteVector state typeof<char> arrayLength contents
            let address, _ = stringArrayInfo state address (Some length)
            let heapAddress = getConcreteHeapAddress address
            writeClassField state address Reflection.stringLengthField length
            state.allocatedTypes <- PersistentDict.add heapAddress (ConcreteType typeof<string>) state.allocatedTypes
            address

    let allocateEmptyString state length =
        let address = commonAllocateString state length Seq.empty
        HeapRef address typeof<string>
    let allocateString state (str : string) =
        let address = commonAllocateString state (Concrete str.Length lengthType) str
        HeapRef address typeof<string>

    let createStringFromChar state char =
        match char.term with
        | Concrete(:? char as c, _) ->
            let string = c.ToString()
            allocateString state string
        | _ ->
            let len = makeNumber 1
            let address = commonAllocateString state len " "
            let address, arrayType = stringArrayInfo state address (Some len)
            writeArrayIndex state address [Concrete 0 indexType] arrayType char
            HeapRef address typeof<string>

    let allocateBoxedLocation state value =
        let typ = typeOf value
        let concreteAddress = allocateConcreteType state typ
        let address = ConcreteHeapAddress concreteAddress
        match state.memoryMode, tryTermToObj state value with
        // 'value' may be null, if it's nullable value type
        | ConcreteMode, Some value when value <> null ->
            assert(value :? ValueType)
            state.concreteMemory.Allocate concreteAddress value
        | _ -> writeBoxedLocationSymbolic state address value typ
        HeapRef address typeof<obj>

    let allocateConcreteObject state obj (typ : Type) =
        assert(not typ.IsAbstract)
        match state.memoryMode with
        | ConcreteMode ->
            let address = allocateObjectIfNeed state obj typ
            HeapRef address typ
        | SymbolicMode -> internalfailf $"allocateConcreteObject: allocating concrete object {obj} in symbolic memory is not implemented"

    let allocateTemporaryLocalVariableOfType state name index typ =
        let tmpKey = TemporaryLocalVariableKey(typ, index)
        let ref = PrimitiveStackLocation tmpKey |> Ref
        let value = makeSymbolicValue {key = tmpKey; time = VectorTime.zero} name typ
        allocateOnStack state tmpKey value
        ref

    let rec lengthOfString state heapRef =
        match heapRef.term with
        | HeapRef(address, typ) ->
            assert(typ = typeof<string>)
            readClassField state address Reflection.stringLengthField
        | Union gvs -> Merging.guardedMap (lengthOfString state) gvs
        | _ -> internalfail "Getting length of string: expected heap reference, but got %O" heapRef

    let initializeStaticMembers state typ =
        if typ = typeof<string> then
            let reference = allocateString state ""
            writeStaticField state typeof<string> Reflection.emptyStringField reference
        state.initializedTypes <- SymbolicSet.add {typ=typ} state.initializedTypes

    let markTypeInitialized state typ =
        state.initializedTypes <- SymbolicSet.add {typ=typ} state.initializedTypes

// ------------------------------- Delegates -------------------------------

    let private objToDelegate state (d : Delegate) =
        let delegateType = d.GetType()
        let target = d.Target
        let target =
            if target <> null then
                let targetType = target.GetType()
                objToTerm state targetType d.Target
            else nullRef typeof<obj>
        concreteDelegate d.Method target delegateType

    let rec readDelegate state reference =
        let cm = state.concreteMemory
        match reference.term with
        | HeapRef({term = ConcreteHeapAddress address}, _) when cm.Contains address ->
            let d = cm.VirtToPhys address
            assert(d :? Delegate && d <> null)
            let d = d :?> Delegate
            let delegateType = d.GetType()
            let invokeList = d.GetInvocationList()
            if invokeList <> null then
                let delegates = Array.map (objToDelegate state) invokeList
                if Array.length delegates = 1 then Array.head delegates |> Some
                else createCombinedDelegate (Array.toList delegates) delegateType |> Some
            else objToDelegate state d |> Some
        | HeapRef({term = ConcreteHeapAddress address}, _) -> state.delegates[address] |> Some
        | HeapRef _ -> None
        | Union gvs ->
            let delegates = gvs |> List.choose (fun (g, v) ->
                Option.bind (fun d -> Some(g, d)) (readDelegate state v))
            if delegates.Length = gvs.Length then delegates |> Merging.merge |> Some else None
        | _ -> internalfailf $"Reading delegate: expected heap reference, but got {reference}"

    and private simplifyDelegateRec state acc d =
        match d.term with
        | CombinedDelegate delegates -> List.append delegates acc
        | ConcreteDelegate _ -> d :: acc
        | _ ->
            assert(isReference d)
            linearizeDelegateRec state d acc

    and private linearizeDelegateRec state d acc =
        if isReference d then
            match readDelegate state d with
            | Some d -> simplifyDelegateRec state acc d
            | None -> d :: acc
        else simplifyDelegateRec state acc d

    and private simplifyDelegate state d =
        simplifyDelegateRec state List.empty d

    let private linearizeDelegate state ref =
        linearizeDelegateRec state ref List.empty

    let private getDelegates state address =
        match PersistentDict.tryFind state.delegates address with
        | Some d -> simplifyDelegate state d
        | None -> List.empty

    let allocateDelegate state (methodInfo : MethodInfo) target delegateType =
        let concreteAddress = allocateConcreteType state delegateType
        let address = ConcreteHeapAddress concreteAddress
        match state.memoryMode, tryTermToObj state target with
        | ConcreteMode, Some target ->
            let d = methodInfo.CreateDelegate(delegateType, target)
            state.concreteMemory.Allocate concreteAddress d
            HeapRef address delegateType
        | _ ->
            let d = concreteDelegate methodInfo target delegateType
            state.delegates <- PersistentDict.add concreteAddress d state.delegates
            HeapRef address delegateType

    let rec private allocateCombinedDelegateSymbolic state concreteAddress delegateRefs t =
        let delegates = List.collect (linearizeDelegate state) delegateRefs
        let combined = createCombinedDelegate delegates t
        state.delegates <- PersistentDict.add concreteAddress combined state.delegates

    let allocateCombinedDelegate state concreteAddress (delegateRefs : term list) t =
        let concreteDelegates = tryTermListToObjects state delegateRefs
        match state.memoryMode, concreteDelegates with
        | ConcreteMode, Some list ->
            assert(List.isEmpty list |> not)
            if List.length list = 1 then
                state.concreteMemory.Allocate concreteAddress (List.head list)
            else
                let delegates = Seq.cast list
                let combined = Delegate.Combine (Seq.toArray delegates)
                state.concreteMemory.Allocate concreteAddress combined
        | _ -> allocateCombinedDelegateSymbolic state concreteAddress delegateRefs t

    let combineDelegates state (delegateRefs : term list) typ =
        assert(List.isEmpty delegateRefs |> not)
        let concreteAddress = allocateConcreteType state typ
        let address = ConcreteHeapAddress concreteAddress
        allocateCombinedDelegate state concreteAddress delegateRefs typ
        HeapRef address typ

    let private delegatesMatch ref1 ref2 =
        match ref1.term, ref2.term with
        | HeapRef(address1, _), HeapRef(address2, _) -> address1 = address2
        | _ -> internalfail $"delegatesMatch: unexpected references {ref1}, {ref2}"

    let removeDelegate state (sourceRef : term) (toRemoveRef : term) typ =
        let cm = state.concreteMemory
        let toRemove = tryTermToObj state toRemoveRef
        match state.memoryMode, sourceRef.term, toRemove with
        | ConcreteMode, HeapRef({term = ConcreteHeapAddress a}, _), Some toRemove when cm.Contains a ->
            let source = cm.VirtToPhys a
            assert(source :? Delegate && toRemove :? Delegate)
            let source = source :?> Delegate
            let result = Delegate.Remove(source, toRemove :?> Delegate)
            if Object.ReferenceEquals(result, source) then sourceRef
            else
                let concreteAddress = allocateConcreteType state typ
                state.concreteMemory.Allocate concreteAddress result
                HeapRef (ConcreteHeapAddress concreteAddress) typ
        | _, HeapRef({term = ConcreteHeapAddress a}, _), _ ->
            let sourceDelegates = getDelegates state a
            let removeDelegates = simplifyDelegate state toRemoveRef
            let removed, result = List.removeSubList sourceDelegates removeDelegates
            if removed then
                if List.isEmpty result then nullRef typ
                else
                    let concreteAddress = allocateConcreteType state typ
                    allocateCombinedDelegate state concreteAddress result typ
                    HeapRef (ConcreteHeapAddress concreteAddress) typ
            else sourceRef
        | _ -> sourceRef

// ------------------------------- Composition -------------------------------

    let private skipSuffixWhile predicate ys =
        let skipIfNeed y acc k =
            if predicate (y::acc) then k (y::acc)
            else List.take (List.length ys - List.length acc) ys
        Cps.List.foldrk skipIfNeed [] ys (always [])

    let private composeTime state time =
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
                let read region =
                    let inst = makeSymbolicHeapRead state x.picker key state.startingTime
                    MemoryRegion.read region key (x.picker.isDefaultKey state) inst
                afters |> List.map (mapsnd read) |> Merging.merge

    type arrayReading with
        interface IMemoryAccessConstantSource with
            override x.Compose state =
                // TODO: do nothing if state is empty!
                let substTerm = fillHoles state
                let substType = substituteTypeVariables state
                let substTime = composeTime state
                let key = x.key :> IHeapArrayKey
                let key = key.Map substTerm substType substTime key.Region |> snd
                let afters =
                    if not x.picker.isDefaultRegion then
                        let effect = MemoryRegion.map substTerm substType substTime x.memoryObject
                        let before = x.picker.extract state
                        MemoryRegion.compose before effect
                    else List.singleton (True(), x.memoryObject)
                let read region =
                    let inst = makeArraySymbolicHeapRead state x.picker key state.startingTime
                    MemoryRegion.read region key (x.picker.isDefaultKey state) inst
                afters |> List.map (mapsnd read) |> Merging.merge

    type stackReading with
        interface IMemoryAccessConstantSource with
            override x.Compose state =
                let key = x.key.Map (typeVariableSubst state)
                readStackLocation state key

    let (|StackReading|_|) (src : ISymbolicConstantSource) =
        match src with
        | :? stackReading as sr -> Some(sr.key)
        | _ -> None

    let composeBaseSource state (baseSource : ISymbolicConstantSource) =
        match baseSource with
        | :? IStatedSymbolicConstantSource as baseSource ->
            baseSource.Compose state
        | _ ->
            match state.model with
            | PrimitiveModel subst when state.complete ->
                let value = ref (Nop())
                if subst.TryGetValue(baseSource, value) then value.Value
                else makeDefaultValue baseSource.TypeOfLocation
            | _ ->
                makeSymbolicValue baseSource (baseSource.ToString()) baseSource.TypeOfLocation

    type private structField with
        interface IMemoryAccessConstantSource with
            override x.Compose state =
                let structTerm = composeBaseSource state x.baseSource
                readStruct emptyReporter state structTerm x.field

    type private heapAddressSource with
        interface IMemoryAccessConstantSource with
            override x.Compose state =
                composeBaseSource state x.baseSource |> extractAddress

    type private pointerAddressSource with
        interface IMemoryAccessConstantSource with
            override x.Compose state =
                composeBaseSource state x.baseSource |> extractAddress

    type private pointerOffsetSource with
        interface IMemoryAccessConstantSource with
            override x.Compose state =
                composeBaseSource state x.baseSource |> extractPointerOffset

    // state is untouched. It is needed because of this situation:
    // Effect: x' <- y + 5, y' <- x + 10
    // Left state: x <- 0, y <- 0
    // After composition: {x <- 5, y <- 15} OR {y <- 10, x <- 15}
    // but expected result is {x <- 5, y <- 10}
    let private fillHolesInStack state stack =
        let keyMapper (k : stackKey) = k.Map (typeVariableSubst state)
        CallStack.map keyMapper (fillHoles state) (substituteTypeVariables state) stack

    let composeRaisedExceptionsOf (state : state) (exceptionRegister : exceptionRegisterStack) =
        let elem, rest = state.exceptionsRegister.Pop()
        match elem with
        | NoException ->
            rest.Push exceptionRegister.Peek
            |> exceptionRegisterStack.map (fillHoles state)
        | _ -> __unreachable__()

    let private composeStacksOf state state' : callStack =
        let stack' = fillHolesInStack state state'.stack
        CallStack.applyEffect state.stack stack'

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
                let! g, dict = dicts
                let! g', mr =
                    let mr =
                        match PersistentDict.tryFind dict k with
                        | Some mr -> mr
                        | None -> MemoryRegion.empty mr'.typ
                    MemoryRegion.compose mr mr' |> List.map (fun (g, mr) -> (g, PersistentDict.add k mr dict))
                return (g &&& g', mr)
            }
        dict'
            |> PersistentDict.map id (MemoryRegion.map substTerm substType substTime)
            |> PersistentDict.fold composeOneRegion [(True(), dict)]

    let private composeTypeVariablesOf state state' =
        let ms, s = state.typeVariables
        let ms', s' = state'.typeVariables
        let ms' = MappedStack.map (fun _ v -> substituteTypeVariables state v) ms'
        (MappedStack.concat ms ms', List.append s' s)

    let private composeInitializedTypes state initializedTypes =
        let it' = SymbolicSet.map (fun _ -> __unreachable__()) (substituteTypeVariables state) (fun _ -> __unreachable__()) initializedTypes
        SymbolicSet.union state.initializedTypes it'

    let private composeConcreteDictionaries mapKey mapValue dict dict' =
        let fillAndMutate acc k v =
            let k' = mapKey k
            let v' = mapValue v
            match PersistentDict.tryFind acc k' with
            | Some v ->
                assert(v = v')
                acc
            | None -> PersistentDict.add k' v' acc
        PersistentDict.fold fillAndMutate dict dict'

    let private composeConcreteMemory mapKey (cm : IConcreteMemory) (cm' : IConcreteMemory) =
        // TODO: implement concrete memory composition
        ()

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

    // TODO: something fails and we get address 79.0 (Test: StackTrace1 without 'def = new ...', so def is symbolic)
    let composeStates state state' =
        assert(VectorTime.isDescending state.currentTime)
        assert(VectorTime.isDescending state'.currentTime)
        assert(not <| VectorTime.isEmpty state.currentTime)
        let substituteTypeVariablesToSymbolicType state = function
            | ConcreteType t -> t |> substituteTypeVariables state |> ConcreteType
            | MockType _ -> __unreachable__()
        let methodMocks = Dictionary()
        for kvp in state.methodMocks do
            methodMocks.Add(kvp.Key, kvp.Value)
        for kvp in state'.methodMocks do
            methodMocks.Add(kvp.Key, kvp.Value)

        // TODO: do nothing if state is empty!
        list {
            let pc = PC.mapPC (fillHoles state) state'.pc |> PC.union state.pc
            // Note: this is not final evaluationStack of resulting cilState, here we forget left state's opStack at all
            let evaluationStack = composeEvaluationStacksOf state state'.evaluationStack
            let exceptionRegister = composeRaisedExceptionsOf state state'.exceptionsRegister
            let stack = composeStacksOf state state'
            let! g1, stackBuffers = composeMemoryRegions state state.stackBuffers state'.stackBuffers
            let! g2, classFields = composeMemoryRegions state state.classFields state'.classFields
            let! g3, arrays = composeMemoryRegions state state.arrays state'.arrays
            let! g4, lengths = composeMemoryRegions state state.lengths state'.lengths
            let! g5, lowerBounds = composeMemoryRegions state state.lowerBounds state'.lowerBounds
            let! g6, staticFields = composeMemoryRegions state state.staticFields state'.staticFields
            let initializedTypes = composeInitializedTypes state state'.initializedTypes
            composeConcreteMemory (composeTime state) state.concreteMemory state'.concreteMemory
            let allocatedTypes = composeConcreteDictionaries (composeTime state) (substituteTypeVariablesToSymbolicType state) state.allocatedTypes state'.allocatedTypes
            let typeVariables = composeTypeVariablesOf state state'
            let delegates = composeConcreteDictionaries (composeTime state) id state.delegates state'.delegates
            let currentTime = composeTime state state'.currentTime
            let g = g1 &&& g2 &&& g3 &&& g4 &&& g5 &&& g6
            if not <| isFalse g then
                return {
                    pc = if isTrue g then pc else PC.add pc g
                    typeStorage = state.typeStorage
                    evaluationStack = evaluationStack
                    exceptionsRegister = exceptionRegister
                    stack = stack
                    stackBuffers = stackBuffers
                    classFields = classFields
                    arrays = arrays
                    lengths = lengths
                    lowerBounds = lowerBounds
                    staticFields = staticFields
                    boxedLocations = state.boxedLocations // TODO: compose boxed locations
                    initializedTypes = initializedTypes
                    concreteMemory = state.concreteMemory // TODO: compose concrete memory
                    allocatedTypes = allocatedTypes
                    initializedAddresses = state.initializedAddresses // TODO: compose initialized addresses
                    typeVariables = typeVariables
                    delegates = delegates
                    currentTime = currentTime
                    startingTime = state.startingTime
                    model = state.model // TODO: compose models (for example, mocks)
                    complete = state.complete
                    memoryMode = state.memoryMode // TODO: compose memory mode
                    methodMocks = methodMocks
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
        let sb = StringBuilder()
        let sb = if PC.isEmpty s.pc then sb else s.pc |> PC.toString |> sprintf "Path condition: %s" |> PrettyPrinting.appendLine sb
        let sb = dumpDict "Fields" (sortBy toString) toString (MemoryRegion.toString "    ") sb s.classFields
        let sb = dumpDict "Array contents" (sortBy arrayTypeToString) arrayTypeToString (MemoryRegion.toString "    ") sb s.arrays
        let sb = dumpDict "Array lengths" (sortBy arrayTypeToString) arrayTypeToString (MemoryRegion.toString "    ") sb s.lengths
        let sb = dumpDict "Types tokens" sortVectorTime VectorTime.print toString sb s.allocatedTypes
        let sb = dumpDict "Static fields" (sortBy toString) toString (MemoryRegion.toString "    ") sb s.staticFields
        let sb = dumpDict "Delegates" sortVectorTime VectorTime.print toString sb s.delegates
        let sb = dumpStack sb s.stack
        let sb = dumpInitializedTypes sb s.initializedTypes
        let sb = dumpEvaluationStack sb s.evaluationStack
        if sb.Length = 0 then "<Empty>"
        else
            System.Text.RegularExpressions.Regex.Replace(sb.ToString(), @"@\d+(\+|\-)\d*\[Microsoft.FSharp.Core.Unit\]", "");
