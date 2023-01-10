namespace VSharp.Core

open System
open System.Collections.Generic
open System.Runtime.CompilerServices
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
        concreteMemory = ConcreteMemory()
        allocatedTypes = PersistentDict.empty
        typeVariables = (MappedStack.empty, Stack.empty)
        delegates = PersistentDict.empty
        currentTime = [1]
        startingTime = VectorTime.zero
        model = PrimitiveModel (Dictionary())
        complete = complete
        methodMocks = Dictionary()
        externMocks = Dictionary<_,_>()
    }

    type memoryMode =
        | ConcreteMemory
        | SymbolicMemory

    let mutable memoryMode = ConcreteMemory

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
        let length = List.length indices
        let attachOne acc i =
            let relOffset = sub indices.[i] lbs.[i]
            let prod acc j = mul acc lens.[j]
            let lensProd = List.fold prod (makeNumber 1) [i .. length - 1]
            let absOffset = mul relOffset lensProd
            add acc absOffset
        List.fold attachOne (makeNumber 0) [0 .. length - 1]

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

    let baseTypeOfAddress state address =
        match address with
        | BoxedLocation(addr, _) -> typeOfConcreteHeapAddress state addr
        | _ -> typeOfAddress address

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

    let getHashCode object =
        // TODO: implement GetHashCode() for value type (it's boxed)
        // TODO: use 'termToObj' for valid hash
        match object.term with
        | ConcreteHeapAddress address
        | HeapRef({term = ConcreteHeapAddress address}, _) -> hashConcreteAddress address
        | HeapRef(address, _) ->
            let name = $"HashCode({address})"
            let source = {object = address}
            Constant name source typeof<Int32>
        | _ -> internalfail $"Getting hash code: unexpected object {object}"

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

    let specializeWithKey constant (key : heapArrayKey) (writeKey : heapArrayKey) =
        match constant.term with
        | Constant(_, ArrayRangeReading(mo, srcAddress, srcFrom, srcTo, picker, time), typ) ->
            let key = key.Specialize writeKey srcAddress srcFrom srcTo
            let source : arrayReading = {picker = picker; key = key; memoryObject = mo; time = time}
            let name = picker.mkName key
            Constant name source typ
        | _ -> constant

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
        interface IMemoryAccessConstantSource  with
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
        interface IMemoryAccessConstantSource  with
            override x.SubTerms = x.baseSource.SubTerms
            override x.Time = x.baseSource.Time
            override x.TypeOfLocation = x.baseSource.TypeOfLocation

    let (|HeapAddressSource|_|) (src : ISymbolicConstantSource) =
        match src with
        | :? heapAddressSource as heapAddress -> Some(heapAddress.baseSource)
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
        | ValueType -> __insufficientInformation__ "Can't instantiate symbolic value of unknown value type %O" typ
        | ByRef _ -> __insufficientInformation__ "Can't instantiate symbolic value of ByRef type %O" typ
        | _ -> __insufficientInformation__ "Not sure which value to instantiate, because it's unknown if %O is a reference or a value type" typ

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
        | Some {key = key'; value = {term = Constant(_, ArrayRangeReading(mo, srcA, srcF, srcT, p, _), _)}} when key'.Includes key ->
            let key = key.Specialize key' srcA srcF srcT
            let inst = makeArraySymbolicHeapRead state p key state.startingTime
            MemoryRegion.read mo key (p.isDefaultKey state) inst
        | Some { key = key'; value = value } when key'.Includes key -> value
        | _ ->
            let source : arrayReading = {picker = picker; key = key; memoryObject = memoryObject; time = time}
            let name = picker.mkName key
            makeSymbolicValue source name typ

    let makeSymbolicThis (m : IMethod) =
        let declaringType = m.DeclaringType
        if isValueType declaringType then __insufficientInformation__ "Can't execute in isolation methods of value types, because we can't be sure where exactly \"this\" is allocated!"
        else HeapRef (Constant "this" {baseSource = {key = ThisKey m; time = VectorTime.zero}} addressType) declaringType

    let fillModelWithParametersAndThis state (method : IMethod) =
        let parameters = method.Parameters |> Seq.map (fun param ->
            (ParameterKey param, None, param.ParameterType)) |> List.ofSeq
        let parametersAndThis =
            if method.HasThis then
                let t = method.DeclaringType
                let addr = [-1]
                let thisRef = HeapRef (ConcreteHeapAddress addr) t
                state.allocatedTypes <- PersistentDict.add addr (ConcreteType t) state.allocatedTypes
                state.startingTime <- [-2]
                (ThisKey method, Some thisRef, t) :: parameters // TODO: incorrect type when ``this'' is Ref to stack
            else parameters
        newStackFrame state None parametersAndThis

// -------------------------- Allocation helpers --------------------------

    let freshAddress state =
        state.currentTime <- VectorTime.advance state.currentTime
        state.currentTime

    let allocateType state (typ : Type) =
        assert(not typ.IsAbstract)
        let concreteAddress = freshAddress state
        assert(not <| PersistentDict.contains concreteAddress state.allocatedTypes)
        state.allocatedTypes <- PersistentDict.add concreteAddress (ConcreteType typ) state.allocatedTypes
        concreteAddress

// =============== Marshalling/unmarshalling without state changing ===============

    // ------------------ Object to term ------------------

    let private allocateObjectIfNeed state (obj : obj) t =
        assert(memoryMode = ConcreteMemory)
        let cm = state.concreteMemory
        let address =
            match cm.TryPhysToVirt obj with
            | Some address -> address
            | None when obj = null -> VectorTime.zero
            | None ->
                let typ = mostConcreteType (obj.GetType()) t
                if typ.IsValueType then Logger.trace "allocateObjectIfNeed: boxing concrete struct %O" obj
                let concreteAddress = allocateType state typ
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
        let makeField (fieldInfo : Reflection.FieldInfo) _ _ =
           fieldInfo.GetValue(obj) |> objToTerm state fieldInfo.FieldType
        makeStruct false makeField t

    and private nullableToTerm state t (obj : obj) =
        let nullableType = Nullable.GetUnderlyingType t
        let valueField, hasValueField = Reflection.fieldsOfNullable t
        let value, hasValue =
            if box obj <> null then objToTerm state nullableType obj, True
            else objToTerm state nullableType (Reflection.createObject nullableType), False
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
        | Concrete(obj, _) -> Some obj
        | Struct(fields, typ) when isNullable typ -> tryNullableTermToObj state fields typ
        | Struct(fields, typ) when not typ.IsByRefLike -> tryStructTermToObj state fields typ
        | HeapRef({term = ConcreteHeapAddress a}, _) -> tryAddressToObj state a
        | Ptr(HeapLocation({term = ConcreteHeapAddress a}, _), _, ConcreteT (:? int as offset, _)) ->
            tryPointerToObj state a offset
        | _ -> None

    and tryStructTermToObj (state : state) fields typ =
        let structObj = Reflection.createObject typ
        let addField _ (fieldId, value) k =
            let fieldInfo = Reflection.getFieldInfo fieldId
            // field was not found in the structure, skipping it
            if fieldInfo = null then k ()
            else
                match tryTermToObj state value with
                // field can be converted to obj, so continue
                | Some v -> fieldInfo.SetValue(structObj, v) |> k
                // field can not be converted to obj, so break and return None
                | None -> None
        Cps.Seq.foldlk addField () (PersistentDict.toSeq fields) (fun _ -> Some structObj)

    and tryNullableTermToObj (state : state) fields typ =
        let valueField, hasValueField = Reflection.fieldsOfNullable typ
        let value = PersistentDict.find fields valueField
        let hasValue = PersistentDict.find fields hasValueField
        match tryTermToObj state value with
        | Some obj when hasValue = True -> Some obj
        | _ when hasValue = False -> Some null
        | _ -> None

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

    let rec extractAddress reference =
        match reference.term with
        | HeapRef(address, _) -> address
        | Union gvs -> gvs |> List.map (fun (g, v) -> (g, extractAddress v)) |> Merging.merge
        | _ -> internalfail "Extracting heap address: expected heap reference, but got %O" reference

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

    let readStruct (structTerm : term) (field : fieldId) =
        match structTerm with
        | { term = Struct(fields, _) } -> fields.[field]
        | _ -> internalfailf "Reading field of structure: expected struct, but got %O" structTerm

    let private readLowerBoundSymbolic (state : state) address dimension arrayType =
        let extractor (state : state) = accessRegion state.lowerBounds (substituteTypeVariablesIntoArrayType state arrayType) lengthType
        let mkname = fun (key : heapVectorIndexKey) -> sprintf "LowerBound(%O, %O)" key.address key.index
        let isDefault state (key : heapVectorIndexKey) = isHeapAddressDefault state key.address || thd3 arrayType
        let key = {address = address; index = dimension}
        let inst typ memoryRegion =
            let sort = ArrayLowerBoundSort arrayType
            let picker = {sort = sort; extract = extractor; mkName = mkname; isDefaultKey = isDefault; isDefaultRegion = false}
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
        let mkname = fun (key : heapVectorIndexKey) -> sprintf "Length(%O, %O)" key.address key.index
        let isDefault state (key : heapVectorIndexKey) = isHeapAddressDefault state key.address
        let key = {address = address; index = dimension}
        let inst typ memoryRegion =
            let sort = ArrayLengthSort arrayType
            let picker = {sort = sort; extract = extractor; mkName = mkname; isDefaultKey = isDefault; isDefaultRegion = false}
            makeSymbolicHeapRead state picker key state.startingTime typ memoryRegion
        MemoryRegion.read (extractor state) key (isDefault state) inst

    let readLength state address dimension arrayType =
        let cm = state.concreteMemory
        match address.term, dimension.term with
        | ConcreteHeapAddress address, Concrete(:? int as dim, _) when cm.Contains address ->
            cm.ReadArrayLength address dim |> objToTerm state typeof<int>
        | _ -> readLengthSymbolic state address dimension arrayType

    let private readArrayRegion state arrayType extractor region (isDefaultRegion : bool) key =
        let isDefault state (key : heapArrayKey) = isHeapAddressDefault state key.Address
        let instantiate typ memory singleValue =
            let sort = ArrayIndexSort arrayType
            let picker = {sort = sort; extract = extractor; mkName = toString; isDefaultKey = isDefault; isDefaultRegion = isDefaultRegion}
            let time =
                if isValueType typ then state.startingTime
                else MemoryRegion.maxTime region.updates state.startingTime
            makeArraySymbolicHeapRead state picker key time typ memory singleValue
        MemoryRegion.read region key (isDefault state) instantiate

    let private readArrayKeySymbolic state key arrayType =
        let extractor state = accessRegion state.arrays (substituteTypeVariablesIntoArrayType state arrayType) (fst3 arrayType)
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

    let private regionFromData state address data regionType =
        let prepareData (index, value) =
            let key = OneArrayIndexKey(address, List.map (int >> makeNumber) index)
            let value = objToTerm state regionType value
            key, value
        Seq.map prepareData data |> MemoryRegion.memset (MemoryRegion.empty regionType)

    let private readRangeFromConcreteArray state address arrayData fromIndices toIndices arrayType =
        let fromIndices = List.map (fun i -> primitiveCast i typeof<int>) fromIndices
        let toIndices = List.map (fun i -> primitiveCast i typeof<int>) toIndices
        let region = regionFromData state address arrayData (fst3 arrayType)
        let key = RangeArrayIndexKey(address, fromIndices, toIndices)
        readArrayRegion state arrayType (always region) region true key

    let readArrayRange state address fromIndices toIndices arrayType =
        let cm = state.concreteMemory
        match address.term with
        | ConcreteHeapAddress concreteAddress when cm.Contains concreteAddress ->
            let data = cm.GetAllArrayData concreteAddress
            readRangeFromConcreteArray state address data fromIndices toIndices arrayType
        | _ -> readArrayRangeSymbolic state address fromIndices toIndices arrayType

    let private readSymbolicIndexFromConcreteArray state address arrayData indices arrayType =
        let region = regionFromData state address arrayData (fst3 arrayType)
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
            readSymbolicIndexFromConcreteArray state address data indices arrayType
        | _ -> readArrayIndexSymbolic state address indices arrayType

    let private readClassFieldSymbolic state address (field : fieldId) =
        if field = Reflection.stringFirstCharField then
            readArrayIndexSymbolic state address [makeNumber 0] (typeof<char>, 1, true)
        else
            let symbolicType = field.typ
            let extractor state = accessRegion state.classFields (substituteTypeVariablesIntoField state field) (substituteTypeVariables state symbolicType)
            let region = extractor state
            let mkname = fun (key : heapAddressKey) -> sprintf "%O.%O" key.address field
            let isDefault state (key : heapAddressKey) = isHeapAddressDefault state key.address
            let key = {address = address}
            let instantiate typ memory =
                let sort = HeapFieldSort field
                let picker = {sort = sort; extract = extractor; mkName = mkname; isDefaultKey = isDefault; isDefaultRegion = false}
                let time =
                    if isValueType typ then state.startingTime
                    else MemoryRegion.maxTime region.updates state.startingTime
                makeSymbolicHeapRead state picker key time typ memory
            MemoryRegion.read region key (isDefault state) instantiate

    let readClassField (state : state) address (field : fieldId) =
        let cm = state.concreteMemory
        match address.term with
        | ConcreteHeapAddress address when cm.Contains address ->
            cm.ReadClassField address field |> objToTerm state field.typ
        | _ -> readClassFieldSymbolic state address field

    let readStaticField state typ (field : fieldId) =
        let extractor state = accessRegion state.staticFields (substituteTypeVariablesIntoField state field) (substituteTypeVariables state field.typ)
        let mkname = fun (key : symbolicTypeKey) -> sprintf "%O.%O" key.typ field
        let isDefault _ _ = state.complete // TODO: when statics are allocated? always or never? depends on our exploration strategy
        let key = {typ = typ}
        let inst typ memoryRegion =
            let sort = StaticFieldSort field
            let picker = {sort = sort; extract = extractor; mkName = mkname; isDefaultKey = isDefault; isDefaultRegion = false}
            makeSymbolicHeapRead state picker key state.startingTime typ memoryRegion
        MemoryRegion.read (extractor state) key (isDefault state) inst

    let readStackBuffer state (stackKey : stackKey) index =
        let extractor state = accessRegion state.stackBuffers (stackKey.Map (typeVariableSubst state)) typeof<int8>
        let mkname = fun (key : stackBufferIndexKey) -> sprintf "%O[%O]" stackKey key.index
        let isDefault _ _ = true
        let key : stackBufferIndexKey = {index = index}
        let inst typ memoryRegion =
            let sort = StackBufferSort stackKey
            let picker = {sort = sort; extract = extractor; mkName = mkname; isDefaultKey = isDefault; isDefaultRegion = false}
            makeSymbolicHeapRead state picker key state.startingTime typ memoryRegion
        MemoryRegion.read (extractor state) key (isDefault state) inst

    let readBoxedLocation state (address : concreteHeapAddress) =
        let cm = state.concreteMemory
        let typ = typeOfConcreteHeapAddress state address
        match memoryMode, cm.TryVirtToPhys address with
        | ConcreteMemory, Some value -> objToTerm state typ value
        | _ ->
            match PersistentDict.tryFind state.boxedLocations address with
            | Some value -> value
            | None -> internalfailf "Boxed location %O was not found in heap: this should not happen!" address

    let rec readDelegate state reference =
        match reference.term with
        | HeapRef(address, _) ->
            match address.term with
            | ConcreteHeapAddress address -> Some state.delegates.[address]
            | _ -> None
        | Union gvs ->
            let delegates = gvs |> List.choose (fun (g, v) ->
                Option.bind (fun d -> Some(g, d)) (readDelegate state v))
            if delegates.Length = gvs.Length then delegates |> Merging.merge |> Some else None
        | _ -> internalfailf "Reading delegate: expected heap reference, but got %O" reference

    let rec private readSafe state = function
        | PrimitiveStackLocation key -> readStackLocation state key
        | ClassField(address, field) -> readClassField state address field
        // [NOTE] ref must be the most concrete, otherwise region will be not found
        | ArrayIndex(address, indices, typ) -> readArrayIndex state address indices typ
        | StaticField(typ, field) -> readStaticField state typ field
        | StructField(address, field) ->
            let structTerm = readSafe state address
            readStruct structTerm field
        | ArrayLength(address, dimension, typ) -> readLength state address dimension typ
        | BoxedLocation(address, _) -> readBoxedLocation state address
        | StackBufferIndex(key, index) -> readStackBuffer state key index
        | ArrayLowerBound(address, dimension, typ) -> readLowerBound state address dimension typ

// ------------------------------- Unsafe reading -------------------------------

    let private checkBlockBounds state reportError blockSize startByte endByte =
        let failCondition = simplifyGreater endByte blockSize id ||| simplifyLess startByte (makeNumber 0) id
        // NOTE: disables overflow in solver
        let noOvf = makeExpressionNoOvf failCondition
        // TODO: move noOvf to failCondition (failCondition = failCondition || !noOvf) ?
        state.pc <- PC.add state.pc noOvf
        reportError state failCondition

    let private readAddressUnsafe address startByte endByte =
        let size = sizeOf address
        match startByte.term, endByte.term with
        | Concrete(:? int as s, _), Concrete(:? int as e, _) when s = 0 && size = e -> List.singleton address
        | _ -> sprintf "reading: reinterpreting %O" address |> undefinedBehaviour

    // NOTE: returns list of slices
    let rec private readTermUnsafe term startByte endByte =
        match term.term with
        | Struct(fields, t) -> readStructUnsafe fields t startByte endByte
        | HeapRef _
        | Ref _
        | Ptr _ -> readAddressUnsafe term startByte endByte
        | Concrete _
        | Constant _
        | Expression _ -> Slice term startByte endByte startByte |> List.singleton
        | _ -> internalfailf "readTermUnsafe: unexpected term %O" term

    and private readStructUnsafe fields structType startByte endByte =
        let readField fieldId = fields.[fieldId]
        readFieldsUnsafe (makeEmpty false) (fun _ -> __unreachable__()) readField false structType startByte endByte

    and private getAffectedFields state reportError readField isStatic (blockType : Type) startByte endByte =
        let blockSize = CSharpUtils.LayoutUtils.ClassSize blockType
        if isValueType blockType |> not then checkBlockBounds state reportError (makeNumber blockSize) startByte endByte
        let fields = Reflection.fieldsOf isStatic blockType
        let getOffsetAndSize (fieldId, fieldInfo : Reflection.FieldInfo) =
            fieldId, CSharpUtils.LayoutUtils.GetFieldOffset fieldInfo, internalSizeOf fieldInfo.FieldType
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
            fieldId, fieldValue, startByte, endByte
        match startByte.term, endByte.term with
        | Concrete(:? int as s, _), Concrete(:? int as e, _) ->
            let concreteGetField (_, fieldOffset, fieldSize as field) affectedFields =
                if (e > fieldOffset && s < fieldOffset + fieldSize) then
                    getField field :: affectedFields
                else affectedFields
            List.foldBack concreteGetField allFields List.empty
        | _ -> List.map getField allFields

    and private readFieldsUnsafe state reportError readField isStatic (blockType : Type) startByte endByte =
        let affectedFields = getAffectedFields state reportError readField isStatic blockType startByte endByte
        List.collect (fun (_, v, s, e) -> readTermUnsafe v s e) affectedFields

    // TODO: Add undefined behaviour:
    // TODO: 1. when reading info between fields
    // TODO: 3. when reading info outside block
    // TODO: 3. reinterpreting ref or ptr should return symbolic ref or ptr
    let private readClassUnsafe state reportError address classType offset (viewSize : int) =
        let endByte = makeNumber viewSize |> add offset
        let readField fieldId = readClassField state address fieldId
        readFieldsUnsafe state reportError readField false classType offset endByte

    let arrayIndicesToOffset state address (elementType, dim, _ as arrayType) indices =
        let lens = List.init dim (fun dim -> readLength state address (makeNumber dim) arrayType)
        let lbs = List.init dim (fun dim -> readLowerBound state address (makeNumber dim) arrayType)
        let linearIndex = linearizeArrayIndex lens lbs indices
        mul linearIndex (internalSizeOf elementType |> makeNumber)

    let private getAffectedIndices state reportError address (elementType, dim, _ as arrayType) offset viewSize =
        let concreteElementSize = internalSizeOf elementType
        let elementSize = makeNumber concreteElementSize
        let lens = List.init dim (fun dim -> readLength state address (makeNumber dim) arrayType)
        let lbs = List.init dim (fun dim -> readLowerBound state address (makeNumber dim) arrayType)
        let arraySize = List.fold mul elementSize lens
        checkBlockBounds state reportError arraySize offset (makeNumber viewSize |> add offset)
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

    let private readArrayUnsafe state reportError address arrayType offset viewSize =
        let indices = getAffectedIndices state reportError address (symbolicTypeToArrayType arrayType) offset viewSize
        List.collect (fun (_, elem, s, e) -> readTermUnsafe elem s e) indices

    let private readStringUnsafe state reportError address offset viewSize =
         // TODO: handle case, when reading string length
        let indices = getAffectedIndices state reportError address (typeof<char>, 1, true) offset viewSize
        List.collect (fun (_, elem, s, e) -> readTermUnsafe elem s e) indices

    let private readStaticUnsafe state reportError t offset (viewSize : int) =
        let endByte = makeNumber viewSize |> add offset
        let readField fieldId = readStaticField state t fieldId
        readFieldsUnsafe state reportError readField true t offset endByte

    let private readStackUnsafe state reportError loc offset (viewSize : int) =
        let term = readStackLocation state loc
        let locSize = sizeOf term |> makeNumber
        let endByte = makeNumber viewSize |> add offset
        checkBlockBounds state reportError locSize offset endByte
        readTermUnsafe term offset endByte

    let private readBoxedUnsafe state loc typ offset viewSize =
        let address =
            match loc.term with
            | ConcreteHeapAddress address -> BoxedLocation(address, typ)
            | _ -> internalfail "readUnsafe: boxed value type case is not fully implemented"
        let endByte = makeNumber viewSize |> add offset
        match readSafe state address with
        | {term = Struct(fields, _)} -> readStructUnsafe fields typ offset endByte
        | term when isPrimitive typ || typ.IsEnum -> readTermUnsafe term offset endByte
        | term -> internalfail $"readUnsafe: reading struct resulted in term {term}"

    let private readUnsafe state reportError baseAddress offset sightType =
        let viewSize = internalSizeOf sightType
        let slices =
            match baseAddress with
            | HeapLocation(loc, sightType) ->
                let typ = mostConcreteTypeOfHeapRef state loc sightType
                match typ with
                | StringType -> readStringUnsafe state reportError loc offset viewSize
                | ClassType _ -> readClassUnsafe state reportError loc typ offset viewSize
                | ArrayType _ -> readArrayUnsafe state reportError loc typ offset viewSize
                | StructType _ -> readBoxedUnsafe state loc typ offset viewSize
                | _ when isPrimitive typ || typ.IsEnum ->
                    readBoxedUnsafe state loc typ offset viewSize
                | _ -> internalfailf "expected complex type, but got %O" typ
            | StackLocation loc -> readStackUnsafe state reportError loc offset viewSize
            | StaticLocation loc -> readStaticUnsafe state reportError loc offset viewSize
        combine slices sightType

// --------------------------- General reading ---------------------------

    let isTypeInitialized state (typ : Type) =
        let key : symbolicTypeKey = {typ=typ}
        let matchingTypes = SymbolicSet.matchingElements key state.initializedTypes
        match matchingTypes with
        | [x] when x = key -> True
        | _ ->
            let name = sprintf "%O_initialized" typ
            let source : typeInitialized = {typ = typ; matchingTypes = SymbolicSet.ofSeq matchingTypes}
            Constant name source typeof<bool>

    // TODO: take type of heap address
    let rec read state reportError reference =
        match reference.term with
        | Ref address -> readSafe state address
        | Ptr(baseAddress, sightType, offset) -> readUnsafe state reportError baseAddress offset sightType
        | Union gvs -> gvs |> List.map (fun (g, v) -> (g, read state reportError v)) |> Merging.merge
        | _ -> internalfailf "Safe reading: expected reference, but got %O" reference

// ------------------------------- Writing -------------------------------

    let rec private ensureConcreteType typ =
        if isOpenType typ then __insufficientInformation__ "Cannot write value of generic type %O" typ

    let writeStackLocation (s : state) key value =
        s.stack <- CallStack.writeStackLocation s.stack key value

    let writeStruct (structTerm : term) (field : fieldId) value =
        match structTerm with
        | { term = Struct(fields, typ) } -> Struct (PersistentDict.add field value fields) typ
        | _ -> internalfailf "Writing field of structure: expected struct, but got %O" structTerm

    let private writeClassFieldSymbolic state address (field : fieldId) value =
        ensureConcreteType field.typ
        let mr = accessRegion state.classFields field field.typ
        let key = {address = address}
        let mr' = MemoryRegion.write mr key value
        state.classFields <- PersistentDict.add field mr' state.classFields

    let private writeArrayKeySymbolic state key arrayType value =
        let elementType = fst3 arrayType
        ensureConcreteType elementType
        let mr = accessRegion state.arrays arrayType elementType
        let mr' = MemoryRegion.write mr key value
        state.arrays <- PersistentDict.add arrayType mr' state.arrays

    let private writeArrayIndexSymbolic state address indices arrayType value =
        let indices = List.map (fun i -> primitiveCast i typeof<int>) indices
        let key = OneArrayIndexKey(address, indices)
        writeArrayKeySymbolic state key arrayType value

    let private writeArrayRangeSymbolic state address fromIndices toIndices arrayType value =
        let fromIndices = List.map (fun i -> primitiveCast i typeof<int>) fromIndices
        let toIndices = List.map (fun i -> primitiveCast i typeof<int>) toIndices
        let key = RangeArrayIndexKey(address, fromIndices, toIndices)
        writeArrayKeySymbolic state key arrayType value

    let initializeArray state address indicesAndValues arrayType =
        let elementType = fst3 arrayType
        ensureConcreteType elementType
        let mr = accessRegion state.arrays arrayType elementType
        let keysAndValues = Seq.map (fun (i, v) -> OneArrayIndexKey(address, i), v) indicesAndValues
        let mr' = MemoryRegion.memset mr keysAndValues
        state.arrays <- PersistentDict.add arrayType mr' state.arrays

    let writeStaticField state typ (field : fieldId) value =
        ensureConcreteType field.typ
        let mr = accessRegion state.staticFields field field.typ
        let key = {typ = typ}
        let mr' = MemoryRegion.write mr key value
        state.staticFields <- PersistentDict.add field mr' state.staticFields

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

    let writeBoxedLocationSymbolic state (address : concreteHeapAddress) value =
        state.boxedLocations <- PersistentDict.add address value state.boxedLocations

    let writeBoxedLocation state (address : concreteHeapAddress) value =
        let cm = state.concreteMemory
        match memoryMode, tryTermToObj state value with
        | ConcreteMemory, Some value when cm.Contains(address) ->
            cm.Remove address
            cm.Allocate address value
        | ConcreteMemory, Some value ->
            cm.Allocate address value
        | ConcreteMemory, None when cm.Contains(address) ->
            cm.Remove address
            writeBoxedLocationSymbolic state address value
        | _ -> writeBoxedLocationSymbolic state address value

// ----------------- Unmarshalling: from concrete to symbolic memory -----------------

    let private unmarshallClass (state : state) address obj =
        let writeField state (fieldId, fieldInfo : Reflection.FieldInfo) =
            let value = fieldInfo.GetValue obj |> objToTerm state fieldInfo.FieldType
            writeClassFieldSymbolic state address fieldId value
        let fields = obj.GetType() |> Reflection.fieldsOf false
        Array.iter (writeField state) fields

    let private unmarshallArray (state : state) address (array : Array) =
        let elemType, dim, _ as arrayType = array.GetType() |> symbolicTypeToArrayType
        let lbs = List.init dim array.GetLowerBound
        let lens = List.init dim array.GetLength
        let writeIndex state (indices : int list) =
            let value = array.GetValue(Array.ofList indices) |> objToTerm state elemType
            let termIndices = List.map makeNumber indices
            writeArrayIndexSymbolic state address termIndices arrayType value
        let allIndices = Array.allIndicesViaLens lbs lens
        Seq.iter (writeIndex state) allIndices
        let termLBs = List.map (objToTerm state typeof<int>) lbs
        let termLens = List.map (objToTerm state typeof<int>) lens
        fillArrayBoundsSymbolic state address termLens termLBs arrayType

    let private unmarshallString (state : state) address (string : string) =
        let concreteStringLength = string.Length
        let stringLength = makeNumber concreteStringLength
        let arrayLength = makeNumber (concreteStringLength + 1)
        let tChar = typeof<char>
        let arrayType = (tChar, 1, true)
        let zero = makeNumber 0
        let writeChars state i value =
            writeArrayIndexSymbolic state address [Concrete i lengthType] arrayType (Concrete value tChar)
        Seq.iteri (writeChars state) string
        writeLengthSymbolic state address zero arrayType arrayLength
        writeLowerBoundSymbolic state address zero arrayType zero
        writeArrayIndexSymbolic state address [stringLength] (tChar, 1, true) (Concrete '\000' tChar)
        writeClassFieldSymbolic state address Reflection.stringLengthField stringLength

    let unmarshall (state : state) concreteAddress =
        let address = ConcreteHeapAddress concreteAddress
        let cm = state.concreteMemory
        let obj = cm.VirtToPhys concreteAddress
        assert(box obj <> null)
        match obj with
        | :? Array as array -> unmarshallArray state address array
        | :? String as string -> unmarshallString state address string
        | _ -> unmarshallClass state address obj
        cm.Remove concreteAddress

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

    let private writeAddressUnsafe address startByte value =
        let addressSize = sizeOf address
        let valueSize = sizeOf value
        match startByte.term with
        | Concrete(:? int as s, _) when s = 0 && addressSize = valueSize -> value
        | _ -> sprintf "writing: reinterpreting %O" term |> undefinedBehaviour

    let rec writeTermUnsafe term startByte value =
        match term.term with
        | Struct(fields, t) -> writeStructUnsafe term fields t startByte value
        | HeapRef _
        | Ref _
        | Ptr _ -> writeAddressUnsafe term startByte value
        | Concrete _
        | Constant _
        | Expression _ ->
            let termType = typeOf term
            let termSize = internalSizeOf termType
            let valueSize = sizeOf value
            match startByte.term with
            | Concrete(:? int as startByte, _) when startByte = 0 && valueSize = termSize -> value
            | _ ->
                let zero = makeNumber 0
                let termSize = internalSizeOf termType |> makeNumber
                let valueSize = sizeOf value |> makeNumber
                let left = Slice term zero startByte zero
                let valueSlices = readTermUnsafe value (neg startByte) (sub termSize startByte)
                let right = Slice term (add startByte valueSize) termSize zero
                combine ([left] @ valueSlices @ [right]) termType
        | _ -> internalfailf "writeTermUnsafe: unexpected term %O" term

    and private writeStructUnsafe structTerm fields structType startByte value =
        let readField fieldId = fields.[fieldId]
        let updatedFields = writeFieldsUnsafe (makeEmpty false) (fun _ -> __unreachable__()) readField false structType startByte value
        let writeField structTerm (fieldId, value) = writeStruct structTerm fieldId value
        List.fold writeField structTerm updatedFields

    and private writeFieldsUnsafe state reportError readField isStatic (blockType : Type) startByte value =
        let endByte = sizeOf value |> makeNumber |> add startByte
        let affectedFields = getAffectedFields state reportError readField isStatic blockType startByte endByte
        List.map (fun (id, v, s, _) -> id, writeTermUnsafe v s value) affectedFields

    let writeClassUnsafe state reportError address typ offset value =
        let readField fieldId = readClassField state address fieldId
        let updatedFields = writeFieldsUnsafe state reportError readField false typ offset value
        let writeField (fieldId, value) = writeClassField state address fieldId value
        List.iter writeField updatedFields

    let writeArrayUnsafe state reportError address arrayType offset value =
        let size = sizeOf value
        let arrayType = symbolicTypeToArrayType arrayType
        let affectedIndices = getAffectedIndices state reportError address arrayType offset size
        let writeElement (index, element, startByte, _) =
            let updatedElement = writeTermUnsafe element startByte value
            writeArrayIndex state address index arrayType updatedElement
        List.iter writeElement affectedIndices

    let private writeStringUnsafe state reportError address offset value =
        let size = sizeOf value
        let arrayType = typeof<char>, 1, true
        let affectedIndices = getAffectedIndices state reportError address arrayType offset size
        let writeElement (index, element, startByte, _) =
            let updatedElement = writeTermUnsafe element startByte value
            writeArrayIndex state address index arrayType updatedElement
        List.iter writeElement affectedIndices

    let writeStaticUnsafe state reportError staticType offset value =
        let readField fieldId = readStaticField state staticType fieldId
        let updatedFields = writeFieldsUnsafe state reportError readField true staticType offset value
        let writeField (fieldId, value) = writeStaticField state staticType fieldId value
        List.iter writeField updatedFields

    let writeStackUnsafe state reportError loc offset value =
        let term = readStackLocation state loc
        let locSize = sizeOf term |> makeNumber
        let endByte = sizeOf value |> makeNumber |> add offset
        checkBlockBounds state reportError locSize offset endByte
        let updatedTerm = writeTermUnsafe term offset value
        writeStackLocation state loc updatedTerm

    let private writeUnsafe state reportError baseAddress offset sightType value =
        assert(sightType = typeof<Void> && isConcrete offset || int (internalSizeOf sightType) = sizeOf value)
        match baseAddress with
        | HeapLocation(loc, sightType) ->
            let typ = mostConcreteTypeOfHeapRef state loc sightType
            match typ with
            | StringType -> writeStringUnsafe state reportError loc offset value
            | ClassType _ -> writeClassUnsafe state reportError loc typ offset value
            | ArrayType _ -> writeArrayUnsafe state reportError loc typ offset value
            | StructType _ -> internalfail "writeUnsafe: unsafe writing is not implemented for structs" // TODO: boxed location?
            | _ -> internalfailf "expected complex type, but got %O" typ
        | StackLocation loc -> writeStackUnsafe state reportError loc offset value
        | StaticLocation loc -> writeStaticUnsafe state reportError loc offset value

// ------------------------------- General writing -------------------------------

    // NOTE: using unsafe write instead of safe, when field intersects,
    // because need to write to all fields, which intersects with 'field'
    let private writeIntersectingField state address (field : fieldId) value =
        let baseAddress, offset = Pointers.addressToBaseAndOffset address
        let ptr = Ptr baseAddress field.typ offset
        match ptr.term with
        | Ptr(baseAddress, sightType, offset) -> writeUnsafe state (fun _ _ -> ()) baseAddress offset sightType value
        | _ -> internalfailf "expected to get ptr, but got %O" ptr

    let rec private writeSafe state address value =
        match address with
        | PrimitiveStackLocation key -> writeStackLocation state key value
        | ClassField(_, field)
        | StructField(_, field) when Reflection.fieldIntersects field ->
            writeIntersectingField state address field value
        | ClassField(address, field) -> writeClassField state address field value
        | ArrayIndex(address, indices, typ) -> writeArrayIndex state address indices typ value
        | StaticField(typ, field) -> writeStaticField state typ field value
        | StructField(address, field) ->
            let oldStruct = readSafe state address
            let newStruct = writeStruct oldStruct field value
            writeSafe state address newStruct
        // TODO: need concrete memory for BoxedLocation?
        | BoxedLocation(address, _) -> writeBoxedLocation state address value
        | StackBufferIndex(key, index) -> writeStackBuffer state key index value
        // NOTE: Cases below is needed to construct a model
        | ArrayLength(address, dimension, typ) -> writeLengthSymbolic state address dimension typ value
        | ArrayLowerBound(address, dimension, typ) -> writeLowerBoundSymbolic state address dimension typ value

    let write state reportError reference value =
        match reference.term with
        | Ref address -> writeSafe state address value
        | Ptr(address, sightType, offset) -> writeUnsafe state reportError address offset sightType value
        | _ -> internalfailf "Writing: expected reference, but got %O" reference
        state

// ------------------------------- Allocation -------------------------------

    let allocateOnStack state key term =
        state.stack <- CallStack.allocate state.stack key term

    // Strings and delegates should be allocated using the corresponding functions (see allocateString and allocateDelegate)!
    let allocateClass state typ =
        assert (not <| isSubtypeOrEqual typ typeof<String>)
        assert (not <| isSubtypeOrEqual typ typeof<Delegate>)
        let concreteAddress = allocateType state typ
        match memoryMode with
        // TODO: it's hack for reflection, remove it after concolic will be implemented
        | _ when isSubtypeOrEqual typ typeof<Type> -> ()
        | ConcreteMemory -> Reflection.createObject typ |> state.concreteMemory.Allocate concreteAddress
        | SymbolicMemory -> ()
        HeapRef (ConcreteHeapAddress concreteAddress) typ

    // TODO: unify allocation with unmarshalling
    let allocateArray state typ lowerBounds lengths =
        assert (isSubtypeOrEqual typ typeof<Array>)
        let concreteAddress = allocateType state typ
        let arrayType = symbolicTypeToArrayType typ
        let address = ConcreteHeapAddress concreteAddress
        let concreteLengths = tryIntListFromTermList lengths
        let concreteLowerBounds = tryIntListFromTermList lowerBounds
        match memoryMode, concreteLengths, concreteLowerBounds with
        | ConcreteMemory, Some concreteLengths, Some concreteLBs ->
            let elementDotNetType = elementType typ
            let array = Array.CreateInstance(elementDotNetType, Array.ofList concreteLengths, Array.ofList concreteLBs) :> obj
            state.concreteMemory.Allocate concreteAddress array
        | _ -> fillArrayBoundsSymbolic state address lengths lowerBounds arrayType
        address

    let allocateVector state (elementType : Type) length =
        let typ = elementType.MakeArrayType()
        allocateArray state typ [makeNumber 0] [length]

    let allocateConcreteVector state (elementType : Type) length contents =
        match memoryMode, length.term with
        | ConcreteMemory, Concrete(:? int as intLength, _) ->
            let concreteAddress = allocateType state (elementType.MakeArrayType())
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
        match memoryMode, length.term with
        | ConcreteMemory, Concrete(:? int as intLength, _) ->
            // TODO: implement interning (for String.Empty)
            let charArray : char array = Array.create intLength '\000'
            Seq.iteri (fun i char -> charArray.SetValue(char, i)) contents
            let string = new string(charArray) :> obj
            allocateObjectIfNeed state string typeof<string>
        | _ ->
            let arrayLength = add length (Concrete 1 lengthType)
            let address = allocateConcreteVector state typeof<char> arrayLength contents
            writeArrayIndexSymbolic state address [length] (typeof<char>, 1, true) (Concrete '\000' typeof<char>)
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
            let address = commonAllocateString state (makeNumber 1) " "
            writeArrayIndex state address [Concrete 0 indexType] (typeof<char>, 1, true) char
            HeapRef address typeof<string>

    let allocateDelegate state delegateTerm =
        let typ = typeOf delegateTerm
        let concreteAddress = allocateType state typ
        let address = ConcreteHeapAddress concreteAddress
        // TODO: create real Delegate objects and use concrete memory
        state.delegates <- PersistentDict.add concreteAddress delegateTerm state.delegates
        HeapRef address typ

    let allocateBoxedLocation state value =
        let typ = typeOf value
        let concreteAddress = allocateType state typ
        let address = ConcreteHeapAddress concreteAddress
        match memoryMode, tryTermToObj state value with
        // 'value' may be null, if it's nullable value type
        | ConcreteMemory, Some value when value <> null ->
            state.concreteMemory.Allocate concreteAddress value
        | _ -> writeBoxedLocationSymbolic state concreteAddress value
        HeapRef address typeof<obj>

    let allocateConcreteObject state obj (typ : Type) =
        assert(not typ.IsAbstract)
        match memoryMode with
        | ConcreteMemory ->
            let address = allocateObjectIfNeed state obj typ
            HeapRef address typ
        | SymbolicMemory -> internalfailf "allocateConcreteObject: allocating concrete object %O in symbolic memory is not implemented" obj

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
        | Union gvs -> gvs |> List.map (fun (g, v) -> (g, lengthOfString state v)) |> Merging.merge
        | _ -> internalfail "Getting length of string: expected heap reference, but got %O" heapRef

    let initializeStaticMembers state typ =
        if typ = typeof<string> then
            let reference = allocateString state ""
            writeStaticField state typeof<string> Reflection.emptyStringField reference
        state.initializedTypes <- SymbolicSet.add {typ=typ} state.initializedTypes

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
                let key = x.key :> IMemoryKey<heapArrayKey, productRegion<vectorTime intervals, int points listProductRegion>>
                let key = key.Map substTerm substType substTime key.Region |> snd
                let afters =
                    if not x.picker.isDefaultRegion then
                        let effect = MemoryRegion.map substTerm substType substTime x.memoryObject
                        let before = x.picker.extract state
                        MemoryRegion.compose before effect
                    else List.singleton (True, x.memoryObject)
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

    type private structField with
        interface IMemoryAccessConstantSource with
            override x.Compose state =
                match x.baseSource with
                | :? IStatedSymbolicConstantSource as baseSource ->
                    let structTerm = baseSource.Compose state
                    readStruct structTerm x.field
                | _ ->
                    let x = x :> ISymbolicConstantSource
                    match state.model with
                    | PrimitiveModel subst when state.complete ->
                        let value = ref Nop
                        if subst.TryGetValue(x, value) then value.Value
                        else makeDefaultValue x.TypeOfLocation
                    | _ ->
                        makeSymbolicValue x (x.ToString()) x.TypeOfLocation

    type private heapAddressSource with
        interface IMemoryAccessConstantSource  with
            override x.Compose state =
                let refTerm =
                    match x.baseSource with
                    | :? IStatedSymbolicConstantSource as baseSource ->
                        baseSource.Compose state
                    | src ->
                        match state.model with
                        | PrimitiveModel subst when state.complete ->
                            let value = ref Nop
                            if subst.TryGetValue(x, value) then value.Value
                            else makeDefaultValue src.TypeOfLocation
                        | _ ->
                            makeSymbolicValue src (src.ToString()) src.TypeOfLocation
                extractAddress refTerm

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
            |> PersistentDict.fold composeOneRegion [(True, dict)]

    let private composeBoxedLocations state state' =
        state'.boxedLocations |> PersistentDict.fold (fun acc k v -> PersistentDict.add (composeTime state k) (fillHoles state v) acc) state.boxedLocations

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
            
        let externMocks = Dictionary()
        for kvp in state.externMocks do
            externMocks.Add(kvp.Key, kvp.Value)
        for kvp in state'.externMocks do
            externMocks.Add(kvp.Key, kvp.Value)
            
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
            let boxedLocations = composeBoxedLocations state state'
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
                    boxedLocations = boxedLocations
                    initializedTypes = initializedTypes
                    concreteMemory = state.concreteMemory
                    allocatedTypes = allocatedTypes
                    typeVariables = typeVariables
                    delegates = delegates
                    currentTime = currentTime
                    startingTime = state.startingTime
                    model = state.model // TODO: compose models (for example, mocks)
                    complete = state.complete
                    methodMocks = methodMocks
                    externMocks = externMocks
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
        let sb = dumpDict "Boxed items" sortVectorTime VectorTime.print toString sb s.boxedLocations
        let sb = dumpDict "Types tokens" sortVectorTime VectorTime.print toString sb s.allocatedTypes
        let sb = dumpDict "Static fields" (sortBy toString) toString (MemoryRegion.toString "    ") sb s.staticFields
        let sb = dumpDict "Delegates" sortVectorTime VectorTime.print toString sb s.delegates
        let sb = dumpStack sb s.stack
        let sb = dumpInitializedTypes sb s.initializedTypes
        let sb = dumpEvaluationStack sb s.evaluationStack
        if sb.Length = 0 then "<Empty>"
        else
            System.Text.RegularExpressions.Regex.Replace(sb.ToString(), @"@\d+(\+|\-)\d*\[Microsoft.FSharp.Core.Unit\]", "");
