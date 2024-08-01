namespace VSharp.Core

open System
open System.Collections
open System.Collections.Generic
open System.Reflection
open FSharpx.Collections
open VSharp
open VSharp.Core
open VSharp.TypeUtils
open VSharp.Utils
open DictionaryType
open SetType

#nowarn "69"

type IMemoryAccessConstantSource =
    inherit IStatedSymbolicConstantSource

module internal Memory =

// ------------------------------- Primitives -------------------------------

    let private isZeroAddress (x : concreteHeapAddress) =
        x = VectorTime.zero

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

// -------------------------------- GetHashCode --------------------------------

    [<StructuralEquality;NoComparison>]
    type private hashCodeSource =
        {object : term}
        interface INonComposableSymbolicConstantSource with
            override x.SubTerms = List.empty
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

    type [<CustomEquality;NoComparison>] regionPicker<'key, 'reg
        when 'key : equality
        and 'key :> IMemoryKey<'key, 'reg>
        and 'reg : equality
        and 'reg :> IRegion<'reg>> =
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

    let rec extractAddress reference =
        match reference.term with
        | HeapRef(address, _) -> address
        | Ptr(HeapLocation(address, _), _, _) -> address
        | Ite gvs -> Merging.guardedMap extractAddress gvs
        | _ -> internalfail $"Extracting heap address: expected heap reference or pointer, but got {reference}"

    let rec extractPointerOffset ptr =
        match ptr.term with
        | Ptr(_, _, offset) -> offset
        | Ref address -> Pointers.addressToBaseAndOffset address |> snd
        | HeapRef _ -> makeNumber 0
        | Ite gvs -> Merging.guardedMap extractPointerOffset gvs
        | _ -> internalfail $"Extracting pointer offset: expected reference or pointer, but got {ptr}"

    [<StructuralEquality;NoComparison>]
    type private stackReading =
        {key : stackKey; time : vectorTime}
        interface IMemoryAccessConstantSource with
            override x.SubTerms = List.empty
            override x.Time = x.time
            override x.TypeOfLocation = x.key.TypeOfLocation

    // -------------------------------- Reading --------------------------------

    [<StructuralEquality;NoComparison>]
    type private heapReading<'key, 'reg when 'key : equality and 'key :> IMemoryKey<'key, 'reg> and 'reg : equality and 'reg :> IRegion<'reg>> =
        {picker : regionPicker<'key, 'reg>; key : 'key; memoryObject : memoryRegion<'key, 'reg>; time : vectorTime}
        interface IMemoryAccessConstantSource with
            override x.SubTerms = List.empty
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
            override x.SubTerms = List.empty
            override x.Time = x.time
            override x.TypeOfLocation = x.picker.sort.TypeOfLocation

    type private generalDictionaryReading<'key when 'key : equality> = heapReading<heapCollectionKey<'key>, productRegion<vectorTime intervals, 'key points>>
    type private addrDictionaryReading = heapReading<addrCollectionKey, productRegion<vectorTime intervals, vectorTime intervals>>
    type private dictionaryCountReading = heapReading<heapAddressKey, vectorTime intervals>

    type private generalSetReading<'key when 'key : equality> = heapReading<heapCollectionKey<'key>, productRegion<vectorTime intervals, 'key points>>
    type private addrSetReading = heapReading<addrCollectionKey, productRegion<vectorTime intervals, vectorTime intervals>>
    type private setCountReading = heapReading<heapAddressKey, vectorTime intervals>

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

    let (|BoolDictionaryReading|_|) (src : ISymbolicConstantSource) =
        match src with
        | :? generalDictionaryReading<bool> as dr -> Some(isConcreteHeapAddress dr.key.address, dr.key, dr.memoryObject)
        | _ -> None

    let (|ByteDictionaryReading|_|) (src : ISymbolicConstantSource) =
        match src with
        | :? generalDictionaryReading<byte> as dr -> Some(isConcreteHeapAddress dr.key.address, dr.key, dr.memoryObject)
        | _ -> None

    let (|SByteDictionaryReading|_|) (src : ISymbolicConstantSource) =
        match src with
        | :? generalDictionaryReading<sbyte> as dr -> Some(isConcreteHeapAddress dr.key.address, dr.key, dr.memoryObject)
        | _ -> None

    let (|CharDictionaryReading|_|) (src : ISymbolicConstantSource) =
        match src with
        | :? generalDictionaryReading<char> as dr -> Some(isConcreteHeapAddress dr.key.address, dr.key, dr.memoryObject)
        | _ -> None

    let (|DecimalDictionaryReading|_|) (src : ISymbolicConstantSource) =
        match src with
        | :? generalDictionaryReading<decimal> as dr -> Some(isConcreteHeapAddress dr.key.address, dr.key, dr.memoryObject)
        | _ -> None

    let (|DoubleDictionaryReading|_|) (src : ISymbolicConstantSource) =
        match src with
        | :? generalDictionaryReading<double> as dr -> Some(isConcreteHeapAddress dr.key.address, dr.key, dr.memoryObject)
        | _ -> None

    let (|IntDictionaryReading|_|) (src : ISymbolicConstantSource) =
        match src with
        | :? generalDictionaryReading<int> as dr -> Some(isConcreteHeapAddress dr.key.address, dr.key, dr.memoryObject)
        | _ -> None

    let (|UIntDictionaryReading|_|) (src : ISymbolicConstantSource) =
        match src with
        | :? generalDictionaryReading<uint> as dr -> Some(isConcreteHeapAddress dr.key.address, dr.key, dr.memoryObject)
        | _ -> None

    let (|LongDictionaryReading|_|) (src : ISymbolicConstantSource) =
        match src with
        | :? generalDictionaryReading<int64> as dr -> Some(isConcreteHeapAddress dr.key.address, dr.key, dr.memoryObject)
        | _ -> None

    let (|ULongDictionaryReading|_|) (src : ISymbolicConstantSource) =
        match src with
        | :? generalDictionaryReading<uint64> as dr -> Some(isConcreteHeapAddress dr.key.address, dr.key, dr.memoryObject)
        | _ -> None

    let (|ShortDictionaryReading|_|) (src : ISymbolicConstantSource) =
        match src with
        | :? generalDictionaryReading<int16> as dr -> Some(isConcreteHeapAddress dr.key.address, dr.key, dr.memoryObject)
        | _ -> None

    let (|UShortDictionaryReading|_|) (src : ISymbolicConstantSource) =
        match src with
        | :? generalDictionaryReading<uint16> as dr -> Some(isConcreteHeapAddress dr.key.address, dr.key, dr.memoryObject)
        | _ -> None

    let (|AddrDictionaryReading|_|) (src : ISymbolicConstantSource) =
        match src with
        | :? addrDictionaryReading as dr -> Some(isConcreteHeapAddress dr.key.address, dr.key, dr.memoryObject)
        | _ -> None

    let (|BoolSetReading|_|) (src : ISymbolicConstantSource) =
        match src with
        | :? generalSetReading<bool> as st -> Some(isConcreteHeapAddress st.key.address, st.key, st.memoryObject)
        | _ -> None

    let (|ByteSetReading|_|) (src : ISymbolicConstantSource) =
        match src with
        | :? generalSetReading<byte> as st -> Some(isConcreteHeapAddress st.key.address, st.key, st.memoryObject)
        | _ -> None

    let (|SByteSetReading|_|) (src : ISymbolicConstantSource) =
        match src with
        | :? generalSetReading<sbyte> as st -> Some(isConcreteHeapAddress st.key.address, st.key, st.memoryObject)
        | _ -> None

    let (|CharSetReading|_|) (src : ISymbolicConstantSource) =
        match src with
        | :? generalSetReading<char> as st -> Some(isConcreteHeapAddress st.key.address, st.key, st.memoryObject)
        | _ -> None

    let (|DecimalSetReading|_|) (src : ISymbolicConstantSource) =
        match src with
        | :? generalSetReading<decimal> as st -> Some(isConcreteHeapAddress st.key.address, st.key, st.memoryObject)
        | _ -> None

    let (|DoubleSetReading|_|) (src : ISymbolicConstantSource) =
        match src with
        | :? generalSetReading<double> as st -> Some(isConcreteHeapAddress st.key.address, st.key, st.memoryObject)
        | _ -> None

    let (|IntSetReading|_|) (src : ISymbolicConstantSource) =
        match src with
        | :? generalSetReading<int> as st -> Some(isConcreteHeapAddress st.key.address, st.key, st.memoryObject)
        | _ -> None

    let (|UIntSetReading|_|) (src : ISymbolicConstantSource) =
        match src with
        | :? generalSetReading<uint> as st -> Some(isConcreteHeapAddress st.key.address, st.key, st.memoryObject)
        | _ -> None

    let (|LongSetReading|_|) (src : ISymbolicConstantSource) =
        match src with
        | :? generalSetReading<int64> as st -> Some(isConcreteHeapAddress st.key.address, st.key, st.memoryObject)
        | _ -> None

    let (|ULongSetReading|_|) (src : ISymbolicConstantSource) =
        match src with
        | :? generalSetReading<uint64> as st -> Some(isConcreteHeapAddress st.key.address, st.key, st.memoryObject)
        | _ -> None

    let (|ShortSetReading|_|) (src : ISymbolicConstantSource) =
        match src with
        | :? generalSetReading<int16> as st -> Some(isConcreteHeapAddress st.key.address, st.key, st.memoryObject)
        | _ -> None

    let (|UShortSetReading|_|) (src : ISymbolicConstantSource) =
        match src with
        | :? generalSetReading<uint16> as st -> Some(isConcreteHeapAddress st.key.address, st.key, st.memoryObject)
        | _ -> None

    let (|AddrSetReading|_|) (src : ISymbolicConstantSource) =
        match src with
        | :? addrSetReading as st -> Some(isConcreteHeapAddress st.key.address, st.key, st.memoryObject)
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
        | :? heapReading<heapAddressKey, vectorTime intervals> as hr -> hr.picker.sort
        | :? heapReading<heapVectorIndexKey, productRegion<vectorTime intervals, int points>> as hr -> hr.picker.sort
        | :? heapReading<stackBufferIndexKey, int points> as hr -> hr.picker.sort
        | :? heapReading<symbolicTypeKey, freeRegion<typeWrapper>> as hr -> hr.picker.sort
        | :? heapReading<heapArrayKey, productRegion<vectorTime intervals, int points listProductRegion>> ->
            internalfail "unexpected array index reading via 'heapReading' source"
        | :? arrayReading as ar -> ar.picker.sort
        | :? generalDictionaryReading<bool> as dr -> dr.picker.sort
        | :? generalDictionaryReading<byte> as dr -> dr.picker.sort
        | :? generalDictionaryReading<sbyte> as dr -> dr.picker.sort
        | :? generalDictionaryReading<char> as dr -> dr.picker.sort
        | :? generalDictionaryReading<decimal> as dr -> dr.picker.sort
        | :? generalDictionaryReading<double> as dr -> dr.picker.sort
        | :? generalDictionaryReading<int> as dr -> dr.picker.sort
        | :? generalDictionaryReading<uint> as dr -> dr.picker.sort
        | :? generalDictionaryReading<int64> as dr -> dr.picker.sort
        | :? generalDictionaryReading<uint64> as dr -> dr.picker.sort
        | :? generalDictionaryReading<int16> as dr -> dr.picker.sort
        | :? generalDictionaryReading<uint16> as dr -> dr.picker.sort
        | :? addrDictionaryReading as dr -> dr.picker.sort
        | :? generalSetReading<bool> as st -> st.picker.sort
        | :? generalSetReading<byte> as st -> st.picker.sort
        | :? generalSetReading<sbyte> as st -> st.picker.sort
        | :? generalSetReading<char> as st -> st.picker.sort
        | :? generalSetReading<decimal> as st -> st.picker.sort
        | :? generalSetReading<double> as st -> st.picker.sort
        | :? generalSetReading<int> as st -> st.picker.sort
        | :? generalSetReading<uint> as st -> st.picker.sort
        | :? generalSetReading<int64> as st -> st.picker.sort
        | :? generalSetReading<uint64> as st -> st.picker.sort
        | :? generalSetReading<int16> as st -> st.picker.sort
        | :? generalSetReading<uint16> as st -> st.picker.sort
        | :? addrSetReading as st -> st.picker.sort
        | _ -> __unreachable__()

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
                state.memory.MakeSymbolicValue baseSource (baseSource.ToString()) baseSource.TypeOfLocation

    [<StructuralEquality;NoComparison>]
    type private structField =
        {baseSource : ISymbolicConstantSource; field : fieldId}
        interface IMemoryAccessConstantSource with
            override x.SubTerms = x.baseSource.SubTerms
            override x.Time = x.baseSource.Time
            override x.TypeOfLocation = x.field.typ

            override x.Compose state =
                let structTerm = composeBaseSource state x.baseSource
                state.memory.ReadStruct emptyReporter structTerm x.field

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
            override x.Compose state =
                composeBaseSource state x.baseSource |> extractAddress

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
            override x.Compose state =
                composeBaseSource state x.baseSource |> extractAddress

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
            override x.Compose state =
                composeBaseSource state x.baseSource |> extractPointerOffset

    let (|PointerOffsetSource|_|) (src : ISymbolicConstantSource) =
        match src with
        | :? pointerOffsetSource as address -> Some(address.baseSource)
        | _ -> None
    let (|TypeInitializedSource|_|) (src : IStatedSymbolicConstantSource) =
        match src with
        | :? State.typeInitialized as ti -> Some(ti.typ, ti.matchingTypes)
        | _ -> None

    let specializeWithKey constant (key : heapArrayKey) (writeKey : heapArrayKey) =
        match constant.term with
        | HeapRef({term = Constant(_, HeapAddressSource(ArrayRangeReading(mo, srcAddress, srcFrom, srcTo, picker, time)), typ)}, _)
        | Constant(_, ArrayRangeReading(mo, srcAddress, srcFrom, srcTo, picker, time), typ) ->
            let key = key.Specialize writeKey srcAddress srcFrom srcTo
            let source : arrayReading = {picker = picker; key = key; memoryObject = mo; time = time}
            let name = picker.mkName key
            Constant name source typ
        | _ -> constant

    let private isHeapAddressDefault state address =
        state.complete ||
        match address.term with
        | ConcreteHeapAddress address -> VectorTime.less state.startingTime address
        | _ -> false

    let rec heapReferenceToBoxReference reference =
        match reference.term with
        | HeapRef(address, typ) ->
            assert(isBoxedType typ)
            Ref (BoxedLocation(address, typ))
        | Ite iteType -> Merging.guardedMap heapReferenceToBoxReference iteType
        | _ -> internalfailf $"Unboxing: expected heap reference, but got {reference}"
    let transformBoxedRef ref =
        match ref.term with
        | HeapRef _ -> heapReferenceToBoxReference ref
        | _ -> ref

    let private ensureConcreteType typ =
        if isOpenType typ then __insufficientInformation__ $"Cannot write value of generic type {typ}"
    let private commonWriteStruct guard (structTerm : term) (field : fieldId) value =
        match structTerm, guard with
        | { term = Struct(fields, typ) }, None -> Struct (PersistentDict.add field value fields) typ
        | { term = Struct(fields, typ) }, Some g ->
            let iteValue = {branches = List.singleton (g, value); elseValue = fields[field]} |> Merging.merge
            Struct (PersistentDict.add field iteValue fields) typ
        | _ -> internalfailf $"Writing field of structure: expected struct, but got {structTerm}"

    let writeStruct structTerm field value = commonWriteStruct None structTerm field value
    let guardedWriteStruct guard structTerm field value = commonWriteStruct guard structTerm field value

    let isSafeContextWrite actualType neededType =
        assert(neededType <> typeof<Void>)
        neededType = actualType
        || canCastImplicitly actualType neededType
        && (
            not (actualType.IsValueType && neededType.IsValueType)
            || internalSizeOf actualType = internalSizeOf neededType
        )

    type stackReading with
        interface IMemoryAccessConstantSource with
            override x.Compose state =
                let key = x.key.Map state.TypeVariableSubst
                state.memory.ReadStackLocation key

    let (|StackReading|_|) (src : ISymbolicConstantSource) =
        match src with
        | :? stackReading as sr -> Some(sr.key)
        | _ -> None

    type Memory private (
        evaluationStack : evaluationStack,
        stack : callStack,                                              // Arguments and local variables
        memoryRegions : pdict<IMemoryRegionId, IMemoryRegion>,
        concreteMemory : ConcreteMemory,                                // Fully concrete objects
        allocatedTypes : pdict<concreteHeapAddress, symbolicType>,      // Types of heap locations allocated via new
        initializedAddresses : pset<term>,                              // Addresses, which invariants were initialized
        delegates : pdict<concreteHeapAddress, term>,                   // Subtypes of System.Delegate allocated in heap
        memoryMode : memoryMode,                                        // If 'ConcreteMode', allocating concrete .NET objects inside 'ConcreteMemory'
        state : state
        ) =

        // Important invariant: self.State.memory === self
        let mutable state = state
        let mutable stack = stack
        let mutable evaluationStack = evaluationStack
        let mutable allocatedTypes = allocatedTypes
        let mutable initializedAddresses = initializedAddresses
        let mutable memoryRegions = memoryRegions
        let mutable delegates = delegates
        let mutable memoryMode = memoryMode

        let typeOfConcreteHeapAddress address =
            if address = VectorTime.zero then typeof<obj>
            else
                match PersistentDict.find allocatedTypes address with
                | ConcreteType t -> t
                | MockType _ -> __unreachable__() // Mock types may appear only in models

        let freshAddress () =
            state.currentTime <- VectorTime.advance state.currentTime
            state.currentTime

        let allocateType symbolicType =
            let concreteAddress = freshAddress()
            assert(not <| PersistentDict.contains concreteAddress allocatedTypes)
            allocatedTypes <- PersistentDict.add concreteAddress symbolicType allocatedTypes
            concreteAddress

        let getOrPutRegionCommon (key : IMemoryRegionId) typ memory =
            match PersistentDict.tryFind memory key with
            | Some value -> value
            | None -> key.Empty typ

        let getOrPutRegion key typ = getOrPutRegionCommon key typ memoryRegions

        // ---------------- Try term to object ----------------

        let tryAddressToObj fullyConcrete address =
            if address = VectorTime.zero then Some null
            elif fullyConcrete then concreteMemory.TryFullyConcrete address
            else concreteMemory.TryVirtToPhys address

        let tryPointerToObj fullyConcrete address (offset : int) =
            match tryAddressToObj fullyConcrete address with
            | Some obj when obj = null ->
                Some (nativeint offset :> obj)
            | Some obj ->
                let gch = Runtime.InteropServices.GCHandle.Alloc(obj, Runtime.InteropServices.GCHandleType.Pinned)
                let pObj = gch.AddrOfPinnedObject() + (nativeint offset)
                Some (pObj :> obj)
            | None -> None

        let castAndSet (fieldInfo : FieldInfo) structObj v =
            let v =
                if v <> null && v.GetType() <> fieldInfo.FieldType then
                    let fieldType = fieldInfo.FieldType
                    if fieldType = typeof<IntPtr> then
                        let gcHandle = System.Runtime.InteropServices.GCHandle.Alloc(v)
                        System.Runtime.InteropServices.GCHandle.ToIntPtr(gcHandle) :> obj
                    elif fieldType.IsPrimitive then convert v fieldType
                    else v
                else v
            fieldInfo.SetValue(structObj, v)

        let writeLowerBoundSymbolic guard address dimension arrayType value =
            ensureConcreteType arrayType.elemType
            let mrKey = MemoryRegionId.createArrayLowerBoundsId arrayType
            let mr = getOrPutRegion mrKey lengthType :?> arrayLowerBoundsRegion
            let key = {address = address; index = dimension}
            let mr' = MemoryRegion.write mr guard key value
            memoryRegions <- PersistentDict.add mrKey mr' memoryRegions

        let writeLengthSymbolic guard address dimension arrayType value =
            ensureConcreteType arrayType.elemType
            let mrKey = MemoryRegionId.createArrayLengthsId arrayType
            let mr = getOrPutRegion mrKey lengthType :?> arrayLengthsRegion
            let key = {address = address; index = dimension}
            let mr' = MemoryRegion.write mr guard key value
            memoryRegions <- PersistentDict.add mrKey mr' memoryRegions

        let writeDictionaryCount guard address dictionaryType value =
            ensureConcreteType dictionaryType.keyType
            let mrKey = MemoryRegionId.createDictionaryCountsId dictionaryType
            let mr = getOrPutRegion mrKey lengthType :?> dictionaryCountsRegion
            let key = { address = address }
            let mr' = MemoryRegion.write mr guard key value
            memoryRegions <- PersistentDict.add mrKey mr' memoryRegions

        let writeSetCount guard address setType value =
            ensureConcreteType setType.setValueType
            let mrKey = MemoryRegionId.createSetCountsId setType
            let mr = getOrPutRegion mrKey lengthType :?> setCountsRegion
            let key = { address = address }
            let mr' = MemoryRegion.write mr guard key value
            memoryRegions <- PersistentDict.add mrKey mr' memoryRegions

        let writeListCount guard address listType value =
            ensureConcreteType listType.listValueType
            let mrKey = MemoryRegionId.createListCountsId listType
            let mr = getOrPutRegion mrKey lengthType :?> listCountsRegion
            let key = { address = address }
            let mr' = MemoryRegion.write mr guard key value
            memoryRegions <- PersistentDict.add mrKey mr' memoryRegions

        let writeArrayKeySymbolic guard key arrayType value =
            let elementType = arrayType.elemType
            ensureConcreteType elementType
            let mrKey = MemoryRegionId.createArraysId arrayType
            let mr = getOrPutRegion mrKey elementType :?> arraysRegion
            let mr' = MemoryRegion.write mr guard key value
            memoryRegions <- PersistentDict.add mrKey mr' memoryRegions

        let writeArrayIndexSymbolic guard address indices arrayType value =
            let indices = List.map (fun i -> primitiveCast i typeof<int>) indices
            let key = OneArrayIndexKey(address, indices)
            writeArrayKeySymbolic guard key arrayType value

        let writeGeneralDictionaryKeyToKeysSymbolic (key : heapCollectionKey<'key>) guard dictionaryType value =
            let mrKey = MemoryRegionId.createDictionaryKeysId dictionaryType
            let mr = getOrPutRegion mrKey typeof<bool> :?> dictionariesRegion<'key>
            let mr' = MemoryRegion.write mr guard key value
            memoryRegions <- PersistentDict.add mrKey mr' memoryRegions

        let writeGeneralDictionaryKeySymbolic (key : heapCollectionKey<'key>) guard dictionaryType value valueType =
            writeGeneralDictionaryKeyToKeysSymbolic key guard dictionaryType <| True()
            let mrKey = MemoryRegionId.createDictionariesId dictionaryType
            let mr = getOrPutRegion mrKey valueType :?> dictionariesRegion<'key>
            let mr' = MemoryRegion.write mr guard key value
            memoryRegions <- PersistentDict.add mrKey mr' memoryRegions

        let writeAddrDictionaryKeyToKeysSymbolic key guard dictionaryType value =
            let mrKey = MemoryRegionId.createDictionaryKeysId dictionaryType
            let mr = getOrPutRegion mrKey typeof<bool> :?> addrDictionaryKeysRegion
            let mr' = MemoryRegion.write mr guard key value
            memoryRegions <- PersistentDict.add mrKey mr' memoryRegions

        let writeAddrDictionaryKeySymbolic key guard dictionaryType value valueType =
            writeAddrDictionaryKeyToKeysSymbolic key guard dictionaryType <| True()
            let mrKey = MemoryRegionId.createDictionariesId dictionaryType
            let mr = getOrPutRegion mrKey valueType :?> addrDictionariesRegion
            let mr' = MemoryRegion.write mr guard key value
            memoryRegions <- PersistentDict.add mrKey mr' memoryRegions

        let writeDictionaryKeySymbolic guard address key (dictionaryType : dictionaryType) value =
            let valueType = dictionaryType.valueType
            ensureConcreteType valueType
            let write =
                match dictionaryType with
                | BoolDictionary _ -> writeGeneralDictionaryKeySymbolic <| ({ address = address; key = key } : heapCollectionKey<bool>)
                | ByteDictionary _ -> writeGeneralDictionaryKeySymbolic <| ({ address = address; key = key } : heapCollectionKey<byte>)
                | SByteDictionary _ -> writeGeneralDictionaryKeySymbolic <| ({ address = address; key = key } : heapCollectionKey<sbyte>)
                | CharDictionary _ -> writeGeneralDictionaryKeySymbolic <| ({ address = address; key = key } : heapCollectionKey<char>)
                | DecimalDictionary _ -> writeGeneralDictionaryKeySymbolic <| ({ address = address; key = key } : heapCollectionKey<decimal>)
                | DoubleDictionary _ -> writeGeneralDictionaryKeySymbolic <| ({ address = address; key = key } : heapCollectionKey<double>)
                | IntDictionary _ -> writeGeneralDictionaryKeySymbolic <| ({ address = address; key = key } : heapCollectionKey<int>)
                | UIntDictionary _ -> writeGeneralDictionaryKeySymbolic <| ({ address = address; key = key } : heapCollectionKey<uint>)
                | LongDictionary _ -> writeGeneralDictionaryKeySymbolic <| ({ address = address; key = key } : heapCollectionKey<int64>)
                | ULongDictionary _ -> writeGeneralDictionaryKeySymbolic <| ({ address = address; key = key } : heapCollectionKey<uint64>)
                | ShortDictionary _ -> writeGeneralDictionaryKeySymbolic <| ({ address = address; key = key } : heapCollectionKey<int16>)
                | UShortDictionary _ -> writeGeneralDictionaryKeySymbolic <| ({ address = address; key = key } : heapCollectionKey<uint16>)
                | AddrDictionary _ -> writeAddrDictionaryKeySymbolic <| ({ address = address; key = key } : addrCollectionKey)
                | _ -> __unreachable__()
            write guard dictionaryType value valueType

        let writeDictionaryKeyToKeysSymbolic guard address key (dictionaryType : dictionaryType) value =
            let valueType = dictionaryType.valueType
            ensureConcreteType valueType
            let write =
                match dictionaryType with
                | BoolDictionary _ -> writeGeneralDictionaryKeyToKeysSymbolic <| ({ address = address; key = key } : heapCollectionKey<bool>)
                | ByteDictionary _ -> writeGeneralDictionaryKeyToKeysSymbolic <| ({ address = address; key = key } : heapCollectionKey<byte>)
                | SByteDictionary _ -> writeGeneralDictionaryKeyToKeysSymbolic <| ({ address = address; key = key } : heapCollectionKey<sbyte>)
                | CharDictionary _ -> writeGeneralDictionaryKeyToKeysSymbolic <| ({ address = address; key = key } : heapCollectionKey<char>)
                | DecimalDictionary _ -> writeGeneralDictionaryKeyToKeysSymbolic <| ({ address = address; key = key } : heapCollectionKey<decimal>)
                | DoubleDictionary _ -> writeGeneralDictionaryKeyToKeysSymbolic <| ({ address = address; key = key } : heapCollectionKey<double>)
                | IntDictionary _ -> writeGeneralDictionaryKeyToKeysSymbolic <| ({ address = address; key = key } : heapCollectionKey<int>)
                | UIntDictionary _ -> writeGeneralDictionaryKeyToKeysSymbolic <| ({ address = address; key = key } : heapCollectionKey<uint>)
                | LongDictionary _ -> writeGeneralDictionaryKeyToKeysSymbolic <| ({ address = address; key = key } : heapCollectionKey<int64>)
                | ULongDictionary _ -> writeGeneralDictionaryKeyToKeysSymbolic <| ({ address = address; key = key } : heapCollectionKey<uint64>)
                | ShortDictionary _ -> writeGeneralDictionaryKeyToKeysSymbolic <| ({ address = address; key = key } : heapCollectionKey<int16>)
                | UShortDictionary _ -> writeGeneralDictionaryKeyToKeysSymbolic <| ({ address = address; key = key } : heapCollectionKey<uint16>)
                | AddrDictionary _ -> writeAddrDictionaryKeyToKeysSymbolic <| ({ address = address; key = key } : addrCollectionKey)
                | _ -> __unreachable__()
            write guard dictionaryType value

        let writeGeneralSetKeySymbolic (key : heapCollectionKey<'key>) guard setType value =
            let mrKey = MemoryRegionId.createSetsId setType
            let mr = getOrPutRegion mrKey typeof<bool> :?> setsRegion<'key>
            let mr' = MemoryRegion.write mr guard key value
            memoryRegions <- PersistentDict.add mrKey mr' memoryRegions

        let writeAddrSetKeySymbolic key guard setType value =
            let mrKey = MemoryRegionId.createSetsId setType
            let mr = getOrPutRegion mrKey typeof<bool> :?> addrSetsRegion
            let mr' = MemoryRegion.write mr guard key value
            memoryRegions <- PersistentDict.add mrKey mr' memoryRegions

        let writeSetKeySymbolic guard address item (setType : setType) value =
            let itemType = setType.setValueType
            ensureConcreteType itemType
            let write =
                match setType with
                | BoolSet _ -> writeGeneralSetKeySymbolic <| ({ address = address; key = item } : heapCollectionKey<bool>)
                | ByteSet _ -> writeGeneralSetKeySymbolic <| ({ address = address; key = item } : heapCollectionKey<byte>)
                | SByteSet _ -> writeGeneralSetKeySymbolic <| ({ address = address; key = item } : heapCollectionKey<sbyte>)
                | CharSet _ -> writeGeneralSetKeySymbolic <| ({ address = address; key = item } : heapCollectionKey<char>)
                | DecimalSet _ -> writeGeneralSetKeySymbolic <| ({ address = address; key = item } : heapCollectionKey<decimal>)
                | DoubleSet _ -> writeGeneralSetKeySymbolic <| ({ address = address; key = item } : heapCollectionKey<double>)
                | IntSet _ -> writeGeneralSetKeySymbolic <| ({ address = address; key = item } : heapCollectionKey<int>)
                | UIntSet _ -> writeGeneralSetKeySymbolic <| ({ address = address; key = item } : heapCollectionKey<uint>)
                | LongSet _ -> writeGeneralSetKeySymbolic <| ({ address = address; key = item } : heapCollectionKey<int64>)
                | ULongSet _ -> writeGeneralSetKeySymbolic <| ({ address = address; key = item } : heapCollectionKey<uint64>)
                | ShortSet _ -> writeGeneralSetKeySymbolic <| ({ address = address; key = item } : heapCollectionKey<int16>)
                | UShortSet _ -> writeGeneralSetKeySymbolic <| ({ address = address; key = item } : heapCollectionKey<uint16>)
                | AddrSet _ -> writeAddrSetKeySymbolic <| ({ address = address; key = item } : addrCollectionKey)
                | _ -> __unreachable__()
            write guard setType value

        let writeListIndexSymbolic guard address index (listType : listType) value =
            let key = OneArrayIndexKey(address, [index])
            let mrKey = MemoryRegionId.createListsId listType
            let mr = getOrPutRegion mrKey listType.listValueType :?> listsRegion
            let mr' = MemoryRegion.write mr guard key value
            memoryRegions <- PersistentDict.add mrKey mr' memoryRegions

        let writeStackLocation key value =
            stack <- CallStack.writeStackLocation stack key value

        let writeClassFieldSymbolic guard address (field : fieldId) value =
            ensureConcreteType field.typ
            let mrKey = MemoryRegionId.createClassFieldsId field
            let mr = getOrPutRegion mrKey field.typ :?> classFieldsRegion
            let key = {address = address}
            let mr' = MemoryRegion.write mr guard key value
            memoryRegions <- PersistentDict.add mrKey mr' memoryRegions

        let writeArrayRangeSymbolic guard address fromIndices toIndices arrayType value =
            let fromIndices = List.map (fun i -> primitiveCast i typeof<int>) fromIndices
            let toIndices = List.map (fun i -> primitiveCast i typeof<int>) toIndices
            let key = RangeArrayIndexKey(address, fromIndices, toIndices)
            writeArrayKeySymbolic guard key arrayType value

        let writeListRangeSymbolic address fromIdx toIdx listType value =
            let key = RangeArrayIndexKey(address, [fromIdx], [toIdx])
            let mrKey = MemoryRegionId.createListsId listType
            let mr = getOrPutRegion mrKey listType.listValueType :?> listsRegion
            let mr' = MemoryRegion.write mr None key value
            memoryRegions <- PersistentDict.add mrKey mr' memoryRegions

        let fillArrayBoundsSymbolic guard address lengths lowerBounds arrayType =
            let d = List.length lengths
            assert(d = arrayType.dimension)
            assert(List.length lowerBounds = d)
            let writeLengths l i = writeLengthSymbolic guard address (Concrete i lengthType) arrayType l
            let writeLowerBounds l i = writeLowerBoundSymbolic guard address (Concrete i lengthType) arrayType l
            List.iter2 writeLengths lengths [0 .. d-1]
            List.iter2 writeLowerBounds lowerBounds [0 .. d-1]

        let writeStackBuffer stackKey guard index value =
            let mrKey = MemoryRegionId.createStackBuffersId stackKey
            let mr = getOrPutRegion mrKey typeof<int8> :?> stackBuffersRegion
            let key : stackBufferIndexKey = {index = index}
            let mr' = MemoryRegion.write mr guard key value
            memoryRegions <- PersistentDict.add mrKey mr' memoryRegions

        let writeBoxedLocationSymbolic guard (address : term) value typ =
            ensureConcreteType typ
            let mrKey = MemoryRegionId.createBoxedLocationsId typ
            let mr = getOrPutRegion mrKey typ :?> boxedLocationsRegion
            let key = {address = address}
            let mr' = MemoryRegion.write mr guard key value
            memoryRegions <- PersistentDict.add mrKey mr' memoryRegions

        let writeAddressUnsafe (reporter : IErrorReporter) address startByte value =
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

        let delegatesMatch ref1 ref2 =
            match ref1.term, ref2.term with
            | HeapRef(address1, _), HeapRef(address2, _) -> address1 = address2
            | _ -> internalfail $"delegatesMatch: unexpected references {ref1}, {ref2}"

        // ------------------------------- Composition -------------------------------

        let composeMemoryRegions dict dict' =
            // TODO: somehow get rid of this copy-paste?
            let substTerm = state.FillHoles
            let substType = state.SubstituteTypeVariables
            let substTime = state.ComposeTime
            let composeOneRegion dict k (mr' : memoryRegion<_, _>) =
                let mr =
                    match PersistentDict.tryFind dict k with
                    | Some mr -> mr
                    | None -> MemoryRegion.empty mr'.typ
                let composed = MemoryRegion.compose mr mr'
                PersistentDict.add k composed dict
            dict'
                |> PersistentDict.map id (MemoryRegion.map substTerm substType substTime)
                |> PersistentDict.fold composeOneRegion dict

        let composeEvaluationStacksOf evaluationStack =
            EvaluationStack.map state.FillHoles evaluationStack

        // state is untouched. It is needed because of this situation:
        // Effect: x' <- y + 5, y' <- x + 10
        // Left state: x <- 0, y <- 0
        // After composition: {x <- 5, y <- 15} OR {y <- 10, x <- 15}
        // but expected result is {x <- 5, y <- 10}
        let fillHolesInStack stack =
            let keyMapper (k : stackKey) = k.Map state.TypeVariableSubst
            CallStack.map keyMapper state.FillHoles state.SubstituteTypeVariables stack

        let composeStacksOf (otherMemory : IMemory) : callStack =
            let stack' = fillHolesInStack otherMemory.Stack
            CallStack.applyEffect stack stack'

        let composeConcreteMemory mapKey (cm : ConcreteMemory) (cm' : ConcreteMemory) =
            // TODO: implement concrete memory composition
            ()

        let composeConcreteDictionaries mapKey mapValue dict dict' =
            let fillAndMutate acc k v =
                let k' = mapKey k
                let v' = mapValue v
                match PersistentDict.tryFind acc k' with
                | Some v ->
                    assert(v = v')
                    acc
                | None -> PersistentDict.add k' v' acc
            PersistentDict.fold fillAndMutate dict dict'

        let substituteTypeVariablesToSymbolicType st =
            match st with
            | ConcreteType t -> state.SubstituteTypeVariables t |> ConcreteType
            | MockType _ -> __unreachable__()

        new() as self =
            Memory(
                EvaluationStack.empty,
                CallStack.empty,
                PersistentDict.empty,
                ConcreteMemory(),
                PersistentDict.empty,
                PersistentSet.empty,
                PersistentDict.empty,
                ConcreteMode,
                Unchecked.defaultof<state>
            )
            then
                let state = {
                    pc = PC.empty
                    typeStorage = typeStorage()
                    initializedTypes = SymbolicSet.empty
                    typeVariables = (MappedStack.empty, Stack.empty)
                    currentTime = [1]
                    startingTime = VectorTime.zero
                    exceptionsRegister = exceptionRegisterStack.Initial
                    model = PrimitiveModel (Dictionary())
                    memory = self
                    complete = false
                    methodMocks = Dictionary()
                }
                self.State <- state

        // ------------------ Object to term ------------------

        member private self.AllocateObjectIfNeed (obj : obj) t =
            assert(memoryMode = ConcreteMode)
            let address =
                match concreteMemory.TryPhysToVirt obj with
                | Some address -> address
                | None when obj = null -> VectorTime.zero
                | None ->
                    let typ = mostConcreteType (obj.GetType()) t
                    let concreteAddress = self.AllocateConcreteType typ
                    concreteMemory.Allocate concreteAddress obj
                    concreteAddress
            ConcreteHeapAddress address

        member private self.ReferenceTypeToTerm (obj : obj) t =
            let address = self.AllocateObjectIfNeed obj t
            let objType = self.TypeOfHeapLocation address
            HeapRef address objType

        member private self.ObjToTerm (t : Type) (obj : obj) =
            match obj with
            | _ when isNullable t -> self.NullableToTerm t obj
            | null -> nullRef t
            | :? bool as b -> makeBool b
            | _ when isNumeric t -> makeNumber obj
            // TODO: need pointer?
            | _ when isPointer t -> Concrete obj t
            | _ when t.IsValueType -> self.StructToTerm obj t
            | _ -> self.ReferenceTypeToTerm obj t

        member private self.StructToTerm (obj : obj) t =
            let makeField (fieldInfo : FieldInfo) _ _ =
               fieldInfo.GetValue(obj) |> self.ObjToTerm fieldInfo.FieldType
            makeStruct false makeField t

        member private self.NullableToTerm t (obj : obj) =
            let nullableType = Nullable.GetUnderlyingType t
            let valueField, hasValueField = Reflection.fieldsOfNullable t
            let value, hasValue =
                if box obj <> null then self.ObjToTerm nullableType obj, True()
                else self.ObjToTerm nullableType (Reflection.createObject nullableType), False()
            let fields = PersistentDict.ofSeq <| seq [(valueField, value); (hasValueField, hasValue)]
            Struct fields t

        member private self.CommonTryTermToObj fullyConcrete term =
            match term.term with
            | ConcreteDelegate _
            | CombinedDelegate _ -> None
            | Concrete(obj, _) -> Some obj
            | Struct(fields, typ) when isNullable typ -> self.TryNullableTermToObj fullyConcrete fields typ
            | Struct(fields, typ) when not typ.IsByRefLike -> self.TryStructTermToObj fullyConcrete fields typ
            | HeapRef({term = ConcreteHeapAddress a}, _) -> tryAddressToObj fullyConcrete a
            | Ptr(HeapLocation({term = ConcreteHeapAddress a}, _), _, ConcreteT (:? int as offset, _)) ->
                tryPointerToObj fullyConcrete a offset
            | _ -> None

        member private self.TryTermListToObjects (terms : term list) =
            let toObj (t : term) acc k =
                match self.TryTermToObj t with
                | Some o -> o :: acc |> k
                | None -> None
            Cps.List.foldrk toObj List.empty terms Some

        member private self.TryTermToObj term = self.CommonTryTermToObj false term

        member private self.TryTermToFullyConcreteObj term = self.CommonTryTermToObj true term

        member private self.TryStructTermToObj fullyConcrete fields typ =
            let structObj = Reflection.createObject typ
            let addField _ (fieldId, value) k =
                let fieldInfo = Reflection.getFieldInfo fieldId
                // field was not found in the structure, skipping it
                if fieldInfo = null then k ()
                else
                    match self.CommonTryTermToObj fullyConcrete value with
                    // field can be converted to obj, so continue
                    | Some v -> castAndSet fieldInfo structObj v |> k
                    // field can not be converted to obj, so break and return None
                    | None -> None
            Cps.Seq.foldlk addField () (PersistentDict.toSeq fields) (fun _ -> Some structObj)

        member private self.TryNullableTermToObj fullyConcrete fields typ =
            let valueField, hasValueField = Reflection.fieldsOfNullable typ
            let value = PersistentDict.find fields valueField
            let hasValue = PersistentDict.find fields hasValueField
            match self.CommonTryTermToObj fullyConcrete value with
            | Some obj when hasValue = True() -> Some obj
            | _ when hasValue = False() -> Some null
            | _ -> None

        // ------------------------------- Safe reading -------------------------------

        member self.MakeSymbolicHeapRead picker key time typ memoryObject =
            let source : heapReading<'key, 'reg> = {picker = picker; key = key; memoryObject = memoryObject; time = time}
            let name = picker.mkName key
            self.MakeSymbolicValue source name typ

        member self.MakeArraySymbolicHeapRead picker (key : heapArrayKey) time typ memoryObject =
            let source : arrayReading = {picker = picker; key = key; memoryObject = memoryObject; time = time}
            let name = picker.mkName key
            self.MakeSymbolicValue source name typ

        member self.MakeGeneralDictionarySymbolicHeapRead<'key when 'key : equality> picker (key : heapCollectionKey<'key>) time typ memoryObject =
            let source : generalDictionaryReading<'key> = {picker = picker; key = key; memoryObject = memoryObject; time = time}
            let name = picker.mkName key
            self.MakeSymbolicValue source name typ

        member self.MakeAddrDictionarySymbolicHeapRead picker (key : addrCollectionKey) time typ memoryObject =
            let source : addrDictionaryReading = {picker = picker; key = key; memoryObject = memoryObject; time = time}
            let name = picker.mkName key
            self.MakeSymbolicValue source name typ

        member self.MakeGeneralSetSymbolicHeapRead<'key when 'key : equality> picker (key : heapCollectionKey<'key>) time typ memoryObject =
            let source : generalSetReading<'key> = {picker = picker; key = key; memoryObject = memoryObject; time = time}
            let name = picker.mkName key
            self.MakeSymbolicValue source name typ

        member self.MakeAddrSetSymbolicHeapRead picker (key : addrCollectionKey) time typ memoryObject =
            let source : addrSetReading = {picker = picker; key = key; memoryObject = memoryObject; time = time}
            let name = picker.mkName key
            self.MakeSymbolicValue source name typ

        member self.RangeReadingUnreachable _ _ = __unreachable__()
        member self.SpecializedReading (readKey : heapArrayKey) utKey =
            match utKey with
            | {key = key'; value = {term = HeapRef({term = Constant(_, HeapAddressSource(ArrayRangeReading(mo, srcA, srcF, srcT, picker, _)), _)}, _)}}
            | {key = key'; value = {term = Constant(_, ArrayRangeReading(mo, srcA, srcF, srcT, picker, _), _)}} ->
                let key = readKey.Specialize key' srcA srcF srcT
                let inst typ memoryObject =
                    self.MakeArraySymbolicHeapRead picker key state.startingTime typ memoryObject
                MemoryRegion.read mo key (picker.isDefaultKey state) inst self.SpecializedReading
            | _ -> utKey.value

        member private self.ReadLowerBoundSymbolic address dimension arrayType =
            let extractor (state : state) =
                let mrKey = MemoryRegionId.createArrayLowerBoundsId <| state.SubstituteTypeVariablesIntoArrayType arrayType
                getOrPutRegionCommon mrKey lengthType state.memory.MemoryRegions :?> arrayLowerBoundsRegion
            let mkName (key : heapVectorIndexKey) = $"LowerBound({key.address}, {key.index})"
            let isDefault state (key : heapVectorIndexKey) = isHeapAddressDefault state key.address || arrayType.isVector
            let key = {address = address; index = dimension}
            let inst typ memoryRegion =
                let sort = ArrayLowerBoundSort arrayType
                let picker =
                    {
                        sort = sort; extract = extractor; mkName = mkName
                        isDefaultKey = isDefault; isDefaultRegion = false
                    }
                self.MakeSymbolicHeapRead picker key state.startingTime typ memoryRegion
            MemoryRegion.read (extractor state) key (isDefault state) inst self.RangeReadingUnreachable

        member private self.ReadLengthSymbolic address dimension arrayType =
            let extractor (state : state) =
                let mrKey = MemoryRegionId.createArrayLengthsId <| state.SubstituteTypeVariablesIntoArrayType arrayType
                getOrPutRegionCommon mrKey lengthType state.memory.MemoryRegions :?> arrayLengthsRegion
            let mkName (key : heapVectorIndexKey) = $"Length({key.address}, {key.index})"
            let isDefault state (key : heapVectorIndexKey) = isHeapAddressDefault state key.address
            let key = {address = address; index = dimension}
            let inst typ memoryRegion =
                let sort = ArrayLengthSort arrayType
                let picker =
                    {
                        sort = sort; extract = extractor; mkName = mkName
                        isDefaultKey = isDefault; isDefaultRegion = false
                    }
                self.MakeSymbolicHeapRead picker key state.startingTime typ memoryRegion
            MemoryRegion.read (extractor state) key (isDefault state) inst self.RangeReadingUnreachable

        member private self.ReadDictionaryCountSymbolic address dictionaryType =
            let extractor (state : state) =
                let mrKey = MemoryRegionId.createDictionaryCountsId <| state.SubstituteTypeVariablesIntoDictionaryType dictionaryType
                getOrPutRegionCommon mrKey lengthType state.memory.MemoryRegions :?> dictionaryCountsRegion
            let mkName (key : heapAddressKey) = $"Count({key.address})"
            let isDefault state (key : heapAddressKey) = isHeapAddressDefault state key.address
            let key = { address = address }
            let inst typ memoryRegion =
                let sort = DictionaryCountSort dictionaryType
                let picker =
                    {
                        sort = sort; extract = extractor; mkName = mkName
                        isDefaultKey = isDefault; isDefaultRegion = false
                    }
                self.MakeSymbolicHeapRead picker key state.startingTime typ memoryRegion
            MemoryRegion.read (extractor state) key (isDefault state) inst self.RangeReadingUnreachable

        member private self.ReadSetCountSymbolic address setType =
            let extractor (state : state) =
                let mrKey = MemoryRegionId.createSetCountsId <| state.SubstituteTypeVariablesIntoSetType setType
                getOrPutRegionCommon mrKey lengthType state.memory.MemoryRegions :?> setCountsRegion
            let mkName (key : heapAddressKey) = $"Count({key.address})"
            let isDefault state (key : heapAddressKey) = isHeapAddressDefault state key.address
            let key = { address = address }
            let inst typ memoryRegion =
                let sort = SetCountSort setType
                let picker =
                    {
                        sort = sort; extract = extractor; mkName = mkName
                        isDefaultKey = isDefault; isDefaultRegion = false
                    }
                self.MakeSymbolicHeapRead picker key state.startingTime typ memoryRegion
            MemoryRegion.read (extractor state) key (isDefault state) inst self.RangeReadingUnreachable

        member private self.ReadListCountSymbolic address listType =
            let extractor (state : state) =
                let mrKey = MemoryRegionId.createListCountsId <| state.SubstituteTypeVariablesIntoListType listType
                getOrPutRegionCommon mrKey lengthType state.memory.MemoryRegions :?> listCountsRegion
            let mkName (key : heapAddressKey) = $"Count({key.address})"
            let isDefault state (key : heapAddressKey) = isHeapAddressDefault state key.address
            let key = { address = address }
            let inst typ memoryRegion =
                let sort = ListCountSort listType
                let picker =
                    {
                        sort = sort; extract = extractor; mkName = mkName
                        isDefaultKey = isDefault; isDefaultRegion = false
                    }
                self.MakeSymbolicHeapRead picker key state.startingTime typ memoryRegion
            MemoryRegion.read (extractor state) key (isDefault state) inst self.RangeReadingUnreachable

        member private self.ReadArrayRegion arrayType extractor region (isDefaultRegion : bool) (key : heapArrayKey) =
            let isDefault state (key : heapArrayKey) = isHeapAddressDefault state key.Address
            let instantiate typ memory =
                let sort = ArrayIndexSort arrayType
                let picker =
                    {
                        sort = sort; extract = extractor; mkName = toString
                        isDefaultKey = isDefault; isDefaultRegion = isDefaultRegion
                    }
                let time =
                    if isValueType typ then state.startingTime
                    else MemoryRegion.maxTime region.updates state.startingTime
                self.MakeArraySymbolicHeapRead picker key time typ memory
            MemoryRegion.read region key (isDefault state) instantiate self.SpecializedReading

        member private self.ReadListRegion listType extractor region (isDefaultRegion : bool) (key : heapArrayKey) =
            let isDefault state (key : heapArrayKey) = isHeapAddressDefault state key.Address
            let instantiate typ memory =
                let sort = ListIndexSort listType
                let picker =
                    {
                        sort = sort; extract = extractor; mkName = toString
                        isDefaultKey = isDefault; isDefaultRegion = isDefaultRegion
                    }
                let time =
                    if isValueType typ then state.startingTime
                    else MemoryRegion.maxTime region.updates state.startingTime
                self.MakeArraySymbolicHeapRead picker key time typ memory
            MemoryRegion.read region key (isDefault state) instantiate self.SpecializedReading

        member private self.ReadGeneralDictionaryRegion<'key when 'key : equality> dictionaryType extractor region (isDefaultRegion : bool) (key : heapCollectionKey<'key>) =
            let isDefault state (key : heapCollectionKey<'key>) = isHeapAddressDefault state key.address
            let instantiate typ memory =
                let sort = DictionaryKeySort dictionaryType
                let picker =
                    {
                        sort = sort; extract = extractor; mkName = toString
                        isDefaultKey = isDefault; isDefaultRegion = isDefaultRegion
                    }
                let time =
                    if isValueType typ then state.startingTime
                    else MemoryRegion.maxTime region.updates state.startingTime
                self.MakeGeneralDictionarySymbolicHeapRead<'key> picker key time typ memory
            MemoryRegion.read region key (isDefault state) instantiate self.RangeReadingUnreachable

        member private self.ReadAddrDictionaryRegion dictionaryType extractor region (isDefaultRegion : bool) (key : addrCollectionKey) =
            let isDefault state (key : addrCollectionKey) = isHeapAddressDefault state key.address
            let instantiate typ memory =
                let sort = AddrDictionaryKeySort dictionaryType
                let picker =
                    {
                        sort = sort; extract = extractor; mkName = toString
                        isDefaultKey = isDefault; isDefaultRegion = isDefaultRegion
                    }
                let time =
                    if isValueType typ then state.startingTime
                    else MemoryRegion.maxTime region.updates state.startingTime
                self.MakeAddrDictionarySymbolicHeapRead picker key time typ memory
            MemoryRegion.read region key (isDefault state) instantiate self.RangeReadingUnreachable

        member private self.HasGeneralDictionaryRegion<'key when 'key : equality> dictionaryType extractor region (isDefaultRegion : bool) (key : heapCollectionKey<'key>) =
            let isDefault state (key : heapCollectionKey<'key>) = isHeapAddressDefault state key.address
            let instantiate typ memory =
                let sort = DictionaryHasKeySort dictionaryType
                let picker =
                    {
                        sort = sort; extract = extractor; mkName = toString
                        isDefaultKey = isDefault; isDefaultRegion = isDefaultRegion
                    }
                let time =
                    if isValueType typ then state.startingTime
                    else MemoryRegion.maxTime region.updates state.startingTime
                self.MakeGeneralSetSymbolicHeapRead<'key> picker key time typ memory
            MemoryRegion.read region key (isDefault state) instantiate self.RangeReadingUnreachable

        member private self.HasAddrDictionaryRegion dictionaryType extractor region (isDefaultRegion : bool) (key : addrCollectionKey) =
            let isDefault state (key : addrCollectionKey) = isHeapAddressDefault state key.address
            let instantiate typ memory =
                let sort = AddrDictionaryHasKeySort dictionaryType
                let picker =
                    {
                        sort = sort; extract = extractor; mkName = toString
                        isDefaultKey = isDefault; isDefaultRegion = isDefaultRegion
                    }
                let time =
                    if isValueType typ then state.startingTime
                    else MemoryRegion.maxTime region.updates state.startingTime
                self.MakeAddrSetSymbolicHeapRead picker key time typ memory
            MemoryRegion.read region key (isDefault state) instantiate self.RangeReadingUnreachable

        member private self.ReadGeneralSetRegion<'key when 'key : equality> setType extractor region (isDefaultRegion : bool) (key : heapCollectionKey<'key>) =
            let isDefault state (key : heapCollectionKey<'key>) = isHeapAddressDefault state key.address
            let instantiate typ memory =
                let sort = SetKeySort setType
                let picker =
                    {
                        sort = sort; extract = extractor; mkName = toString
                        isDefaultKey = isDefault; isDefaultRegion = isDefaultRegion
                    }
                let time =
                    if isValueType typ then state.startingTime
                    else MemoryRegion.maxTime region.updates state.startingTime
                self.MakeGeneralSetSymbolicHeapRead<'key> picker key time typ memory
            MemoryRegion.read region key (isDefault state) instantiate self.RangeReadingUnreachable

        member private self.ReadAddrSetRegion setType extractor region (isDefaultRegion : bool) (key : addrCollectionKey) =
            let isDefault state (key : addrCollectionKey) = isHeapAddressDefault state key.address
            let instantiate typ memory =
                let sort = AddrSetKeySort setType
                let picker =
                    {
                        sort = sort; extract = extractor; mkName = toString
                        isDefaultKey = isDefault; isDefaultRegion = isDefaultRegion
                    }
                let time =
                    if isValueType typ then state.startingTime
                    else MemoryRegion.maxTime region.updates state.startingTime
                self.MakeAddrSetSymbolicHeapRead picker key time typ memory
            MemoryRegion.read region key (isDefault state) instantiate self.RangeReadingUnreachable

        member private self.ReadArrayKeySymbolic key arrayType =
            let extractor (state : state) =
                let arrayType = state.SubstituteTypeVariablesIntoArrayType arrayType
                let mrKey = MemoryRegionId.createArraysId arrayType
                getOrPutRegionCommon mrKey arrayType.elemType state.memory.MemoryRegions :?> arraysRegion
            self.ReadArrayRegion arrayType extractor (extractor state) false key


        member private self.ReadListRangeSymbolic address fromIdx toIdx listType =
            let key = RangeArrayIndexKey(address, [fromIdx], [toIdx])
            let extractor (state : state) =
                let listType = state.SubstituteTypeVariablesIntoListType listType
                let mrKey = MemoryRegionId.createListsId listType
                getOrPutRegionCommon mrKey listType.listValueType state.memory.MemoryRegions :?> listsRegion
            self.ReadListRegion listType extractor (extractor state) false key

        member private self.ReadListIndexSymbolic address index listType =
            let key = OneArrayIndexKey(address, [index])
            let extractor (state : state) =
                let listType = state.SubstituteTypeVariablesIntoListType listType
                let mrKey = MemoryRegionId.createListsId listType
                getOrPutRegionCommon mrKey listType.listValueType state.memory.MemoryRegions
                :?> listsRegion
            self.ReadListRegion listType extractor (extractor state) false key

        member private self.ReadGeneralDictionaryKeySymbolic<'key when 'key : equality> address key dictionaryType =
            let key : heapCollectionKey<'key> = { address = address; key = key }
            let extractor (state : state) =
                let dictionaryType = state.SubstituteTypeVariablesIntoDictionaryType dictionaryType
                let mrKey = MemoryRegionId.createDictionariesId dictionaryType
                getOrPutRegionCommon mrKey dictionaryType.valueType state.memory.MemoryRegions
                :?> memoryRegion<heapCollectionKey<'key>, productRegion<vectorTime intervals, 'key points>>
            self.ReadGeneralDictionaryRegion<'key> dictionaryType extractor (extractor state) false key

        member private self.ReadAddrDictionaryKeySymbolic address key dictionaryType =
            let key : addrCollectionKey = { address = address; key = key }
            let extractor (state : state) =
                let dictionaryType = state.SubstituteTypeVariablesIntoDictionaryType dictionaryType
                let mrKey = MemoryRegionId.createDictionariesId dictionaryType
                getOrPutRegionCommon mrKey dictionaryType.valueType state.memory.MemoryRegions :?> addrDictionariesRegion
            self.ReadAddrDictionaryRegion dictionaryType extractor (extractor state) false key

        member private self.ReadDictionaryKeySymbolic address key dictionaryType =
            let read =
                match dictionaryType with
                | BoolDictionary _ -> self.ReadGeneralDictionaryKeySymbolic<bool>
                | ByteDictionary _ -> self.ReadGeneralDictionaryKeySymbolic<byte>
                | SByteDictionary _ -> self.ReadGeneralDictionaryKeySymbolic<sbyte>
                | CharDictionary _ -> self.ReadGeneralDictionaryKeySymbolic<char>
                | DecimalDictionary _ -> self.ReadGeneralDictionaryKeySymbolic<decimal>
                | DoubleDictionary _ -> self.ReadGeneralDictionaryKeySymbolic<double>
                | IntDictionary _ -> self.ReadGeneralDictionaryKeySymbolic<int>
                | UIntDictionary _ -> self.ReadGeneralDictionaryKeySymbolic<uint>
                | LongDictionary _ -> self.ReadGeneralDictionaryKeySymbolic<int64>
                | ULongDictionary _ -> self.ReadGeneralDictionaryKeySymbolic<uint64>
                | ShortDictionary _ -> self.ReadGeneralDictionaryKeySymbolic<int16>
                | UShortDictionary _ -> self.ReadGeneralDictionaryKeySymbolic<uint16>
                | AddrDictionary _ -> self.ReadAddrDictionaryKeySymbolic
                | _ -> __unreachable__()
            read address key dictionaryType

        member private self.HasGeneralDictionaryKeySymbolic<'key when 'key : equality> address key dictionaryType =
            let key : heapCollectionKey<'key> = { address = address; key = key }
            let extractor (state : state) =
                let dictionaryType = state.SubstituteTypeVariablesIntoDictionaryType dictionaryType
                let mrKey = MemoryRegionId.createDictionaryKeysId dictionaryType
                getOrPutRegionCommon mrKey typeof<bool> state.memory.MemoryRegions
                :?> memoryRegion<heapCollectionKey<'key>, productRegion<vectorTime intervals, 'key points>>
            self.HasGeneralDictionaryRegion<'key> dictionaryType extractor (extractor state) false key

        member private self.HasAddrDictionaryKeySymbolic address key dictionaryType =
            let key : addrCollectionKey = { address = address; key = key }
            let extractor (state : state) =
                let dictionaryType = state.SubstituteTypeVariablesIntoDictionaryType dictionaryType
                let mrKey = MemoryRegionId.createDictionaryKeysId dictionaryType
                getOrPutRegionCommon mrKey typeof<bool> state.memory.MemoryRegions :?> addrDictionaryKeysRegion
            self.HasAddrDictionaryRegion dictionaryType extractor (extractor state) false key

        member private self.DictionaryHasKeySymbolic address key dictionaryType =
            let read =
                match dictionaryType with
                | BoolDictionary _ -> self.HasGeneralDictionaryKeySymbolic<bool>
                | ByteDictionary _ -> self.HasGeneralDictionaryKeySymbolic<byte>
                | SByteDictionary _ -> self.HasGeneralDictionaryKeySymbolic<sbyte>
                | CharDictionary _ -> self.HasGeneralDictionaryKeySymbolic<char>
                | DecimalDictionary _ -> self.HasGeneralDictionaryKeySymbolic<decimal>
                | DoubleDictionary _ -> self.HasGeneralDictionaryKeySymbolic<double>
                | IntDictionary _ -> self.HasGeneralDictionaryKeySymbolic<int>
                | UIntDictionary _ -> self.HasGeneralDictionaryKeySymbolic<uint>
                | LongDictionary _ -> self.HasGeneralDictionaryKeySymbolic<int64>
                | ULongDictionary _ -> self.HasGeneralDictionaryKeySymbolic<uint64>
                | ShortDictionary _ -> self.HasGeneralDictionaryKeySymbolic<int16>
                | UShortDictionary _ -> self.HasGeneralDictionaryKeySymbolic<uint16>
                | AddrDictionary _ -> self.HasAddrDictionaryKeySymbolic
                | _ -> __unreachable__()
            read address key dictionaryType

        member private self.ReadGeneralSetKeySymbolic<'key when 'key : equality> address item setType =
            let key : heapCollectionKey<'key> = { address = address; key = item }
            let extractor (state : state) =
                let setType = state.SubstituteTypeVariablesIntoSetType setType
                let mrKey = MemoryRegionId.createSetsId setType
                getOrPutRegionCommon mrKey typeof<bool> state.memory.MemoryRegions
                :?> memoryRegion<heapCollectionKey<'key>, productRegion<vectorTime intervals, 'key points>>
            self.ReadGeneralSetRegion<'key> setType extractor (extractor state) false key

        member private self.ReadAddrSetKeySymbolic address item setType =
            let key : addrCollectionKey = { address = address; key = item }
            let extractor (state : state) =
                let setType = state.SubstituteTypeVariablesIntoSetType setType
                let mrKey = MemoryRegionId.createSetsId setType
                getOrPutRegionCommon mrKey typeof<bool> state.memory.MemoryRegions :?> addrSetsRegion
            self.ReadAddrSetRegion setType extractor (extractor state) false key

        member private self.ReadSetKeySymbolic address item setType =
            let read =
                match setType with
                | BoolSet _ -> self.ReadGeneralSetKeySymbolic<bool>
                | ByteSet _ -> self.ReadGeneralSetKeySymbolic<byte>
                | SByteSet _ -> self.ReadGeneralSetKeySymbolic<sbyte>
                | CharSet _ -> self.ReadGeneralSetKeySymbolic<char>
                | DecimalSet _ -> self.ReadGeneralSetKeySymbolic<decimal>
                | DoubleSet _ -> self.ReadGeneralSetKeySymbolic<double>
                | IntSet _ -> self.ReadGeneralSetKeySymbolic<int>
                | UIntSet _ -> self.ReadGeneralSetKeySymbolic<uint>
                | LongSet _ -> self.ReadGeneralSetKeySymbolic<int64>
                | ULongSet _ -> self.ReadGeneralSetKeySymbolic<uint64>
                | ShortSet _ -> self.ReadGeneralSetKeySymbolic<int16>
                | UShortSet _ -> self.ReadGeneralSetKeySymbolic<uint16>
                | AddrSet _ -> self.ReadAddrSetKeySymbolic
                | _ -> __unreachable__()
            read address item setType

        member private self.ReadArrayIndexSymbolic address indices arrayType =
            let indices = List.map (fun i -> primitiveCast i typeof<int>) indices
            let key = OneArrayIndexKey(address, indices)
            self.ReadArrayKeySymbolic key arrayType

        member private self.ReadArrayRangeSymbolic address fromIndices toIndices arrayType =
            let fromIndices = List.map (fun i -> primitiveCast i typeof<int>) fromIndices
            let toIndices = List.map (fun i -> primitiveCast i typeof<int>) toIndices
            let key = RangeArrayIndexKey(address, fromIndices, toIndices)
            self.ReadArrayKeySymbolic key arrayType

        member private self.ArrayRegionMemsetData concreteAddress data regionType region =
            let address = ConcreteHeapAddress concreteAddress
            let prepareData (index, value) =
                let key = OneArrayIndexKey(address, List.map (int >> makeNumber) index)
                let value = self.ObjToTerm regionType value
                key, value
            Seq.map prepareData data |> MemoryRegion.memset region

        member private self.ListRegionMemsetData concreteAddress data listType (region : listsRegion) =
            let address = ConcreteHeapAddress concreteAddress
            let regionType = listType.listValueType
            let prepareData idx value =
                let key = OneArrayIndexKey(address, [makeNumber idx])
                let value = self.ObjToTerm regionType value
                key, value
            Seq.mapi prepareData data |> MemoryRegion.memset region

        member private self.AddrDictionaryRegionMemsetData concreteAddress data dictionaryType (region : addrDictionariesRegion) =
            let address = ConcreteHeapAddress concreteAddress
            let keyType = dictionaryType.keyType
            let regionType = dictionaryType.valueType
            let prepareData (key, value) =
                let key : addrCollectionKey = { address = address; key = self.ObjToTerm keyType key }
                let value = self.ObjToTerm regionType (value :> obj)
                key, value
            Seq.map prepareData data |> MemoryRegion.memset region

        member private self.GeneralDictionaryRegionMemsetData<'key when 'key : equality> concreteAddress data dictionaryType region =
            let address = ConcreteHeapAddress concreteAddress
            let keyType = dictionaryType.keyType
            let regionType = dictionaryType.valueType
            let prepareData (key, value) =
                let key : heapCollectionKey<'key> = { address = address; key = self.ObjToTerm keyType key }
                let value = self.ObjToTerm regionType (value :> obj)
                key, value
            Seq.map prepareData data |> MemoryRegion.memset region

        member private self.GeneralSetRegionMemsetData<'key when 'key : equality> concreteAddress (data : seq<obj>) itemType region =
            let address = ConcreteHeapAddress concreteAddress
            let prepareData item =
                let key : heapCollectionKey<'key> = { address = address; key = self.ObjToTerm itemType item }
                key, True()
            Seq.map prepareData data |> MemoryRegion.memset region

        member private self.AddrSetRegionMemsetData concreteAddress data itemType region =
            let address = ConcreteHeapAddress concreteAddress
            let prepareData item =
                let key : addrCollectionKey = { address = address; key = self.ObjToTerm itemType item }
                key, True()
            Seq.map prepareData data |> MemoryRegion.memset region

        member private self.ArrayRegionFromData concreteAddress data regionType =
            let region = MemoryRegion.emptyWithExplicit regionType concreteAddress
            self.ArrayRegionMemsetData concreteAddress data regionType region

        member private self.ReadRangeFromConcreteArray concreteAddress arrayData fromIndices toIndices arrayType =
            let address = ConcreteHeapAddress concreteAddress
            let fromIndices = List.map (fun i -> primitiveCast i typeof<int>) fromIndices
            let toIndices = List.map (fun i -> primitiveCast i typeof<int>) toIndices
            let region = self.ArrayRegionFromData concreteAddress arrayData arrayType.elemType
            let key = RangeArrayIndexKey(address, fromIndices, toIndices)
            self.ReadArrayRegion arrayType (always region) region true key

        member private self.ReadSymbolicIndexFromConcreteArray concreteAddress arrayData indices arrayType =
            let address = ConcreteHeapAddress concreteAddress
            let region = self.ArrayRegionFromData concreteAddress arrayData arrayType.elemType
            let indices = List.map (fun i -> primitiveCast i typeof<int>) indices
            let key = OneArrayIndexKey(address, indices)
            self.ReadArrayRegion arrayType (always region) region true key

        member private self.ReadSymbolicIndexFromConcreteList concreteAddress listData index listType =
            let address = ConcreteHeapAddress concreteAddress
            let region = MemoryRegion.emptyWithExplicit listType.listValueType concreteAddress
            let region : listsRegion = self.ListRegionMemsetData concreteAddress listData listType region
            let key = OneArrayIndexKey(address, [index])
            self.ReadListRegion listType (always region) region true key

        member private self.ReadSymbolicKeyFromConcreteGeneralDictionary<'key when 'key : equality> concreteAddress data key dictionaryType =
            let address = ConcreteHeapAddress concreteAddress
            let region = MemoryRegion.emptyWithExplicit dictionaryType.valueType concreteAddress
            let region : memoryRegion<heapCollectionKey<'key>, productRegion<intervals<vectorTime>, points<'key>>> =
                self.GeneralDictionaryRegionMemsetData<'key> concreteAddress data dictionaryType region
            let key : heapCollectionKey<'key> = { address = address; key = key }
            self.ReadGeneralDictionaryRegion<'key> dictionaryType (always region) region true key

        member private self.ReadSymbolicKeyFromConcreteAddrDictionary concreteAddress data key dictionaryType =
            let address = ConcreteHeapAddress concreteAddress
            let regionType = dictionaryType.valueType
            let region = MemoryRegion.emptyWithExplicit regionType concreteAddress
            let region : addrDictionariesRegion = self.AddrDictionaryRegionMemsetData concreteAddress data dictionaryType region
            let key : addrCollectionKey = { address = address; key = key }
            self.ReadAddrDictionaryRegion dictionaryType (always region) region true key

        member private self.ReadSymbolicKeyFromConcreteDictionary concreteAddress data key dictionaryType =
            let read =
                match dictionaryType with
                | BoolDictionary _ -> self.ReadSymbolicKeyFromConcreteGeneralDictionary<bool>
                | ByteDictionary _ -> self.ReadSymbolicKeyFromConcreteGeneralDictionary<byte>
                | SByteDictionary _ -> self.ReadSymbolicKeyFromConcreteGeneralDictionary<sbyte>
                | CharDictionary _ -> self.ReadSymbolicKeyFromConcreteGeneralDictionary<char>
                | DecimalDictionary _ -> self.ReadSymbolicKeyFromConcreteGeneralDictionary<decimal>
                | DoubleDictionary _ -> self.ReadSymbolicKeyFromConcreteGeneralDictionary<double>
                | IntDictionary _ -> self.ReadSymbolicKeyFromConcreteGeneralDictionary<int>
                | UIntDictionary _ -> self.ReadSymbolicKeyFromConcreteGeneralDictionary<uint>
                | LongDictionary _ -> self.ReadSymbolicKeyFromConcreteGeneralDictionary<int64>
                | ULongDictionary _ -> self.ReadSymbolicKeyFromConcreteGeneralDictionary<uint64>
                | ShortDictionary _ -> self.ReadSymbolicKeyFromConcreteGeneralDictionary<int16>
                | UShortDictionary _ -> self.ReadSymbolicKeyFromConcreteGeneralDictionary<uint16>
                | AddrDictionary _ -> self.ReadSymbolicKeyFromConcreteAddrDictionary
                | _ -> __unreachable__()
            read concreteAddress data key dictionaryType

        member private self.HasSymbolicKeyFromConcreteGeneralDictionary<'key when 'key : equality> concreteAddress data key dictionaryType =
            let address = ConcreteHeapAddress concreteAddress
            let keyType = dictionaryType.keyType
            let region = MemoryRegion.emptyWithExplicit typeof<bool> concreteAddress
            let region : memoryRegion<heapCollectionKey<'key>, productRegion<intervals<vectorTime>, points<'key>>> =
                self.GeneralSetRegionMemsetData<'key> concreteAddress data keyType region
            let key : heapCollectionKey<'key> = { address = address; key = key }
            self.HasGeneralDictionaryRegion<'key> dictionaryType (always region) region true key

        member private self.HasSymbolicKeyFromConcreteAddrDictionary concreteAddress data key dictionaryType =
            let address = ConcreteHeapAddress concreteAddress
            let keyType = dictionaryType.keyType
            let region = MemoryRegion.emptyWithExplicit typeof<bool> concreteAddress
            let region : addrDictionaryKeysRegion = self.AddrSetRegionMemsetData concreteAddress data keyType region
            let key : addrCollectionKey = { address = address; key = key }
            self.HasAddrDictionaryRegion dictionaryType (always region) region true key

        member private self.DictionaryHasSymbolicKeyFromConcreteDictionary concreteAddress data key dictionaryType =
            let contains =
                match dictionaryType with
                | BoolDictionary _ -> self.HasSymbolicKeyFromConcreteGeneralDictionary<bool>
                | ByteDictionary _ -> self.HasSymbolicKeyFromConcreteGeneralDictionary<byte>
                | SByteDictionary _ -> self.HasSymbolicKeyFromConcreteGeneralDictionary<sbyte>
                | CharDictionary _ -> self.HasSymbolicKeyFromConcreteGeneralDictionary<char>
                | DecimalDictionary _ -> self.HasSymbolicKeyFromConcreteGeneralDictionary<decimal>
                | DoubleDictionary _ -> self.HasSymbolicKeyFromConcreteGeneralDictionary<double>
                | IntDictionary _ -> self.HasSymbolicKeyFromConcreteGeneralDictionary<int>
                | UIntDictionary _ -> self.HasSymbolicKeyFromConcreteGeneralDictionary<uint>
                | LongDictionary _ -> self.HasSymbolicKeyFromConcreteGeneralDictionary<int64>
                | ULongDictionary _ -> self.HasSymbolicKeyFromConcreteGeneralDictionary<uint64>
                | ShortDictionary _ -> self.HasSymbolicKeyFromConcreteGeneralDictionary<int16>
                | UShortDictionary _ -> self.HasSymbolicKeyFromConcreteGeneralDictionary<uint16>
                | AddrDictionary _ -> self.HasSymbolicKeyFromConcreteAddrDictionary
                | _ -> __unreachable__()
            contains concreteAddress data key dictionaryType

        member private self.ReadSymbolicKeyFromConcreteGeneralSet<'key when 'key : equality> concreteAddress data item setType =
            let address = ConcreteHeapAddress concreteAddress
            let itemType = setType.setValueType
            let region = MemoryRegion.emptyWithExplicit typeof<bool> concreteAddress
            let region : memoryRegion<heapCollectionKey<'key>, productRegion<intervals<vectorTime>, points<'key>>> =
                self.GeneralSetRegionMemsetData<'key> concreteAddress data itemType region
            let key : heapCollectionKey<'key> = { address = address; key = item }
            self.ReadGeneralSetRegion<'key> setType (always region) region true key

        member private self.ReadSymbolicKeyFromConcreteAddrSet concreteAddress data item setType =
            let address = ConcreteHeapAddress concreteAddress
            let itemType = setType.setValueType
            let region = MemoryRegion.emptyWithExplicit typeof<bool> concreteAddress
            let region : addrSetsRegion = self.AddrSetRegionMemsetData concreteAddress data itemType region
            let key : addrCollectionKey = { address = address; key = item }
            self.ReadAddrSetRegion setType (always region) region true key

        member private self.ReadSymbolicKeyFromConcreteSet concreteAddress data item setType =
            let read =
                match setType with
                | BoolSet _ -> self.ReadSymbolicKeyFromConcreteGeneralSet<bool>
                | ByteSet _ -> self.ReadSymbolicKeyFromConcreteGeneralSet<byte>
                | SByteSet _ -> self.ReadSymbolicKeyFromConcreteGeneralSet<sbyte>
                | CharSet _ -> self.ReadSymbolicKeyFromConcreteGeneralSet<char>
                | DecimalSet _ -> self.ReadSymbolicKeyFromConcreteGeneralSet<decimal>
                | DoubleSet _ -> self.ReadSymbolicKeyFromConcreteGeneralSet<double>
                | IntSet _ -> self.ReadSymbolicKeyFromConcreteGeneralSet<int>
                | UIntSet _ -> self.ReadSymbolicKeyFromConcreteGeneralSet<uint>
                | LongSet _ -> self.ReadSymbolicKeyFromConcreteGeneralSet<int64>
                | ULongSet _ -> self.ReadSymbolicKeyFromConcreteGeneralSet<uint64>
                | ShortSet _ -> self.ReadSymbolicKeyFromConcreteGeneralSet<int16>
                | UShortSet _ -> self.ReadSymbolicKeyFromConcreteGeneralSet<uint16>
                | AddrSet _ -> self.ReadSymbolicKeyFromConcreteAddrSet
                | _ -> __unreachable__()
            read concreteAddress data item setType

        member private self.ArrayMemsetData concreteAddress data arrayType =
            let arrayType = state.SubstituteTypeVariablesIntoArrayType arrayType
            let elemType = arrayType.elemType
            ensureConcreteType elemType
            let mrKey = MemoryRegionId.createArraysId arrayType
            let region = getOrPutRegion mrKey elemType :?> arraysRegion
            let region' = self.ArrayRegionMemsetData concreteAddress data elemType region
            let region' = MemoryRegion.addExplicitAddress concreteAddress region'
            memoryRegions <- PersistentDict.add mrKey region' memoryRegions

        member private self.GeneralDictionaryMemsetData<'key when 'key : equality> concreteAddress data valueType dictionaryType =
            let mrKey = MemoryRegionId.createDictionariesId dictionaryType
            let region = getOrPutRegion mrKey valueType :?> memoryRegion<heapCollectionKey<'key>, productRegion<intervals<vectorTime>, points<'key>>>
            let region' = self.GeneralDictionaryRegionMemsetData<'key> concreteAddress data dictionaryType region
            let region' = MemoryRegion.addExplicitAddress concreteAddress region'
            memoryRegions <- PersistentDict.add mrKey region' memoryRegions

        member private self.AddrDictionaryMemsetData concreteAddress data valueType dictionaryType =
            let mrKey = MemoryRegionId.createDictionariesId dictionaryType
            let region = getOrPutRegion mrKey valueType :?> addrDictionariesRegion
            let region' = self.AddrDictionaryRegionMemsetData concreteAddress data dictionaryType region
            let region' = MemoryRegion.addExplicitAddress concreteAddress region'
            memoryRegions <- PersistentDict.add mrKey region' memoryRegions

        member private self.DictionaryMemsetData concreteAddress data dictionaryType =
            let dictionaryType = state.SubstituteTypeVariablesIntoDictionaryType dictionaryType
            let valueType = dictionaryType.valueType
            ensureConcreteType valueType
            let memset =
                match dictionaryType with
                | BoolDictionary _ -> self.GeneralDictionaryMemsetData<bool>
                | ByteDictionary _ -> self.GeneralDictionaryMemsetData<byte>
                | SByteDictionary _ -> self.GeneralDictionaryMemsetData<sbyte>
                | CharDictionary _ -> self.GeneralDictionaryMemsetData<char>
                | DecimalDictionary _ -> self.GeneralDictionaryMemsetData<decimal>
                | DoubleDictionary _ -> self.GeneralDictionaryMemsetData<double>
                | IntDictionary _ -> self.GeneralDictionaryMemsetData<int>
                | UIntDictionary _ -> self.GeneralDictionaryMemsetData<uint>
                | LongDictionary _ -> self.GeneralDictionaryMemsetData<int64>
                | ULongDictionary _ -> self.GeneralDictionaryMemsetData<uint64>
                | ShortDictionary _ -> self.GeneralDictionaryMemsetData<int16>
                | UShortDictionary _ -> self.GeneralDictionaryMemsetData<uint16>
                | AddrDictionary _ -> self.AddrDictionaryMemsetData
                | kt -> internalfail $"Dictionary memset data: expected key type but get {kt}"
            memset concreteAddress data valueType dictionaryType

        member private self.GeneralSetMemsetData<'key when 'key : equality> concreteAddress data setType =
            let mrKey = MemoryRegionId.createSetsId setType
            let region = getOrPutRegion mrKey typeof<bool> :?> memoryRegion<heapCollectionKey<'key>, productRegion<intervals<vectorTime>, points<'key>>>
            let region' = self.GeneralSetRegionMemsetData<'key> concreteAddress data setType.setValueType region
            let region' = MemoryRegion.addExplicitAddress concreteAddress region'
            memoryRegions <- PersistentDict.add mrKey region' memoryRegions

        member private self.AddrSetMemsetData concreteAddress data setType =
            let mrKey = MemoryRegionId.createSetsId setType
            let region = getOrPutRegion mrKey typeof<bool> :?> addrSetsRegion
            let region' = self.AddrSetRegionMemsetData concreteAddress data setType.setValueType region
            let region' = MemoryRegion.addExplicitAddress concreteAddress region'
            memoryRegions <- PersistentDict.add mrKey region' memoryRegions

        member private self.SetMemsetData concreteAddress data setType =
            let setType = state.SubstituteTypeVariablesIntoSetType setType
            let itemType = setType.setValueType
            ensureConcreteType itemType
            let memset =
                match setType with
                | BoolSet _ -> self.GeneralSetMemsetData<bool>
                | ByteSet _ -> self.GeneralSetMemsetData<byte>
                | SByteSet _ -> self.GeneralSetMemsetData<sbyte>
                | CharSet _ -> self.GeneralSetMemsetData<char>
                | DecimalSet _ -> self.GeneralSetMemsetData<decimal>
                | DoubleSet _ -> self.GeneralSetMemsetData<double>
                | IntSet _ -> self.GeneralSetMemsetData<int>
                | UIntSet _ -> self.GeneralSetMemsetData<uint>
                | LongSet _ -> self.GeneralSetMemsetData<int64>
                | ULongSet _ -> self.GeneralSetMemsetData<uint64>
                | ShortSet _ -> self.GeneralSetMemsetData<int16>
                | UShortSet _ -> self.GeneralSetMemsetData<uint16>
                | AddrSet _ -> self.AddrSetMemsetData
                | st -> internalfail $"Set memset data: expected item type but get {st}"
            memset concreteAddress data setType

        member private self.ListMemsetData concreteAddress data listType =
            let mrKey = MemoryRegionId.createListsId listType
            let region = getOrPutRegion mrKey listType.listValueType :?> listsRegion
            let region' = self.ListRegionMemsetData concreteAddress data listType region
            let region' = MemoryRegion.addExplicitAddress concreteAddress region'
            memoryRegions <- PersistentDict.add mrKey region' memoryRegions

        member private self.MakeSymbolicStackRead key typ time =
            let source = {key = key; time = time}
            let name = toString key
            self.MakeSymbolicValue source name typ

        member private self.ReadLowerBound address dimension arrayType =
            let cm = concreteMemory
            match address.term, dimension.term with
            | ConcreteHeapAddress address, Concrete(:? int as dim, _) when cm.Contains address ->
                cm.ReadArrayLowerBound address dim |> self.ObjToTerm typeof<int>
            | _ -> self.ReadLowerBoundSymbolic address dimension arrayType

        member private self.ReadLength address dimension arrayType =
            let cm = concreteMemory
            match address.term, dimension.term with
            | ConcreteHeapAddress address, Concrete(:? int as dim, _) when cm.Contains address ->
                cm.ReadArrayLength address dim |> self.ObjToTerm typeof<int>
            | _ -> self.ReadLengthSymbolic address dimension arrayType

        member private self.ReadDictionaryCount address dictionaryType =
            let cm = concreteMemory
            match address.term with
            | ConcreteHeapAddress address when cm.Contains address ->
                cm.ReadDictionaryCount address |> self.ObjToTerm typeof<int>
            | _ -> self.ReadDictionaryCountSymbolic address dictionaryType

        member private self.ReadSetCount address setType =
            let cm = concreteMemory
            match address.term with
            | ConcreteHeapAddress address when cm.Contains address ->
                cm.ReadSetCount address |> self.ObjToTerm typeof<int>
            | _ -> self.ReadSetCountSymbolic address setType

        member private self.ReadListCount address listType =
            let cm = concreteMemory
            match address.term with
            | ConcreteHeapAddress address when cm.Contains address ->
                cm.ReadListCount address |> self.ObjToTerm typeof<int>
            | _ -> self.ReadListCountSymbolic address listType

        member private self.ReadArrayIndex address indices arrayType =
            let cm = concreteMemory
            let concreteIndices = tryIntListFromTermList indices
            match address.term, concreteIndices with
            | ConcreteHeapAddress address, Some concreteIndices when cm.Contains address ->
                cm.ReadArrayIndex address concreteIndices |> self.ObjToTerm arrayType.elemType
            | ConcreteHeapAddress concreteAddress, None when cm.Contains concreteAddress ->
                let data = cm.GetAllArrayData concreteAddress
                self.ReadSymbolicIndexFromConcreteArray concreteAddress data indices arrayType
            // TODO: remember all concrete data from 'ConcreteMemory' and add it to symbolic constant [Test: ConcreteDictionaryTest1]
            | _ -> self.ReadArrayIndexSymbolic address indices arrayType

        member private self.ReadDictionaryKey address key dictionaryType =
            let cm = concreteMemory
            let concreteKey = tryCollectionKeyFromTerm key
            match address.term, concreteKey with
            | ConcreteHeapAddress address, Some concreteKey when cm.Contains address ->
                cm.ReadDictionaryKey address concreteKey |> self.ObjToTerm dictionaryType.valueType
            | ConcreteHeapAddress concreteAddress, None when cm.Contains concreteAddress ->
                let data = cm.GetAllDictionaryData concreteAddress
                self.ReadSymbolicKeyFromConcreteDictionary concreteAddress data key dictionaryType
            | _ -> self.ReadDictionaryKeySymbolic address key dictionaryType

        member private self.ReadDictionaryKeyContains address key dictionaryType =
            let cm = concreteMemory
            let concreteKey = tryCollectionKeyFromTerm key
            match address.term, concreteKey with
            | ConcreteHeapAddress address, Some concreteKey when cm.Contains address ->
                cm.DictionaryHasKey address concreteKey |> makeBool
            | ConcreteHeapAddress concreteAddress, None when cm.Contains concreteAddress ->
                let data = Seq.map fst <| cm.GetAllDictionaryData concreteAddress
                self.DictionaryHasSymbolicKeyFromConcreteDictionary concreteAddress data key dictionaryType
            | _ -> self.DictionaryHasKeySymbolic address key dictionaryType

        member private self.ReadSetKey address item setType =
            let cm = concreteMemory
            let concreteItem = tryCollectionKeyFromTerm item
            match address.term, concreteItem with
            | ConcreteHeapAddress address, Some concreteItem when cm.Contains address ->
                cm.ReadSetKey address concreteItem |> makeBool
            | ConcreteHeapAddress concreteAddress, None when cm.Contains concreteAddress ->
                let data = cm.GetAllSetData concreteAddress
                self.ReadSymbolicKeyFromConcreteSet concreteAddress data item setType
            | _ -> self.ReadSetKeySymbolic address item setType

        member private self.ReadListIndex address index listType =
            let cm = concreteMemory
            let concreteIndex = tryIntFromTerm index
            match address.term, concreteIndex with
            | ConcreteHeapAddress address, Some concreteIndex when cm.Contains address ->
                cm.ReadListIndex address concreteIndex |> self.ObjToTerm listType.listValueType
            | ConcreteHeapAddress concreteAddress, None when cm.Contains concreteAddress ->
                let data = cm.GetAllListData concreteAddress
                self.ReadSymbolicIndexFromConcreteList concreteAddress data index listType
            | _ -> self.ReadListIndexSymbolic address index listType

        member private self.CommonReadClassFieldSymbolic address (field : fieldId) =
            let symbolicType = field.typ
            let extractor (state : state) =
                let field = state.SubstituteTypeVariablesIntoField field
                let typ = state.SubstituteTypeVariables symbolicType
                let mrKey = MemoryRegionId.createClassFieldsId field
                getOrPutRegionCommon mrKey typ state.memory.MemoryRegions :?> classFieldsRegion
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
                self.MakeSymbolicHeapRead picker key time typ memory
            MemoryRegion.read region key (isDefault state) instantiate self.RangeReadingUnreachable

        member private self.ReadBoxedSymbolic address typ =
            let extractor (state : state) =
                let mrKey = MemoryRegionId.createBoxedLocationsId typ
                getOrPutRegionCommon mrKey typ state.memory.MemoryRegions :?> boxedLocationsRegion
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
                self.MakeSymbolicHeapRead picker key time typ memory
            MemoryRegion.read region key (isDefault state) instantiate self.RangeReadingUnreachable

        member private self.ReadClassFieldSymbolic address (field : fieldId) =
            if field = Reflection.stringFirstCharField then
                let arrayAddress, arrayType = self.StringArrayInfo address None
                self.ReadArrayIndexSymbolic arrayAddress [makeNumber 0] arrayType
            else self.CommonReadClassFieldSymbolic address field

        member private self.ReadClassField address (field : fieldId) =
            match address.term with
            | ConcreteHeapAddress address when concreteMemory.Contains address ->
                concreteMemory.ReadClassField address field |> self.ObjToTerm field.typ
            | _ -> self.ReadClassFieldSymbolic address field

        member private self.ReadStaticField typ (field : fieldId) =
            let extractor (state : state) =
                let field = state.SubstituteTypeVariablesIntoField field
                let typ = state.SubstituteTypeVariables field.typ
                let mrKey = MemoryRegionId.createStaticFieldsId field
                getOrPutRegionCommon mrKey typ state.memory.MemoryRegions :?> staticFieldsRegion
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
                self.MakeSymbolicHeapRead picker key state.startingTime typ memoryRegion
            MemoryRegion.read (extractor state) key (isDefault state) inst self.RangeReadingUnreachable

        member private self.ReadStackBuffer (stackKey : stackKey) index =
            let extractor (state : state) =
                let mrKey = MemoryRegionId.createStackBuffersId <| stackKey.Map state.TypeVariableSubst
                getOrPutRegionCommon mrKey typeof<int8> state.memory.MemoryRegions :?> stackBuffersRegion
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
                self.MakeSymbolicHeapRead picker key state.startingTime typ memoryRegion
            MemoryRegion.read (extractor state) key (isDefault state) inst self.RangeReadingUnreachable

        member private self.ReadBoxedLocation (address : term) sightType =
            assert(isBoxedType sightType)
            let typeFromMemory = self.TypeOfHeapLocation address
            let typ = mostConcreteType typeFromMemory sightType
            match memoryMode, address.term with
            | ConcreteMode, ConcreteHeapAddress address when concreteMemory.Contains address ->
                let value = concreteMemory.ReadBoxedLocation address
                self.ObjToTerm typ value
            | _ -> self.ReadBoxedSymbolic address typ

        member private self.ReadSafe reporter = function
            | PrimitiveStackLocation key -> self.ReadStackLocation key
            | ClassField(address, field) -> self.ReadClassField address field
            // [NOTE] ref must be the most concrete, otherwise region will be not found
            | ArrayIndex(address, indices, typ) -> self.ReadArrayIndex address indices typ
            | StaticField(typ, field) -> self.ReadStaticField typ field
            | StructField(address, field) ->
                let structTerm = self.ReadSafe reporter address
                self.ReadStruct reporter structTerm field
            | ArrayLength(address, dimension, typ) -> self.ReadLength address dimension typ
            | BoxedLocation(address, typ) -> self.ReadBoxedLocation address typ
            | StackBufferIndex(key, index) -> self.ReadStackBuffer key index
            | ArrayLowerBound(address, dimension, typ) -> self.ReadLowerBound address dimension typ
            | DictionaryKey(address, key, typ) -> self.ReadDictionaryKey address key typ
            | DictionaryCount(address, typ) -> self.ReadDictionaryCount address typ
            | DictionaryHasKey(address, key, typ) -> self.ReadDictionaryKeyContains address key typ
            | SetKey(address, item, typ) -> self.ReadSetKey address item typ
            | SetCount(address, typ) -> self.ReadSetCount address typ
            | ListIndex(address, index, typ) -> self.ReadListIndex address index typ
            | ListCount(address, typ) -> self.ReadListCount address typ

    // ------------------------------- Unsafe reading -------------------------------

        member private self.CheckBlockBounds (reporter : IErrorReporter) blockSize startByte endByte =
            let zero = makeNumber 0
            let failCondition =
                simplifyLess startByte zero id
                ||| simplifyGreaterOrEqual startByte blockSize id
                ||| simplifyLessOrEqual endByte zero id
                ||| simplifyGreater endByte blockSize id
            reporter.ReportFatalError "reading out of block bounds" failCondition

        member private self.ReadAddressUnsafe (reporter : IErrorReporter) address startByte endByte =
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

        member private self.SliceTerm term startByte endByte pos stablePos =
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
        member private self.CommonReadTermUnsafe (reporter : IErrorReporter) term startByte endByte pos stablePos sightType =
            match term.term, sightType with
            | Slice _, _ ->
                self.SliceTerm term startByte endByte pos stablePos |> List.singleton
            | _, Some sightType when
                startByte = makeNumber 0 &&
                let typ = typeOf term
                let size = internalSizeOf typ
                endByte = makeNumber size && typ = sightType ->
                    List.singleton term
            | Struct(fields, t), _ -> self.CommonReadStructUnsafe reporter fields t startByte endByte pos stablePos sightType
            | HeapRef _, _
            | Ref _, _
            | Ptr _, _ -> self.ReadAddressUnsafe reporter term startByte endByte
            | Combined([t], _), _ -> self.CommonReadTermUnsafe reporter t startByte endByte pos stablePos sightType
            | Combined(slices, _), _ ->
                let readSlice part = self.CommonReadTermUnsafe reporter part startByte endByte pos stablePos sightType
                List.collect readSlice slices
            | Concrete _, _
            | Constant _, _
            | Expression _, _ ->
                self.SliceTerm term startByte endByte pos stablePos |> List.singleton
            | Ite iteType, _ ->
                let mapper term = self.CommonReadTermUnsafe reporter term startByte endByte pos stablePos sightType
                let mappedIte = iteType.mapValues mapper
                assert(List.forall (fun (_, list) -> List.length list = 1) mappedIte.branches && List.length mappedIte.elseValue = 1)
                mappedIte.mapValues List.head |> Merging.merge |> List.singleton
            | _ -> internalfailf $"readTermUnsafe: unexpected term {term}"

        member private self.ReadTermUnsafe reporter term startByte endByte sightType =
            self.CommonReadTermUnsafe reporter term startByte endByte (neg startByte) false sightType

        member private self.ReadTermPartUnsafe reporter term startByte endByte sightType =
            self.CommonReadTermUnsafe reporter term startByte endByte startByte true sightType

        member private self.CommonReadStructUnsafe reporter fields structType startByte endByte pos stablePos sightType =
            let readField fieldId = fields[fieldId]
            self.CommonReadFieldsUnsafe reporter readField false structType startByte endByte pos stablePos sightType

        member private self.ReadStructUnsafe reporter fields structType startByte endByte sightType =
            self.CommonReadStructUnsafe reporter fields structType startByte endByte (neg startByte) false sightType

        member private self.GetAffectedFields reporter readField isStatic (blockType : Type) startByte endByte =
            // TODO: incorrect in case of static field
            let blockSize = Reflection.blockSize blockType
            let inBlock =
                isValueType blockType
                || self.CheckBlockBounds reporter (makeNumber blockSize) startByte endByte
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

        member private self.CommonReadFieldsUnsafe reporter readField isStatic (blockType : Type) startByte endByte pos stablePos sightType =
            let affectedFields = self.GetAffectedFields reporter readField isStatic blockType startByte endByte
            let readField (_, o, v, s, e) =
                self.CommonReadTermUnsafe reporter v s e (add pos o) stablePos sightType
            List.collect readField affectedFields

        member private self.ReadFieldsUnsafe reporter readField isStatic (blockType : Type) startByte endByte sightType =
            self.CommonReadFieldsUnsafe reporter readField isStatic blockType startByte endByte (neg startByte) false sightType

        // TODO: Add undefined behaviour:
        // TODO: 1. when reading info between fields
        // TODO: 3. when reading info outside block
        // TODO: 3. reinterpreting ref or ptr should return symbolic ref or ptr
        member private self.ReadClassUnsafe reporter address classType offset (viewSize : int) sightType =
            let endByte = makeNumber viewSize |> add offset
            let readField fieldId = self.ReadClassField address fieldId
            self.ReadFieldsUnsafe reporter readField false classType offset endByte sightType

        member private self.GetAffectedIndices reporter address ({elemType = elementType; dimension = dim} as arrayType) offset viewSize =
            let concreteElementSize = internalSizeOf elementType
            let elementSize = makeNumber concreteElementSize
            let lens = List.init dim (fun dim -> self.ReadLength address (makeNumber dim) arrayType)
            let lbs = List.init dim (fun dim -> self.ReadLowerBound address (makeNumber dim) arrayType)
            let arraySize = List.fold mul elementSize lens
            let inBlock = self.CheckBlockBounds reporter arraySize offset (makeNumber viewSize |> add offset)
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
                    let element = self.ReadArrayIndex address indices arrayType
                    let startByte = sub offset currentOffset
                    let endByte = makeNumber viewSize |> add startByte
                    (indices, element, startByte, endByte), add currentOffset elementSize
                List.mapFold getElement (mul firstElement elementSize) [0 .. countToRead - 1] |> fst
            else List.empty

        member private self.ReadArrayUnsafe reporter address arrayType offset viewSize sightType =
            let indices = self.GetAffectedIndices reporter address (symbolicTypeToArrayType arrayType) offset viewSize
            let readIndex (_, elem, s, e) =
                self.ReadTermUnsafe reporter elem s e sightType
            List.collect readIndex indices

        member private self.ReadStringUnsafe reporter address offset viewSize sightType =
             // TODO: handle case, when reading string length
            let address, arrayType = self.StringArrayInfo address None
            let indices = self.GetAffectedIndices reporter address arrayType offset viewSize
            let readChar (_, elem, s, e) =
                self.ReadTermUnsafe reporter elem s e sightType
            List.collect readChar indices

        member private self.ReadStaticUnsafe reporter t offset (viewSize : int) sightType =
            let endByte = makeNumber viewSize |> add offset
            let readField fieldId = self.ReadStaticField t fieldId
            self.ReadFieldsUnsafe reporter readField true t offset endByte sightType

        member private self.ReadStackUnsafe reporter loc offset (viewSize : int) sightType =
            let term = self.ReadStackLocation loc
            let locSize = sizeOf term |> makeNumber
            let endByte = makeNumber viewSize |> add offset
            let inBlock = self.CheckBlockBounds reporter locSize offset endByte
            if inBlock then self.ReadTermUnsafe reporter term offset endByte sightType
            else List.empty

        member private self.ReadBoxedUnsafe reporter loc typ offset viewSize sightType =
            let address = BoxedLocation(loc, typ)
            let endByte = makeNumber viewSize |> add offset
            match self.ReadSafe reporter address with
            | {term = Struct(fields, _)} -> self.ReadStructUnsafe reporter fields typ offset endByte sightType
            | term when isPrimitive typ || typ.IsEnum -> self.ReadTermUnsafe reporter term offset endByte sightType
            | term -> internalfail $"readUnsafe: reading struct resulted in term {term}"

        member private self.ReadUnsafe reporter baseAddress offset sightType =
            let viewSize = internalSizeOf sightType
            let slices =
                let sightType = Some sightType
                match baseAddress with
                | HeapLocation(loc, t) ->
                    let typ = self.MostConcreteTypeOfHeapRef loc t
                    match typ with
                    | StringType -> self.ReadStringUnsafe reporter loc offset viewSize sightType
                    | ClassType _ -> self.ReadClassUnsafe reporter loc typ offset viewSize sightType
                    | ArrayType _ -> self.ReadArrayUnsafe reporter loc typ offset viewSize sightType
                    | _ when typ = typeof<Void> -> internalfail $"readUnsafe: reading from 'Void' by reference {baseAddress}"
                    | StructType _ -> self.ReadBoxedUnsafe reporter loc typ offset viewSize sightType
                    | _ when isPrimitive typ || typ.IsEnum ->
                        self.ReadBoxedUnsafe reporter loc typ offset viewSize sightType
                    | _ -> internalfailf $"Expected complex type, but got {typ}"
                | StackLocation loc -> self.ReadStackUnsafe reporter loc offset viewSize sightType
                | StaticLocation loc -> self.ReadStaticUnsafe reporter loc offset viewSize sightType
            combine slices sightType

        member private self.ReadFieldUnsafe (reporter : IErrorReporter) (block : term) (field : fieldId) =
            let declaringType = field.declaringType
            match block.term with
            | Combined(_, t) when declaringType.IsAssignableFrom t || sizeOf block = internalSizeOf field.declaringType ->
                assert(sizeOf block = internalSizeOf field.declaringType)
                let fieldType = field.typ
                let startByte = Reflection.getFieldIdOffset field
                let endByte = startByte + internalSizeOf fieldType
                let sightType = Some fieldType
                let slices = self.ReadTermUnsafe reporter block (makeNumber startByte) (makeNumber endByte) sightType
                combine slices fieldType
            | Combined(slices, _) ->
                let isSuitableRef slice =
                    match slice.term with
                    | _ when isReference slice -> self.MostConcreteTypeOfRef slice |> declaringType.IsAssignableFrom
                    | Ptr(pointerBase, sightType, offset) ->
                        match self.TryPtrToRef pointerBase sightType offset with
                        | Some address -> declaringType.IsAssignableFrom(address.TypeOfLocation)
                        | None -> false
                    | _ -> false
                let refs = List.filter isSuitableRef slices |> List.distinct
                if List.length refs = 1 then
                    let ref = List.head refs
                    self.ReferenceField ref field |> self.Read reporter
                else internalfail $"readFieldUnsafe: unexpected block {block}"
            | _ -> internalfail $"readFieldUnsafe: unexpected block {block}"

    // -------------------------------- Pointer helpers --------------------------------

        member private self.TryPtrToRef pointerBase sightType offset : address option =
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
                let typ = self.TypeOfHeapLocation address |> mostConcreteType t
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
                            if typ = typeof<string> then self.StringArrayInfo address None
                            else address, arrayType.CreateVector sightType
                        ArrayIndex(address, [index], arrayType) |> Some
                    else None
                elif isValueType typ && suitableType typ && offset = zero then
                    BoxedLocation(address, t) |> Some
                else None
            | StackLocation stackKey when suitableType stackKey.TypeOfLocation && offset = zero ->
                PrimitiveStackLocation stackKey |> Some
            | _ -> None

        member private self.ReferenceField reference fieldId =
            let declaringType = fieldId.declaringType
            let isSuitableField address typ =
                let typ = self.MostConcreteTypeOfHeapRef address typ
                declaringType.IsAssignableFrom typ
            match reference.term with
            | HeapRef(address, typ) when isSuitableField address typ |> not ->
                // TODO: check this case with casting via "is"
                Logger.trace $"[WARNING] unsafe cast of term {reference} in safe context"
                let offset = Reflection.getFieldIdOffset fieldId |> makeNumber
                Ptr (HeapLocation(address, typ)) fieldId.typ offset
            | HeapRef(address, typ) when typ = typeof<string> && fieldId = Reflection.stringFirstCharField ->
                let address, arrayType = self.StringArrayInfo address None
                ArrayIndex(address, [makeNumber 0], arrayType) |> Ref
            | HeapRef(address, typ) when declaringType.IsValueType ->
                // TODO: Need to check mostConcreteTypeOfHeapRef using pathCondition?
                assert(isSuitableField address typ)
                let ref = heapReferenceToBoxReference reference
                self.ReferenceField ref fieldId
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
            | Ite iteType ->
                let filtered = iteType.filter (fun t -> True() <> Pointers.isBadRef t)
                let referenceField term = self.ReferenceField term fieldId
                Merging.guardedMap referenceField filtered
            | _ -> internalfailf $"Referencing field: expected reference, but got {reference}"

        member private self.ReferenceKey reference key typ =
            let t = symbolicTypeToDictionaryType typ
            Ref <| DictionaryKey(reference, key, t)

        member private self.DictionaryKeyContains dict key typ =
            let t = symbolicTypeToDictionaryType typ
            Ref <| DictionaryHasKey(dict, key, t)

        member private self.DictionaryCount dict typ =
            let t = symbolicTypeToDictionaryType typ
            Ref <| DictionaryCount(dict, t)

        member private self.SetKey reference item typ =
            let t = symbolicTypeToSetType typ
            Ref <| SetKey(reference, item, t)

        member private self.SetCount reference typ =
            let t = symbolicTypeToSetType typ
            Ref <| SetCount(reference, t)

        member private self.ListIndex reference index typ =
            let t = symbolicTypeToListType typ
            Ref <| ListIndex(reference, index, t)

        member private self.ListCount reference typ =
            let t = symbolicTypeToListType typ
            Ref <| ListCount(reference, t)

    // --------------------------- General reading ---------------------------

        // TODO: take type of heap address
        member private self.Read (reporter : IErrorReporter) reference =
            match reference.term with
            | Ref address -> self.ReadSafe reporter address
            | DetachedPtr _ ->
                reporter.ReportFatalError "reading by detached pointer" (True()) |> ignore
                Nop()
            | Ptr(baseAddress, sightType, offset) ->
                self.ReadUnsafe reporter baseAddress offset sightType
            | Ite iteType ->
                iteType.filter (fun v -> True() <> Pointers.isBadRef v)
                |> Merging.guardedMap (self.Read reporter)
            | _ when typeOf reference |> isNative ->
                reporter.ReportFatalError "reading by detached pointer" (True()) |> ignore
                Nop()
            | _ -> internalfailf $"Reading: expected reference, but got {reference}"

        // ----------------- Unmarshalling: from concrete to symbolic memory -----------------

        member private self.UnmarshallClass concreteAddress obj =
            let address = ConcreteHeapAddress concreteAddress
            let writeField (fieldId, fieldInfo : FieldInfo) =
                let value = fieldInfo.GetValue obj |> self.ObjToTerm fieldInfo.FieldType
                writeClassFieldSymbolic None address fieldId value
            let fields = obj.GetType() |> Reflection.fieldsOf false
            Array.iter writeField fields

        member private self.UnmarshallArray concreteAddress (array : Array) =
            let address = ConcreteHeapAddress concreteAddress
            let arrayType = array.GetType() |> symbolicTypeToArrayType
            let dim = arrayType.dimension
            let lbs = List.init dim array.GetLowerBound
            let lens = List.init dim array.GetLength
            let indicesWithValues = Array.getArrayIndicesWithValues array
            self.ArrayMemsetData concreteAddress indicesWithValues arrayType
            let lbToObj lb = self.ObjToTerm typeof<int> lb
            let lenToObj len = self.ObjToTerm typeof<int> len
            let termLBs = List.map lbToObj lbs
            let termLens = List.map lenToObj lens
            fillArrayBoundsSymbolic None address termLens termLBs arrayType

        member private self.FillDictionaryKeysSymbolic guard address data dictionaryType =
            let fill (key, _) =
                let key = self.ObjToTerm dictionaryType.keyType key
                writeDictionaryKeyToKeysSymbolic guard address key dictionaryType <| True()
            Seq.iter fill data

        member private self.FillDictionaryCountsSymbolic guard address data dictionaryType =
            let count = makeNumber <| Seq.length data
            writeDictionaryCount guard address dictionaryType count

        member private self.UnmarshallDictionary concreteAddress (dict : IDictionary) =
            let address = ConcreteHeapAddress concreteAddress
            let dictionaryType = dict.GetType() |> symbolicTypeToDictionaryType
            let data = Reflection.keyAndValueSeqFromDictionaryObj dict
            self.DictionaryMemsetData concreteAddress data dictionaryType
            self.FillDictionaryKeysSymbolic None address data dictionaryType
            self.FillDictionaryCountsSymbolic None address data dictionaryType

        member private self.FillSetCountsSymbolic guard address data setType =
            let count = Seq.length data |> makeNumber
            writeSetCount guard address setType count

        member private self.UnmarshallSet concreteAddress (set : obj) =
            let address = ConcreteHeapAddress concreteAddress
            let setType = set.GetType() |> symbolicTypeToSetType
            let data = seq { for item in (set :?> IEnumerable) -> item }
            self.SetMemsetData concreteAddress data setType
            self.FillSetCountsSymbolic None address data setType

        member private self.FillListCountsSymbolic guard address data listType =
            let count = Seq.length data |> makeNumber
            writeListCount guard address listType count

        member private self.UnmarshallList concreteAddress (list : obj) =
            let address = ConcreteHeapAddress concreteAddress
            let listType = list.GetType() |> symbolicTypeToListType
            let data = seq { for item in (list :?> IEnumerable) -> item }
            self.ListMemsetData concreteAddress data listType
            self.FillListCountsSymbolic None address data listType

        member private self.UnmarshallString concreteAddress (string : string) =
            let address = ConcreteHeapAddress concreteAddress
            let concreteStringLength = string.Length
            let stringLength = makeNumber concreteStringLength
            let address, _ = self.StringArrayInfo address (Some stringLength)
            writeClassFieldSymbolic None address Reflection.stringLengthField stringLength
            self.UnmarshallArray concreteAddress (string.ToCharArray())

        member private self.Unmarshall concreteAddress =
            let obj = concreteMemory.VirtToPhys concreteAddress
            assert(box obj <> null)
            concreteMemory.Remove concreteAddress
            match obj with
            | :? Array as array -> self.UnmarshallArray concreteAddress array
            | :? IDictionary as dict -> self.UnmarshallDictionary concreteAddress dict
            | Set set -> self.UnmarshallSet concreteAddress set
            | List list -> self.UnmarshallList concreteAddress list
            | :? String as string -> self.UnmarshallString concreteAddress string
            | _ -> self.UnmarshallClass concreteAddress obj

        // ------------------------------- Writing -------------------------------

        member private self.CommonWriteClassField guard address (field : fieldId) value =
            let concreteValue = self.TryTermToObj value
            match address.term, concreteValue, guard with
            | ConcreteHeapAddress concreteAddress, Some obj, None when concreteMemory.Contains concreteAddress ->
                concreteMemory.WriteClassField concreteAddress field obj
            | ConcreteHeapAddress concreteAddress, _, _ when concreteMemory.Contains concreteAddress ->
                self.Unmarshall concreteAddress
                writeClassFieldSymbolic guard address field value
            | _ -> writeClassFieldSymbolic guard address field value

        member private self.CommonWriteArrayIndex guard address indices arrayType value =
            let concreteValue = self.TryTermToObj value
            let concreteIndices = tryIntListFromTermList indices
            match address.term, concreteValue, concreteIndices, guard with
            | ConcreteHeapAddress a, Some obj, Some concreteIndices, None when concreteMemory.Contains a ->
                concreteMemory.WriteArrayIndex a concreteIndices obj
            | ConcreteHeapAddress a, _, _, _ when concreteMemory.Contains a ->
                self.Unmarshall a
                writeArrayIndexSymbolic guard address indices arrayType value
            | _ -> writeArrayIndexSymbolic guard address indices arrayType value

        member private self.CommonWriteDictionaryKey guard address key dictionaryType value =
            let concreteValue = self.TryTermToObj value
            let concreteKey = tryCollectionKeyFromTerm key
            match address.term, concreteValue, concreteKey, guard with
            | ConcreteHeapAddress a, Some obj, Some concreteKey, None when concreteMemory.Contains a ->
                concreteMemory.WriteDictionaryKey a concreteKey obj
            | ConcreteHeapAddress a, _, _, _ when concreteMemory.Contains a ->
                self.Unmarshall a
                writeDictionaryKeySymbolic guard address key dictionaryType value
            | _ -> writeDictionaryKeySymbolic guard address key dictionaryType value

        member private self.CommonWriteSetKey guard address item setType value =
            let concreteValue = self.TryTermToObj value
            let concreteKey = tryCollectionKeyFromTerm item
            match address.term, concreteValue, concreteKey, guard with
            | ConcreteHeapAddress a, Some obj, Some concreteKey, None when concreteMemory.Contains a ->
                concreteMemory.WriteSetKey a concreteKey obj
            | ConcreteHeapAddress a, _, _, _ when concreteMemory.Contains a ->
                self.Unmarshall a
                writeSetKeySymbolic guard address item setType value
            | _ -> writeSetKeySymbolic guard address item setType value

        member private self.CommonWriteListIndex guard address index listType value =
            let concreteValue = self.TryTermToObj value
            let concreteIndex = tryIntFromTerm index
            match address.term, concreteValue, concreteIndex, guard with
            | ConcreteHeapAddress a, Some obj, Some concreteIndex, None when concreteMemory.Contains a ->
                concreteMemory.WriteListIndex a concreteIndex obj
            | ConcreteHeapAddress a, _, _, _ when concreteMemory.Contains a ->
                self.Unmarshall a
                writeListIndexSymbolic guard address index listType value
            | _ -> writeListIndexSymbolic guard address index listType value

        member private self.ListRemoveAt address index (listType : listType) count =
            let concreteIndex = tryIntFromTerm index
            match address.term, concreteIndex with
            | ConcreteHeapAddress a, Some concreteIndex when concreteMemory.Contains a ->
                concreteMemory.ListRemoveAt a concreteIndex
            | ConcreteHeapAddress a, _ when concreteMemory.Contains a ->
                self.Unmarshall a
                let srcFrom = add index <| makeNumber 1
                let srcMemory = self.ReadListRangeSymbolic address srcFrom count listType
                let dstTo = sub count <| makeNumber 1
                writeListRangeSymbolic address index dstTo listType srcMemory
            | _ ->
                let srcFrom = add index <| makeNumber 1
                let srcMemory = self.ReadListRangeSymbolic address srcFrom count listType
                let dstTo = sub count <| makeNumber 1
                writeListRangeSymbolic address index dstTo listType srcMemory

        member private self.ListInsertIndex address index value (listType : listType) count =
            let concreteIndex = tryIntFromTerm index
            let concreteValue = self.TryTermToObj value
            match address.term, concreteIndex, concreteValue with
            | ConcreteHeapAddress a, Some concreteIndex, Some concreteValue when concreteMemory.Contains a ->
                concreteMemory.InsertIndex a concreteIndex concreteValue
            | ConcreteHeapAddress a, _, _ when concreteMemory.Contains a ->
                self.Unmarshall a
                let srcMemory = self.ReadListRangeSymbolic address index count listType
                let dstTo = add count <| makeNumber 1
                let dstFrom = add index <| makeNumber 1
                writeListRangeSymbolic address dstFrom dstTo listType srcMemory
                writeListIndexSymbolic None address index listType value
            | _ ->
                let srcMemory = self.ReadListRangeSymbolic address index count listType
                let dstTo = add count <| makeNumber 1
                let dstFrom = add index <| makeNumber 1
                writeListRangeSymbolic address dstFrom dstTo listType srcMemory
                writeListIndexSymbolic None address index listType value

        member private self.ListCopyToSymbolic list array index arrayIndex count listType arrayType =
            let srcTo = add index count
            let srcMemory = self.ReadListRangeSymbolic list index srcTo listType
            let dstTo = add arrayIndex count
            writeArrayRangeSymbolic None array [arrayIndex] [dstTo] arrayType srcMemory

        member private self.ListCopyToRange list index array arrayIndex count listType arrayType =
            let conIdx = tryIntFromTerm index
            let conArrayIdx = tryIntFromTerm arrayIndex
            let conCount = tryIntFromTerm count
            let cm = concreteMemory
            match list.term, array.term, conIdx, conArrayIdx, conCount with
            | ConcreteHeapAddress listAddr, ConcreteHeapAddress arrayAddr, Some conIdx, Some conArrayIdx, Some conCount
                when (cm.Contains listAddr) && (cm.Contains arrayAddr) ->
                cm.ListCopyToRange listAddr conIdx arrayAddr conArrayIdx conCount
            | ConcreteHeapAddress listAddr, _, _, _, _ when cm.Contains listAddr ->
                self.Unmarshall listAddr
                self.ListCopyToSymbolic list array index arrayIndex count listType arrayType
            | _, ConcreteHeapAddress arrayAddr, _, _, _ when cm.Contains arrayAddr ->
                self.Unmarshall arrayAddr
                self.ListCopyToSymbolic list array index arrayIndex count listType arrayType
            | _ -> self.ListCopyToSymbolic list array index arrayIndex count listType arrayType

        member private self.CommonWriteArrayRange guard address fromIndices toIndices arrayType value =
            let concreteValue = self.TryTermToObj value
            let concreteFromIndices = tryIntListFromTermList fromIndices
            let concreteToIndices = tryIntListFromTermList toIndices
            match address.term, concreteValue, concreteFromIndices, concreteToIndices, guard with
            | ConcreteHeapAddress a, Some v, Some l, Some r, None when concreteMemory.Contains a && List.length l = 1 ->
                assert(List.length r = 1)
                let l = List.head l
                let r = List.head r
                concreteMemory.FillArray a l (r - l) v
            | ConcreteHeapAddress a, _, _, _, _ when concreteMemory.Contains a ->
                self.Unmarshall a
                writeArrayRangeSymbolic guard address fromIndices toIndices arrayType value
            | _ -> writeArrayRangeSymbolic guard address fromIndices toIndices arrayType value

        // ------------------------------- Unsafe writing -------------------------------

        // [NOTE] guard is not needed, since the result of this function is either taken into ITE for structs or
        // we do unmarshall in callers and do not lose information
        member private self.WriteTermUnsafe reporter term startByte value =
            let termType = typeOf term
            let valueType = typeOf value
            match term.term with
            | _ when startByte = makeNumber 0 && termType = valueType -> value
            | Struct(fields, t) -> self.WriteStructUnsafe reporter term fields t startByte value
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
                    let left = self.ReadTermPartUnsafe reporter term zero startByte None
                    let valueSlices = self.ReadTermUnsafe reporter value (neg startByte) (sub termSize startByte) None
                    let right = self.ReadTermPartUnsafe reporter term (add startByte valueSize) termSize None
                    combine (left @ valueSlices @ right) termType
             | Ite gvs ->
                  let mapper term = self.WriteTermUnsafe reporter term startByte value
                  Merging.guardedMap mapper gvs
            | _ -> internalfailf $"writeTermUnsafe: unexpected term {term}"

        // [NOTE] guard is not needed, since the result of this function is taken into ITE with the old struct
        member private self.WriteStructUnsafe reporter structTerm fields structType startByte value =
            let readField fieldId = fields[fieldId]
            let updatedFields = self.WriteFieldsUnsafe reporter readField false structType startByte value
            let writeField structTerm (fieldId, value) = writeStruct structTerm fieldId value
            List.fold writeField structTerm updatedFields

        // [NOTE] guard is not needed, since if it is not True(), we do unmarshall and do not lose information
        member private self.WriteFieldsUnsafe reporter readField isStatic (blockType : Type) startByte value =
            let endByte = sizeOf value |> makeNumber |> add startByte
            let affectedFields = self.GetAffectedFields reporter readField isStatic blockType startByte endByte
            let writeField (id, _, v, s, _) = id, self.WriteTermUnsafe reporter v s value
            List.map writeField affectedFields

        member private self.WriteClassUnsafe reporter guard address typ offset value =
            let readField fieldId = self.ReadClassField address fieldId
            let updatedFields = self.WriteFieldsUnsafe reporter readField false typ offset value
            let writeField (fieldId, value) = self.CommonWriteClassField guard address fieldId value
            List.iter writeField updatedFields

        member private self.WriteArrayUnsafe reporter guard address arrayType offset value =
            let size = sizeOf value
            let arrayType = symbolicTypeToArrayType arrayType
            let affectedIndices = self.GetAffectedIndices reporter address arrayType offset size
            let writeElement (index, element, startByte, _) =
                let updatedElement = self.WriteTermUnsafe reporter element startByte value
                self.CommonWriteArrayIndex guard address index arrayType updatedElement
            List.iter writeElement affectedIndices

        member private self.WriteStringUnsafe reporter guard address offset value =
            let size = sizeOf value
            let address, arrayType = self.StringArrayInfo address None
            let affectedIndices = self.GetAffectedIndices reporter address arrayType offset size
            let writeElement (index, element, startByte, _) =
                let updatedElement = self.WriteTermUnsafe reporter element startByte value
                self.CommonWriteArrayIndex guard address index arrayType updatedElement
            List.iter writeElement affectedIndices

        member private self.WriteStaticUnsafe reporter guard staticType offset value =
            let readField fieldId = self.ReadStaticField staticType fieldId
            let updatedFields = self.WriteFieldsUnsafe reporter readField true staticType offset value
            let writeField (fieldId, value) = self.CommonWriteStaticField guard staticType fieldId value
            List.iter writeField updatedFields

        member private self.WriteStackUnsafe reporter loc offset value =
            let term = self.ReadStackLocation loc
            let locSize = sizeOf term |> makeNumber
            let endByte = sizeOf value |> makeNumber |> add offset
            let inBlock = self.CheckBlockBounds reporter locSize offset endByte
            if inBlock then
                let updatedTerm = self.WriteTermUnsafe reporter term offset value
                writeStackLocation loc updatedTerm

        member private self.WriteUnsafe reporter guard baseAddress offset value =
            match baseAddress with
            | HeapLocation(loc, sightType) ->
                let typ = self.MostConcreteTypeOfHeapRef loc sightType
                match typ with
                | StringType -> self.WriteStringUnsafe reporter guard loc offset value
                | ClassType _ -> self.WriteClassUnsafe reporter guard loc typ offset value
                | ArrayType _ -> self.WriteArrayUnsafe reporter guard loc typ offset value
                | StructType _ -> internalfail "writeUnsafe: unsafe writing is not implemented for structs" // TODO: boxed location?
                | _ -> internalfailf $"expected complex type, but got {typ}"
            | StackLocation loc -> self.WriteStackUnsafe reporter loc offset value
            | StaticLocation loc -> self.WriteStaticUnsafe reporter guard loc offset value

        // NOTE: using unsafe write instead of safe, when field intersects,
        // because need to write to all fields, which intersects with 'field'
        member private self.WriteIntersectingField reporter guard address (field : fieldId) value =
            let baseAddress, offset = Pointers.addressToBaseAndOffset address
            let ptr = Ptr baseAddress field.typ offset
            match ptr.term with
            | Ptr(baseAddress, _, offset) -> self.WriteUnsafe reporter guard baseAddress offset value
            | _ -> internalfailf $"expected to get ptr, but got {ptr}"

        member private self.WriteBoxedLocation guard (address : term) value =
            match memoryMode, address.term, self.TryTermToObj value with
            | ConcreteMode, ConcreteHeapAddress a, Some value when concreteMemory.Contains(a) ->
                concreteMemory.WriteBoxedLocation a value
            | ConcreteMode, ConcreteHeapAddress a, Some value ->
                concreteMemory.Allocate a value
            | ConcreteMode, ConcreteHeapAddress a, None when concreteMemory.Contains(a) ->
                concreteMemory.Remove a
                typeOf value |> writeBoxedLocationSymbolic guard address value
            | _ -> typeOf value |> writeBoxedLocationSymbolic guard address value

        member private self.WriteSafe reporter guard address value =
            match address with
            | PrimitiveStackLocation key ->
                assert Option.isNone guard
                writeStackLocation key value
            | ClassField(_, field)
            | StructField(_, field) when Reflection.fieldIntersects field ->
                self.WriteIntersectingField reporter guard address field value
            | ClassField(address, field) -> self.CommonWriteClassField guard address field value
            | ArrayIndex(address, indices, typ) -> self.CommonWriteArrayIndex guard address indices typ value
            | DictionaryKey(address, key, typ) -> self.CommonWriteDictionaryKey guard address key typ value
            | SetKey(address, item, typ) -> self.CommonWriteSetKey guard address item typ value
            | ListIndex(address, index, typ) -> self.CommonWriteListIndex guard address index typ value
            | StaticField(typ, field) -> self.CommonWriteStaticField guard typ field value
            | StructField(address, field) ->
                let oldStruct = self.ReadSafe reporter address
                let updatedStruct = guardedWriteStruct guard oldStruct field value
                self.WriteSafe reporter None address updatedStruct
            // TODO: need concrete memory for BoxedLocation?
            | BoxedLocation(address, _) -> self.WriteBoxedLocation guard address value
            | StackBufferIndex(key, index) -> writeStackBuffer key guard index value
            // NOTE: Cases below is needed to construct a model
            | ArrayLength(address, dimension, typ) -> writeLengthSymbolic guard address dimension typ value
            | ArrayLowerBound(address, dimension, typ) -> writeLowerBoundSymbolic guard address dimension typ value
            | DictionaryCount(address, typ) -> writeDictionaryCount guard address typ value
            | DictionaryHasKey(address, key, typ) -> writeDictionaryKeyToKeysSymbolic guard address key typ value
            | SetCount(address, typ) -> writeSetCount guard address typ value
            | ListCount(address, typ) -> writeListCount guard address typ value

        // TODO: unify allocation with unmarshalling
        member private self.CommonAllocateString length contents =
            match memoryMode, length.term with
            | ConcreteMode, Concrete(:? int as intLength, _) ->
                // TODO: implement interning (for String.Empty)
                let charArray : char array = Array.create intLength '\000'
                Seq.iteri (fun i char -> charArray.SetValue(char, i)) contents
                let string = new string(charArray) :> obj
                self.AllocateObjectIfNeed string typeof<string>
            | _ ->
                let arrayLength = add length (Concrete 1 lengthType)
                let address = self.AllocateConcreteVector typeof<char> arrayLength contents
                let address, _ = self.StringArrayInfo address (Some length)
                let heapAddress = getConcreteHeapAddress address
                self.CommonWriteClassField None address Reflection.stringLengthField length
                allocatedTypes <- PersistentDict.add heapAddress (ConcreteType typeof<string>) allocatedTypes
                address

        // ------------------------------- Delegates -------------------------------

        member private self.ObjToDelegate (d : Delegate) =
            let delegateType = d.GetType()
            let target = d.Target
            let target =
                if target <> null then
                    let targetType = target.GetType()
                    self.ObjToTerm targetType d.Target
                else nullRef typeof<obj>
            concreteDelegate d.Method target delegateType

        member private self.SimplifyDelegateRec acc d =
            match d.term with
            | CombinedDelegate delegates -> List.append delegates acc
            | ConcreteDelegate _ -> d :: acc
            | _ ->
                assert(isReference d)
                self.LinearizeDelegateRec d acc

        member private self.LinearizeDelegateRec d acc =
            if isReference d then
                match self.ReadDelegate d with
                | Some d -> self.SimplifyDelegateRec acc d
                | None -> d :: acc
            else self.SimplifyDelegateRec acc d

        member private self.SimplifyDelegate d =
            self.SimplifyDelegateRec List.empty d

        member private self.LinearizeDelegate ref =
            self.LinearizeDelegateRec ref List.empty

        member private self.GetDelegates address =
            match PersistentDict.tryFind delegates address with
            | Some d -> self.SimplifyDelegate d
            | None -> List.empty

        member private self.AllocateCombinedDelegateSymbolic concreteAddress delegateRefs t =
            let linearizedDelegates = List.collect self.LinearizeDelegate delegateRefs
            let combined = createCombinedDelegate linearizedDelegates t
            delegates <- PersistentDict.add concreteAddress combined delegates


        // ------------------------------- Stack -------------------------------

        member private self.NewStackFrame m frame =
            let newStack = CallStack.newStackFrame stack m frame
            let newEvaluationStack = EvaluationStack.newStackFrame evaluationStack
            stack <- newStack
            evaluationStack <- newEvaluationStack

        member private self.PopFrame() =
            let newStack = CallStack.popFrame stack
            let newEvaluationStack = EvaluationStack.popStackFrame evaluationStack
            stack <- newStack
            evaluationStack <- newEvaluationStack

        member private self.ForcePopFrames(count : int) =
            let newStack = CallStack.popFrames stack count
            let newEvaluationStack = EvaluationStack.forcePopStackFrames count evaluationStack
            stack <- newStack
            evaluationStack <- newEvaluationStack

        member private self.ReadStackLocation key =
            let makeSymbolic typ =
                if state.complete then makeDefaultValue typ
                else self.MakeSymbolicStackRead key typ state.startingTime
            CallStack.readStackLocation stack key makeSymbolic

    // ------------------------------- Types -------------------------------

        // TODO: use only MostConcreteTypeOfHeapRef someday
        member private self.TypeOfHeapLocation(address : heapAddress) =
            let getTypeOfAddress = term >> function
                | ConcreteHeapAddress address -> typeOfConcreteHeapAddress address
                | Constant(_, (:? IMemoryAccessConstantSource as source), AddressType) -> source.TypeOfLocation
                | _ -> __unreachable__()
            commonTypeOf getTypeOfAddress address

        member private self.MostConcreteTypeOfHeapRef address sightType =
            let locationType = self.TypeOfHeapLocation address
            if isAssignable locationType sightType then locationType
            else
                if isAssignable sightType locationType |> not then
                    if locationType = typeof<string> && sightType = typeof<char[]> || locationType = typeof<char[]> && sightType = typeof<string> then
                        typeof<string>
                    else
                        Logger.trace $"mostConcreteTypeOfHeapRef: Sight type ({sightType}) of address {address} differs from type in heap ({locationType})"
                        sightType
                else
                    sightType

        member private self.MostConcreteTypeOfRef(ref) =
            let getType ref =
                match ref.term with
                | HeapRef(address, sightType) -> self.MostConcreteTypeOfHeapRef address sightType
                | Ref address -> address.TypeOfLocation
                | Ptr(_, t, _) -> t
                | _ -> internalfail $"reading type token: expected heap reference, but got {ref}"
            commonTypeOf getType ref

        member private self.BaseTypeOfAddress(address) =
            match address with
            | BoxedLocation(address, _) -> self.TypeOfHeapLocation address
            | _ -> address.TypeOfLocation

        member private self.ArrayIndicesToOffset address ({elemType = elementType; dimension = dim} as arrayType) indices =
            let lens = List.init dim (fun dim -> self.ReadLength address (makeNumber dim) arrayType)
            let lbs = List.init dim (fun dim -> self.ReadLowerBound address (makeNumber dim) arrayType)
            let linearIndex = linearizeArrayIndex lens lbs indices
            mul linearIndex (internalSizeOf elementType |> makeNumber)

        member private self.ReadArrayRange address fromIndices toIndices arrayType =
            let cm = concreteMemory
            match address.term with
            | ConcreteHeapAddress concreteAddress when cm.Contains concreteAddress ->
                let data = cm.GetAllArrayData concreteAddress
                self.ReadRangeFromConcreteArray concreteAddress data fromIndices toIndices arrayType
            | _ -> self.ReadArrayRangeSymbolic address fromIndices toIndices arrayType

        member private self.StringArrayInfo stringAddress length =
            let arrayType = arrayType.CharVector
            if PersistentSet.contains stringAddress initializedAddresses then
                stringAddress, arrayType
            else
                initializedAddresses <- PersistentSet.add initializedAddresses stringAddress
                match stringAddress.term with
                | ConcreteHeapAddress cha when concreteMemory.Contains cha -> stringAddress, arrayType
                | _ ->
                    let zero = makeNumber 0
                    let stringLength =
                        match length with
                        | Some len -> len
                        | None -> self.CommonReadClassFieldSymbolic stringAddress Reflection.stringLengthField
                    let greaterZero = simplifyGreaterOrEqual stringLength zero id
                    state.AddConstraint greaterZero
                    let arrayLength = add stringLength (makeNumber 1)
                    writeLengthSymbolic None stringAddress zero arrayType arrayLength
                    writeLowerBoundSymbolic None stringAddress zero arrayType zero
                    let zeroChar = makeNumber '\000'
                    writeArrayIndexSymbolic None stringAddress [stringLength] arrayType zeroChar
                    stringAddress, arrayType

        member private self.ReadStruct reporter (structTerm : term) (field : fieldId) =
            match structTerm.term with
            | Struct(fields, _) -> fields[field]
            | Combined _ -> self.ReadFieldUnsafe reporter structTerm field
            | _ -> internalfail $"Reading field of structure: expected struct, but got {structTerm}"

        member private self.MakeSymbolicValue (source : ISymbolicConstantSource) name typ =
            match typ with
            | Bool
            | AddressType
            | Numeric _ -> Constant name source typ
            | StructType _ ->
                let makeField _ field typ =
                    let fieldSource = {baseSource = source; field = field}
                    self.MakeSymbolicValue fieldSource $"{name}.{field}" typ
                makeStruct false makeField typ
            | ReferenceType ->
                let addressSource : heapAddressSource = {baseSource = source}
                let address = self.MakeSymbolicValue addressSource name addressType
                HeapRef address typ
            | Pointer t ->
                let locationType =
                    if t = typeof<Void> then typeof<byte>.MakeArrayType()
                    else t.MakeArrayType()
                let addressSource : pointerAddressSource = {baseSource = source; locationType = locationType}
                let address = self.MakeSymbolicValue addressSource $"address of {name}" addressType
                let offsetSource : pointerOffsetSource = {baseSource = source}
                let offset = self.MakeSymbolicValue offsetSource $"offset of {name}" typeof<int>
                let pointerBase = HeapLocation(address, locationType)
                Ptr pointerBase t offset
            | ValueType -> __insufficientInformation__ $"Can't instantiate symbolic value of unknown value type {typ}"
            | ByRef _ -> __insufficientInformation__ $"Can't instantiate symbolic value of ByRef type {typ}"
            | _ -> __insufficientInformation__ $"Not sure which value to instantiate, because it's unknown if {typ} is a reference or a value type"

        // This function is used only for creating 'this' of reference types
        member private _.MakeSymbolicThis(m : IMethod) =
            let declaringType = m.DeclaringType
            assert(isValueType declaringType |> not)
            let source : heapAddressSource = {baseSource = {key = ThisKey m; time = VectorTime.zero}}
            let address = Constant "this" source addressType
            HeapRef address declaringType

        member private self.FillModelWithParametersAndThis (method : IMethod) =
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
            self.NewStackFrame None parametersAndThis

        // -------------------------- Allocation helpers --------------------------

        member private _.AllocateConcreteType (typ : Type) =
            assert(not typ.IsAbstract || isDelegate typ)
            allocateType(ConcreteType typ)

        member private _.AllocateMockType mock =
            allocateType(MockType mock)


        member private self.InitializeArray address indicesAndValues arrayType =
            let elementType = arrayType.elemType
            ensureConcreteType elementType
            let mrKey = MemoryRegionId.createArraysId arrayType
            let mr = getOrPutRegion mrKey elementType :?> arraysRegion
            let keysAndValues = Seq.map (fun (i, v) -> OneArrayIndexKey(address, i), v) indicesAndValues
            let mr' = MemoryRegion.memset mr keysAndValues
            memoryRegions <- PersistentDict.add mrKey mr' memoryRegions

        member private self.CommonWriteStaticField guard typ (field : fieldId) value =
            ensureConcreteType field.typ
            let fieldType =
                if isImplementationDetails typ then typeof<byte>.MakeArrayType()
                else field.typ
            let mrKey = MemoryRegionId.createStaticFieldsId field
            let mr = getOrPutRegion mrKey fieldType :?> staticFieldsRegion
            let key = {typ = typ}
            let mr' = MemoryRegion.write mr guard key value
            memoryRegions <- PersistentDict.add mrKey mr' memoryRegions
            let currentMethod = CallStack.getCurrentFunc stack
            if not currentMethod.IsStaticConstructor then
                concreteMemory.StaticFieldChanged field

        member private self.CommonWrite (reporter : IErrorReporter) guard reference value =
            assert(if Option.isSome guard then isTrue guard.Value |> not else true)
            let transformed = transformBoxedRef reference
            match transformed.term with
            | Ref address -> self.WriteSafe reporter guard address value
            | DetachedPtr _ -> reporter.ReportFatalError "writing by detached pointer" (True()) |> ignore
            | Ptr(address, _, offset) -> self.WriteUnsafe reporter guard address offset value
            | _ when typeOf reference |> isNative ->
                reporter.ReportFatalError "writing by detached pointer" (True()) |> ignore
            | Ite iteType ->
                let filtered = iteType.filter(fun r ->  Pointers.isBadRef r |> isTrue |> not)
                filtered.ToDisjunctiveGvs()
                |> List.iter (fun (g, r) -> self.CommonWrite reporter (Some g) r value)
            | _ -> internalfail $"Writing: expected reference, but got {reference}"

        // ------------------------------- Allocation -------------------------------
        member private self.AllocateOnStack key term =
            stack <- CallStack.allocate stack key term

        // Strings and delegates should be allocated using the corresponding functions (see allocateString and allocateDelegate)!
        member private self.AllocateClass typ =
            assert (not <| isSubtypeOrEqual typ typeof<String>)
            assert (not <| isSubtypeOrEqual typ typeof<Delegate>)
            let concreteAddress = self.AllocateConcreteType typ
            match memoryMode with
            // TODO: it's hack for reflection, remove it after concolic will be implemented
            | _ when isSubtypeOrEqual typ typeof<Type> -> ()
            | ConcreteMode ->
                let object = Reflection.createObject typ
                concreteMemory.Allocate concreteAddress object
            | SymbolicMode -> ()
            HeapRef (ConcreteHeapAddress concreteAddress) typ

        // TODO: unify allocation with unmarshalling
        member private self.AllocateArray typ lowerBounds lengths =
            assert (isSubtypeOrEqual typ typeof<Array>)
            let concreteAddress = self.AllocateConcreteType typ
            let arrayType = symbolicTypeToArrayType typ
            let address = ConcreteHeapAddress concreteAddress
            let concreteLengths = tryIntListFromTermList lengths
            let concreteLowerBounds = tryIntListFromTermList lowerBounds
            match memoryMode, concreteLengths, concreteLowerBounds with
            | ConcreteMode, Some concreteLengths, Some concreteLBs ->
                let elementDotNetType = elementType typ
                let array = Array.CreateInstance(elementDotNetType, Array.ofList concreteLengths, Array.ofList concreteLBs) :> obj
                concreteMemory.Allocate concreteAddress array
            | _ -> fillArrayBoundsSymbolic None address lengths lowerBounds arrayType
            address

        member private self.AllocateVector (elementType : Type) length =
            let typ = elementType.MakeArrayType()
            self.AllocateArray typ [makeNumber 0] [length]

        member private self.AllocateConcreteVector (elementType : Type) length contents =
            match memoryMode, length.term with
            | ConcreteMode, Concrete(:? int as intLength, _) ->
                let concreteAddress = self.AllocateConcreteType(elementType.MakeArrayType())
                let array = Array.CreateInstance(elementType, intLength)
                Seq.iteri (fun i value -> array.SetValue(value, i)) contents
                concreteMemory.Allocate concreteAddress (array :> obj)
                ConcreteHeapAddress concreteAddress
            | _ ->
                let address = self.AllocateVector elementType length
                let arrayType = arrayType.CreateVector elementType
                let mrKey = MemoryRegionId.createArraysId arrayType
                let mr = getOrPutRegion mrKey elementType :?> arraysRegion
                let keysAndValues = Seq.mapi (fun i v -> OneArrayIndexKey(address, [makeNumber i]), Concrete v elementType) contents
                let mr' = MemoryRegion.memset mr keysAndValues
                memoryRegions <- PersistentDict.add mrKey mr' memoryRegions
                address

        member private self.AllocateEmptyString length =
            let address = self.CommonAllocateString length Seq.empty
            HeapRef address typeof<string>

        member private self.AllocateString (str : string) =
            let address = self.CommonAllocateString (Concrete str.Length lengthType) str
            HeapRef address typeof<string>

        member private self.CreateStringFromChar char =
            match char.term with
            | Concrete(:? char as c, _) ->
                let string = c.ToString()
                self.AllocateString string
            | _ ->
                let len = makeNumber 1
                let address = self.CommonAllocateString len " "
                let address, arrayType = self.StringArrayInfo address (Some len)
                self.CommonWriteArrayIndex None address [Concrete 0 indexType] arrayType char
                HeapRef address typeof<string>

        member private self.AllocateBoxedLocation value =
            let typ = typeOf value
            let concreteAddress = self.AllocateConcreteType typ
            let address = ConcreteHeapAddress concreteAddress
            match memoryMode, self.TryTermToObj value with
            // 'value' may be null, if it's nullable value type
            | ConcreteMode, Some value when value <> null ->
                assert(value :? ValueType)
                concreteMemory.AllocateBoxedLocation concreteAddress value typ
            | _ -> writeBoxedLocationSymbolic None address value typ
            HeapRef address typeof<obj>

        member private self.AllocateConcreteObject obj (typ : Type) =
            assert(not typ.IsAbstract)
            match memoryMode with
            | ConcreteMode ->
                let address = self.AllocateObjectIfNeed obj typ
                HeapRef address typ
            | SymbolicMode -> internalfailf $"allocateConcreteObject: allocating concrete object {obj} in symbolic memory is not implemented"

        member private self.AllocateTemporaryLocalVariableOfType name index typ =
            let tmpKey = TemporaryLocalVariableKey(typ, index)
            let ref = PrimitiveStackLocation tmpKey |> Ref
            let value = self.MakeSymbolicValue {key = tmpKey; time = VectorTime.zero} name typ
            self.AllocateOnStack tmpKey value
            ref

        member private self.LengthOfString heapRef =
            match heapRef.term with
            | HeapRef(address, typ) ->
                assert(typ = typeof<string>)
                self.ReadClassField address Reflection.stringLengthField
            | Ite gvs -> Merging.guardedMap self.LengthOfString gvs
            | _ -> internalfail "Getting length of string: expected heap reference, but got %O" heapRef

        // ------------------------------- Delegates -------------------------------

        member private self.ReadDelegate reference =
            match reference.term with
            | HeapRef({term = ConcreteHeapAddress address}, _) when concreteMemory.Contains address ->
                let d = concreteMemory.ReadDelegate address
                let delegateType = d.GetType()
                let invokeList = d.GetInvocationList()
                if invokeList <> null then
                    let delegates = Array.map self.ObjToDelegate invokeList
                    if Array.length delegates = 1 then Array.head delegates |> Some
                    else createCombinedDelegate (Array.toList delegates) delegateType |> Some
                else self.ObjToDelegate d |> Some
            | HeapRef({term = ConcreteHeapAddress address}, _) -> delegates[address] |> Some
            | HeapRef _ -> None
            | Ite iteType ->
                let delegates = iteType.choose self.ReadDelegate
                if delegates.branches.Length = iteType.branches.Length then Merging.merge delegates |> Some else None
            | _ -> internalfailf $"Reading delegate: expected heap reference, but got {reference}"

        member private self.AllocateDelegate (methodInfo : MethodInfo) target delegateType =
            let concreteAddress = self.AllocateConcreteType delegateType
            let address = ConcreteHeapAddress concreteAddress
            match memoryMode, self.TryTermToObj target with
            | ConcreteMode, Some target ->
                let d = methodInfo.CreateDelegate(delegateType, target)
                concreteMemory.AllocateDelegate concreteAddress d
                HeapRef address delegateType
            | _ ->
                let d = concreteDelegate methodInfo target delegateType
                delegates <- PersistentDict.add concreteAddress d delegates
                HeapRef address delegateType

        member private self.AllocateCombinedDelegate concreteAddress (delegateRefs : term list) t =
            let concreteDelegates = self.TryTermListToObjects delegateRefs
            match memoryMode, concreteDelegates with
            | ConcreteMode, Some list ->
                assert(List.isEmpty list |> not)
                if List.length list = 1 then
                    concreteMemory.Allocate concreteAddress (List.head list)
                else
                    let delegates = Seq.cast list
                    let combined = Delegate.Combine (Seq.toArray delegates)
                    concreteMemory.Allocate concreteAddress combined
            | _ -> self.AllocateCombinedDelegateSymbolic concreteAddress delegateRefs t

        member private self.CombineDelegates (delegateRefs : term list) typ =
            assert(List.isEmpty delegateRefs |> not)
            let concreteAddress = self.AllocateConcreteType typ
            let address = ConcreteHeapAddress concreteAddress
            self.AllocateCombinedDelegate concreteAddress delegateRefs typ
            HeapRef address typ

        member private self.RemoveDelegate (sourceRef : term) (toRemoveRef : term) typ =
            let toRemove = self.TryTermToObj toRemoveRef
            match memoryMode, sourceRef.term, toRemove with
            | ConcreteMode, HeapRef({term = ConcreteHeapAddress a}, _), Some toRemove when concreteMemory.Contains a ->
                let source = concreteMemory.VirtToPhys a
                assert(source :? Delegate && toRemove :? Delegate)
                let source = source :?> Delegate
                let result = Delegate.Remove(source, toRemove :?> Delegate)
                if Object.ReferenceEquals(result, source) then sourceRef
                else self.ObjToTerm typeof<Delegate> result
            | _, HeapRef({term = ConcreteHeapAddress a}, _), _ ->
                let sourceDelegates = self.GetDelegates a
                let removeDelegates = self.SimplifyDelegate toRemoveRef
                let removed, result = List.removeSubList sourceDelegates removeDelegates
                if removed then
                    if List.isEmpty result then nullRef typ
                    else
                        let concreteAddress = self.AllocateConcreteType typ
                        self.AllocateCombinedDelegate concreteAddress result typ
                        HeapRef (ConcreteHeapAddress concreteAddress) typ
                else sourceRef
            | _ -> sourceRef

        member internal _.State
            with get() = state
            and set value = state <- value

        interface IMemory with

            override _.AllocatedTypes
                with get() = allocatedTypes
                and set value = allocatedTypes <- value
            override _.MemoryRegions
                with get() = memoryRegions
                and set value = memoryRegions <- value
            override _.ConcreteMemory with get() = concreteMemory
            override _.Delegates with get() = delegates
            override _.EvaluationStack
                with get() = evaluationStack
                and set value = evaluationStack <- value
            override _.InitializedAddresses with get() = initializedAddresses
            override _.MemoryMode
                with get() = memoryMode
                and set value = memoryMode <- value
            override _.Stack
                with get() = stack
                and set(callStack: callStack) = stack <- callStack

            override self.Copy() =
                let copy = Memory(
                        evaluationStack,
                        stack,
                        memoryRegions,
                        concreteMemory.Copy(),
                        allocatedTypes,
                        initializedAddresses,
                        delegates,
                        memoryMode,
                        state
                    )
                copy

            member self.ObjToTerm t obj = self.ObjToTerm t obj
            member self.Read reporter reference = self.Read reporter reference
            member self.ReadArrayIndex address indices arrayType = self.ReadArrayIndex address indices arrayType
            member self.ReadFieldUnsafe reporter block field = self.ReadFieldUnsafe reporter block field
            member self.ReadLength address dimension arrayType = self.ReadLength address dimension arrayType
            member self.ReadLowerBound address dimension arrayType = self.ReadLowerBound address dimension arrayType
            member self.ReadStaticField typ field = self.ReadStaticField typ field
            member self.ReferenceField reference fieldId = self.ReferenceField reference fieldId
            member self.ReferenceKey reference key typ = self.ReferenceKey reference key typ
            member self.DictionaryKeyContains reference key typ = self.DictionaryKeyContains reference key typ
            member self.DictionaryCount reference typ = self.DictionaryCount reference typ
            member self.SetKey reference item typ = self.SetKey reference item typ
            member self.SetCount reference typ = self.SetCount reference typ
            member self.ListIndex reference index typ = self.ListIndex reference index typ
            member self.ListCount reference typ = self.ListCount reference typ
            member self.ListRemoveAt reference index listType count = self.ListRemoveAt reference index listType count
            member self.ListInsertIndex reference index value listType count = self.ListInsertIndex reference index value listType count
            member self.ListCopyToRange list index array arrayIndex count listType arrayType = self.ListCopyToRange list index array arrayIndex count listType arrayType
            member self.TryPtrToRef pointerBase sightType offset = self.TryPtrToRef pointerBase sightType offset
            member self.TryTermToFullyConcreteObj term = self.TryTermToFullyConcreteObj term
            member self.TryTermToObj term = self.TryTermToObj term
            member self.Unmarshall concreteAddress = self.Unmarshall concreteAddress
            member self.WriteArrayIndex address indices arrayType value =
                self.CommonWriteArrayIndex None address indices arrayType value
            member self.WriteArrayRange address fromIndices toIndices arrayType value =
                self.CommonWriteArrayRange None address fromIndices toIndices arrayType value
            member self.WriteClassField address field value =
                self.CommonWriteClassField None address field value
            member self.GuardedWriteClassField guard address field value =
                self.CommonWriteClassField guard address field value

            member self.WriteStackLocation key value = writeStackLocation key value
            member self.AllocateArray typ lowerBounds lengths = self.AllocateArray typ lowerBounds lengths
            member self.AllocateBoxedLocation value = self.AllocateBoxedLocation value
            member self.AllocateClass typ = self.AllocateClass typ
            member self.AllocateCombinedDelegate concreteAddress delegateRefs t =
                self.AllocateCombinedDelegate concreteAddress delegateRefs t
            member self.AllocateConcreteObject obj typ = self.AllocateConcreteObject obj typ
            member self.AllocateConcreteType typ = self.AllocateConcreteType typ
            member self.AllocateConcreteVector elementType length contents =
                self.AllocateConcreteVector elementType length contents
            member self.AllocateDelegate methodInfo target delegateType =
                self.AllocateDelegate methodInfo target delegateType
            member self.AllocateEmptyString length = self.AllocateEmptyString length
            member self.AllocateMockType mock = self.AllocateMockType mock
            member self.AllocateOnStack key term = self.AllocateOnStack key term
            member self.AllocateString str = self.AllocateString str
            member self.AllocateTemporaryLocalVariableOfType name index typ =
                self.AllocateTemporaryLocalVariableOfType name index typ
            member self.AllocateVector elementType length = self.AllocateVector elementType length
            member self.ArrayIndicesToOffset address arrayType indices =
                self.ArrayIndicesToOffset address arrayType indices
            member self.BaseTypeOfAddress address = self.BaseTypeOfAddress address
            member self.CombineDelegates delegateRefs typ = self.CombineDelegates delegateRefs typ
            member self.CreateStringFromChar char = self.CreateStringFromChar char
            member self.FillModelWithParametersAndThis method= self.FillModelWithParametersAndThis method
            member self.ForcePopFrames count = self.ForcePopFrames count
            member self.InitializeArray address indicesAndValues arrayType =
                self.InitializeArray address indicesAndValues arrayType
            member self.LengthOfString heapRef = self.LengthOfString heapRef
            member self.MakeSymbolicThis method = self.MakeSymbolicThis method
            member self.MakeSymbolicValue source name typ = self.MakeSymbolicValue source name typ
            member self.MostConcreteTypeOfHeapRef address sightType = self.MostConcreteTypeOfHeapRef address sightType
            member self.MostConcreteTypeOfRef ref = self.MostConcreteTypeOfRef ref
            member self.NewStackFrame method frame = self.NewStackFrame method frame
            member self.PopFrame() = self.PopFrame()
            member self.SpecializedReading readKey utKey = self.SpecializedReading readKey utKey
            member self.ReadArrayRange address fromIndices toIndices arrayType =
                self.ReadArrayRange address fromIndices toIndices arrayType
            member self.ReadDelegate reference = self.ReadDelegate reference
            member self.ReadStackLocation key = self.ReadStackLocation key
            member self.ReadStruct reporter structTerm field = self.ReadStruct reporter structTerm field
            member self.RemoveDelegate sourceRef toRemoveRef typ = self.RemoveDelegate sourceRef toRemoveRef typ
            member self.StringArrayInfo stringAddress length = self.StringArrayInfo stringAddress length
            member self.TypeOfHeapLocation address = self.TypeOfHeapLocation address
            member self.Write reporter reference value = self.CommonWrite reporter None reference value
            member self.WriteStaticField typ field value = self.CommonWriteStaticField None typ field value

    type heapReading<'key, 'reg when 'key : equality and 'key :> IMemoryKey<'key, 'reg> and 'reg : equality and 'reg :> IRegion<'reg>> with
        interface IMemoryAccessConstantSource with
            override x.Compose state =
                // TODO: do nothing if state is empty!
                let substTerm = state.FillHoles
                let substType = state.SubstituteTypeVariables
                let substTime = state.ComposeTime
                let key = x.key.Map substTerm substType substTime x.key.Region |> snd
                let effect = MemoryRegion.map substTerm substType substTime x.memoryObject
                let before = x.picker.extract state
                let after = MemoryRegion.compose before effect
                assert(state.memory :? Memory)
                let memory = state.memory :?> Memory
                let inst typ region = memory.MakeSymbolicHeapRead x.picker key state.startingTime typ region
                MemoryRegion.read after key (x.picker.isDefaultKey state) inst memory.RangeReadingUnreachable

    type arrayReading with
        interface IMemoryAccessConstantSource with
            override x.Compose state =
                // TODO: do nothing if state is empty!
                let substTerm = state.FillHoles
                let substType = state.SubstituteTypeVariables
                let substTime = state.ComposeTime
                let key = x.key :> IHeapArrayKey
                let key = key.Map substTerm substType substTime key.Region |> snd
                let after =
                    if not x.picker.isDefaultRegion then
                        let effect = MemoryRegion.map substTerm substType substTime x.memoryObject
                        let before = x.picker.extract state
                        MemoryRegion.compose before effect
                    else x.memoryObject
                assert(state.memory :? Memory)
                let memory = state.memory :?> Memory
                let inst = memory.MakeArraySymbolicHeapRead x.picker key state.startingTime
                MemoryRegion.read after key (x.picker.isDefaultKey state) inst memory.SpecializedReading

    type state with
        static member MakeEmpty complete =
            let memory = Memory()
            let state = { memory.State with complete = complete }
            memory.State <- state
            state

        // TODO: think about moving it to State.fs and get rid of type downcast
        member x.Copy newPc =
            let memory = x.memory.Copy() :?> Memory
            let methodMocks = Dictionary()
            for entry in x.methodMocks do
                let method = entry.Key
                let newMock = entry.Value.Copy()
                methodMocks.Add(method, newMock)
            let state = { memory.State with pc = newPc; methodMocks = methodMocks; memory = memory }
            memory.State <- state
            state
