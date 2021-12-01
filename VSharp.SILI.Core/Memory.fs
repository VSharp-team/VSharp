namespace VSharp.Core

open System
open System.Collections.Generic
open System.Text
open FSharpx.Collections
open VSharp
open VSharp.Core.Types
open VSharp.Core.Types.Constructor
open VSharp.Utils

#nowarn "69"

type IMemoryAccessConstantSource =
    inherit IStatedSymbolicConstantSource
    abstract TypeOfLocation : symbolicType

module internal Memory =

// ------------------------------- Primitives -------------------------------
    
    type memoryMode =
        | ConcreteMemory
        | SymbolicMemory

    let mutable memoryMode = SymbolicMemory

    let copy state newPc =
        let state =
            match memoryMode with
            | ConcreteMemory -> ConcreteMemory.deepCopy state
            | SymbolicMemory -> state
        { state with pc = newPc }

    let private isZeroAddress (x : concreteHeapAddress) =
        x = VectorTime.zero

    let addConstraint (s : state) cond =
        s.pc <- PC.add s.pc cond

    let delinearizeArrayIndex ind lens lbs =
        let detachOne (acc, lens) lb =
            let lensProd = List.fold mul (makeNumber 1) (List.tail lens)
            let curOffset = div acc lensProd
            let curIndex = add curOffset lb
            let rest = rem acc lensProd
            curIndex, (rest, List.tail lens)
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
        let newMappedStack = subst |> List.fold (fun acc (k, v) -> MappedStack.push k v acc) oldMappedStack
        state.typeVariables <- (newMappedStack, newStack)

    let popTypeVariablesSubstitution state =
        let oldMappedStack, oldStack = state.typeVariables
        let toPop, newStack = Stack.pop oldStack
        let newMappedStack = List.fold MappedStack.remove oldMappedStack toPop
        state.typeVariables <- (newMappedStack, newStack)

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
        if isAssignable locationType sightType then locationType
        else
            assert(isAssignable sightType locationType)
            sightType

    let baseTypeOfAddress state address =
        match address with
        | BoxedLocation(addr, _) -> typeOfConcreteHeapAddress state addr
        | _ -> typeOfAddress address

// -------------------------------- GetHashCode --------------------------------

    [<StructuralEquality;NoComparison>]
    type private hashCodeSource =
        {object : term}
        interface IStatedSymbolicConstantSource with
            override x.SubTerms = Seq.empty
            override x.Time = VectorTime.zero            
            override x.IndependentWith otherSource =
                match otherSource with
                | :? hashCodeSource as otherHashCodeSource ->
                    match otherHashCodeSource.object, x.object with
                    | ConstantT(_, otherConstantSource, _), ConstantT(_, constantSource, _) ->
                        otherConstantSource.IndependentWith constantSource
                    | _ -> true
                | _ -> true 

    let hashConcreteAddress (address : concreteHeapAddress) =
        address.GetHashCode() |> makeNumber

    let getHashCode object =
        assert(isReference object)
        // TODO: implement GetHashCode() for value type (it's boxed)
        match object.term with
        | HeapRef({term = ConcreteHeapAddress address}, _) -> hashConcreteAddress address
        | HeapRef(address, _) ->
            let name = sprintf "HashCode(%O)" address
            let source = {object = address}
            Constant name source Int32
        | _ -> internalfailf "expected HeapRef, but got %O" object

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
        interface IMemoryAccessConstantSource with
            override x.SubTerms = Seq.empty
            override x.Time =
                match x.time with
                | Some time -> time
                | None -> internalfailf "Requesting time of primitive stack location %O" x.key
            override x.TypeOfLocation = x.key.TypeOfLocation
            override x.IndependentWith otherSource =
                match otherSource with
                | :? stackReading as otherReading -> x <> otherReading
                | _ -> true

    [<StructuralEquality;NoComparison>]
    type private heapReading<'key, 'reg when 'key : equality and 'key :> IMemoryKey<'key, 'reg> and 'reg : equality and 'reg :> IRegion<'reg>> =
        {picker : regionPicker<'key, 'reg>; key : 'key; memoryObject : memoryRegion<'key, 'reg>; time : vectorTime}
        interface IMemoryAccessConstantSource with
            override x.SubTerms =
                let addKeySubTerms _ (regionKey : updateTreeKey<'key, term>) acc =
                    Seq.fold PersistentSet.add acc regionKey.key.SubTerms
                let subterms =
                    RegionTree.foldr addKeySubTerms PersistentSet.empty x.memoryObject.updates
                    |> PersistentSet.toSeq
                    |> Seq.append x.key.SubTerms
                subterms
            override x.Time = x.time
            override x.TypeOfLocation = x.picker.sort.TypeOfLocation
            override x.IndependentWith otherSource =
                match otherSource with
                | :? heapReading<'key, 'reg> as otherReading ->
                    let rootRegions hr = match hr.memoryObject.updates with | Node dict -> PersistentDict.keys dict
                    Seq.allPairs (rootRegions x) (rootRegions otherReading) |> Seq.forall (fun (reg1, reg2) -> reg1.CompareTo reg2 = Disjoint)
                | _ -> true

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
            override x.IndependentWith otherSource =
                match otherSource with
                | :? structField as otherField ->
                    x.field <> otherField.field || x.baseSource.IndependentWith otherField.baseSource
                | _ -> true

    let (|StructFieldSource|_|) (src : IMemoryAccessConstantSource) =
        match src with
        | :? structField as sf -> Some(sf.baseSource, sf.field)
        | _ -> None

    [<StructuralEquality;NoComparison>]
    type private heapAddressSource =
        {baseSource : IMemoryAccessConstantSource}
        interface IMemoryAccessConstantSource  with
            override x.SubTerms = x.baseSource.SubTerms
            override x.Time = x.baseSource.Time
            override x.TypeOfLocation = x.baseSource.TypeOfLocation
            override x.IndependentWith otherSource =
                match otherSource with
                | :? heapAddressSource as otherAddress -> x.baseSource.IndependentWith otherAddress.baseSource
                | _ -> true

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
            override x.IndependentWith otherSource =
                match otherSource with
                | :? typeInitialized as otherType ->
                    let xDotNetType = toDotNetType x.typ
                    let otherDotNetType = toDotNetType otherType.typ
                    structuralInfimum xDotNetType otherDotNetType = None
                | _ -> true

    let (|TypeInitializedSource|_|) (src : IStatedSymbolicConstantSource) =
        match src with
        | :? typeInitialized as ti -> Some(ti.typ, ti.matchingTypes)
        | _ -> None

    let rec makeSymbolicValue (source : IMemoryAccessConstantSource) name typ =
        match typ with
        | Bool
        | AddressType
        | Numeric _ -> Constant name source typ
        | StructType _ ->
            let makeField _ field typ =
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

// =============== Marshalling/unmarshalling without state changing ===============

    // ------------------ Object to term ------------------

    let private referenceTypeToTerm state (obj : obj) =
        let address = PhysToVirt.find state obj |> ConcreteHeapAddress
        let objType = typeOfHeapLocation state address
        HeapRef address objType

    let rec objToTerm (state : state) (t : Type) (obj : obj) =
        match obj with
        | _ when TypeUtils.isNullable t -> nullableToTerm state t obj
        | null -> nullRef
        | :? bool as b -> makeBool b
        | _ when TypeUtils.isNumeric t -> makeNumber obj
        // TODO: need pointer?
        | _ when TypeUtils.isPointer t -> __notImplemented__()
        | _ when t.IsValueType -> structToTerm state obj t
        | _ -> referenceTypeToTerm state obj

    and private structToTerm state (obj : obj) t =
        let makeField (fieldInfo : Reflection.FieldInfo) _ _ =
           fieldInfo.GetValue(obj) |> objToTerm state fieldInfo.FieldType
        makeStruct false makeField (fromDotNetType t)

    and private nullableToTerm state t (obj : obj) =
        let nullableType = Nullable.GetUnderlyingType t
        let valueField, hasValueField = Reflection.fieldsOfNullable t
        let value, hasValue =
            if box obj <> null then objToTerm state nullableType obj, True
            else objToTerm state nullableType (Reflection.createObject nullableType), False
        let fields = PersistentDict.ofSeq <| seq[(valueField, value); (hasValueField, hasValue)]
        Struct fields (fromDotNetType t)

    // ---------------- Try term to object ----------------

    let tryAddressToObj (state : state) address =
        if address = VectorTime.zero then Some null
        else ConcreteMemory.tryFind state.concreteMemory address

    let rec tryTermToObj (state : state) term =
        match term.term with
        | Concrete(obj, _) -> Some obj
        | Struct(fields, typ) when isNullable typ -> tryNullableTermToObj state fields typ
        | Struct(fields, typ) -> tryStructTermToObj state fields typ
        | HeapRef({term = ConcreteHeapAddress a}, _) -> tryAddressToObj state a
        | _ -> None

    and tryStructTermToObj (state : state) fields typ =
        let structObj = toDotNetType typ |> Reflection.createObject
        let addField _ (fieldId, value) k =
            let fieldInfo = Reflection.getFieldInfo fieldId
            match tryTermToObj state value with
            // field can be converted to obj, so continue
            | Some v -> fieldInfo.SetValue(structObj, v) |> k
            // field can not be converted to obj, so break and return None
            | None -> None
        Cps.Seq.foldlk addField () (PersistentDict.toSeq fields) (fun _ -> Some structObj)

    and tryNullableTermToObj (state : state) fields typ =
        let valueField, hasValueField = Reflection.fieldsOfNullable (toDotNetType typ)
        let value = PersistentDict.find fields valueField
        let hasValue = PersistentDict.find fields hasValueField
        match tryTermToObj state value with
        | Some obj when hasValue = True -> Some obj
        | _ when hasValue = False -> Some null
        | _ -> None

// ------------------------------- Reading -------------------------------

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
        | ConcreteHeapAddress address -> VectorTime.less state.startingTime address
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

    let private readLowerBoundSymbolic (state : state) address dimension arrayType =
        let extractor (state : state) = accessRegion state.lowerBounds (substituteTypeVariablesIntoArrayType state arrayType) lengthType
        let mkname = fun (key : heapVectorIndexKey) -> sprintf "LowerBound(%O, %O)" key.address key.index
        let isDefault state (key : heapVectorIndexKey) = isHeapAddressDefault state key.address || thd3 arrayType
        let key = {address = address; index = dimension}
        MemoryRegion.read (extractor state) key (isDefault state)
            (makeSymbolicHeapRead {sort = ArrayLowerBoundSort arrayType; extract = extractor; mkname = mkname; isDefaultKey = isDefault} key state.startingTime)

    let readLowerBound state address dimension arrayType =
        let cm = state.concreteMemory
        match address.term, dimension.term with
        | ConcreteHeapAddress address, Concrete(:? int as dim, _) when ConcreteMemory.contains cm address ->
            ConcreteMemory.readArrayLowerBound cm address dim |> objToTerm state typeof<int>
        | _ -> readLowerBoundSymbolic state address dimension arrayType

    let private readLengthSymbolic state address dimension arrayType =
        let extractor (state : state) = accessRegion state.lengths (substituteTypeVariablesIntoArrayType state arrayType) lengthType
        let mkname = fun (key : heapVectorIndexKey) -> sprintf "Length(%O, %O)" key.address key.index
        let isDefault state (key : heapVectorIndexKey) = isHeapAddressDefault state key.address
        let key = {address = address; index = dimension}
        MemoryRegion.read (extractor state) key (isDefault state)
            (makeSymbolicHeapRead {sort = ArrayLengthSort arrayType; extract = extractor; mkname = mkname; isDefaultKey = isDefault} key state.startingTime)

    let readLength state address dimension arrayType =
        let cm = state.concreteMemory
        match address.term, dimension.term with
        | ConcreteHeapAddress address, Concrete(:? int as dim, _) when ConcreteMemory.contains cm address ->
            ConcreteMemory.readArrayLength cm address dim |> objToTerm state typeof<int>
        | _ -> readLengthSymbolic state address dimension arrayType

    let private readArrayRegion state arrayType extractor region address indices =
        let key = {address = address; indices = indices}
        let isDefault state (key : heapArrayIndexKey) = isHeapAddressDefault state key.address
        let instantiate typ memory =
            let mkname = fun (key : heapArrayIndexKey) -> sprintf "%O[%s]" key.address (List.map toString key.indices |> join ", ")
            let picker = {sort = ArrayIndexSort arrayType; extract = extractor; mkname = mkname; isDefaultKey = isDefault}
            let time =
                if isValueType typ then state.startingTime
                else MemoryRegion.maxTime region.updates state.startingTime
            makeSymbolicHeapRead picker key time typ memory
        MemoryRegion.read region key (isDefault state) instantiate

    let private readArrayIndexSymbolic state address indices arrayType =
        let extractor state = accessRegion state.arrays (substituteTypeVariablesIntoArrayType state arrayType) (fst3 arrayType)
        readArrayRegion state arrayType extractor (extractor state) address indices

    let private readSymbolicIndexFromConcreteArray state address arrayData indices arrayType =
        let writeOneIndex mr (index, value) =
            let key = {address = address; indices = List.map makeNumber index}
            objToTerm state (value.GetType()) value |> MemoryRegion.write mr key
        let region = arrayData |> Seq.fold writeOneIndex (MemoryRegion.empty (fst3 arrayType))
        readArrayRegion state arrayType (always region) region address indices

    let readArrayIndex state address indices arrayType =
        let cm = state.concreteMemory
        let concreteIndices = tryIntListFromTermList indices
        match address.term, concreteIndices with
        | ConcreteHeapAddress address, Some concreteIndices when ConcreteMemory.contains cm address ->
            ConcreteMemory.readArrayIndex cm address concreteIndices |> objToTerm state (fst3 arrayType |> toDotNetType)
        | ConcreteHeapAddress concreteAddress, None when ConcreteMemory.contains cm concreteAddress ->
            let data = ConcreteMemory.getAllArrayData state.concreteMemory concreteAddress
            readSymbolicIndexFromConcreteArray state address data indices arrayType
        | _ -> readArrayIndexSymbolic state address indices arrayType

    let private readClassFieldSymbolic state address (field : fieldId) =
        if field = Reflection.stringFirstCharField then
            readArrayIndexSymbolic state address [makeNumber 0] (Char, 1, true)
        else
            let symbolicType = fromDotNetType field.typ
            let extractor state = accessRegion state.classFields (substituteTypeVariablesIntoField state field) (substituteTypeVariables state symbolicType)
            let region = extractor state
            let mkname = fun (key : heapAddressKey) -> sprintf "%O.%O" key.address field
            let isDefault state (key : heapAddressKey) = isHeapAddressDefault state key.address
            let key = {address = address}
            let instantiate typ memory =
                let picker = {sort = HeapFieldSort field; extract = extractor; mkname = mkname; isDefaultKey = isDefault}
                let time =
                    if isValueType typ then state.startingTime
                    else MemoryRegion.maxTime region.updates state.startingTime
                makeSymbolicHeapRead picker key time typ memory
            MemoryRegion.read region key (isDefault state) instantiate

    let readClassField (state : state) address (field : fieldId) =
        let cm = state.concreteMemory
        match address.term with
        | ConcreteHeapAddress address when ConcreteMemory.contains cm address ->
            ConcreteMemory.readClassField cm address field |> objToTerm state field.typ
        | _ -> readClassFieldSymbolic state address field

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
        let key : stackBufferIndexKey = {index = index}
        MemoryRegion.read (extractor state) key (isDefault state)
            (makeSymbolicHeapRead {sort = StackBufferSort stackKey; extract = extractor; mkname = mkname; isDefaultKey = isDefault} key state.startingTime)

    let readBoxedLocation state (address : concreteHeapAddress) =
        match PersistentDict.tryFind state.boxedLocations address with
        | Some value -> value
        | None -> internalfailf "Boxed location %O was not found in heap: this should not happen!" address

    let rec readDelegate state reference =
        match reference.term with
        | HeapRef(address, _) ->
            match address.term with
            | ConcreteHeapAddress address -> state.delegates.[address]
            | _ -> __insufficientInformation__ "Unknown delegate %O!" address
        | Union gvs -> gvs |> List.map (fun (g, v) -> (g, readDelegate state v)) |> Merging.merge
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

// ------------------------------- General reading -------------------------------

    // TODO: take type of heap address
    let rec read state reference =
        match reference.term with
        | Ref address -> readSafe state address
        | Ptr _ -> internalfailf "reading ptr %O from safe context" reference
        | Union gvs -> gvs |> List.map (fun (g, v) -> (g, read state v)) |> Merging.merge
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
            let filterUnsat (g, v) k =
                let pc = PC.add state.pc g
                if PC.isFalse pc then k None
                else Some (pc, v) |> k
            Cps.List.choosek filterUnsat gvs (fun pcs ->
            match pcs with
            | [] -> k []
            | (pc, v)::pcs ->
                let copyState (pc, v) k = f (copy state pc) v k
                Cps.List.mapk copyState pcs (fun results ->
                    state.pc <- pc
                    f state v (fun r ->
                    r::results |> mergeResults |> k)))
        | _ -> f state term (List.singleton >> k)
    let guardedStatedApplyk f state term k = commonGuardedStatedApplyk f state term mergeResults k
    let guardedStatedApply f state term = guardedStatedApplyk (Cps.ret2 f) state term id

    let guardedStatedMap mapper state term =
        commonGuardedStatedApplyk (fun state term k -> mapper state term |> k) state term id id

    let commonStatedConditionalExecutionk (state : state) conditionInvocation thenBranch elseBranch merge2Results k =
        let keepIndependentWith pc cond =
            let fragments = PC.fragments pc
            fragments
            |> Seq.tryFind (PC.toSeq >> Seq.contains cond)
            |> function
                | Some(fragment) -> fragment
                | None -> pc                
        let execution thenState elseState condition k =
            assert (condition <> True && condition <> False)
            thenBranch thenState (fun thenResult ->
            elseBranch elseState (fun elseResult ->
            merge2Results thenResult elseResult |> k))
        conditionInvocation state (fun (condition, conditionState) ->            
        let negatedCondition = !!condition
        let thenPc = PC.add state.pc condition
        let elsePc = PC.add state.pc negatedCondition
        let independentThenPc = keepIndependentWith thenPc condition
        // Unnecessary
        let independentElsePc = keepIndependentWith elsePc negatedCondition 
        if PC.isFalse independentThenPc then
            conditionState.pc <- elsePc
            elseBranch conditionState (List.singleton >> k)
        elif PC.isFalse independentElsePc then
            conditionState.pc <- thenPc
            thenBranch conditionState (List.singleton >> k)
        else
            conditionState.pc <- independentThenPc
            match SolverInteraction.checkSat conditionState with
            | SolverInteraction.SmtUnknown _ ->
                conditionState.pc <- independentElsePc
                match SolverInteraction.checkSat conditionState with
                | SolverInteraction.SmtUnsat _
                | SolverInteraction.SmtUnknown _ ->
                    __insufficientInformation__ "Unable to witness branch"
                | SolverInteraction.SmtSat model ->
                    conditionState.pc <- thenPc
                    conditionState.model <- Some model.mdl
                    elseBranch conditionState (List.singleton >> k)
            | SolverInteraction.SmtUnsat _ ->
                conditionState.pc <- elsePc
                elseBranch conditionState (List.singleton >> k)
            | SolverInteraction.SmtSat model ->
                conditionState.pc <- independentElsePc
                conditionState.model <- Some model.mdl
                match SolverInteraction.checkSat conditionState with
                | SolverInteraction.SmtUnsat _
                | SolverInteraction.SmtUnknown _ ->
                    conditionState.pc <- thenPc
                    thenBranch conditionState (List.singleton >> k)
                | SolverInteraction.SmtSat model2 ->
                    conditionState.pc <- elsePc
                    let thenState = conditionState
                    let elseState = copy conditionState elsePc
                    elseState.model <- Some model2.mdl
                    thenState.pc <- thenPc
                    execution thenState elseState condition k)

    let statedConditionalExecutionWithMergek state conditionInvocation thenBranch elseBranch k =
        commonStatedConditionalExecutionk state conditionInvocation thenBranch elseBranch merge2Results k
    let statedConditionalExecutionWithMerge state conditionInvocation thenBranch elseBranch =
        statedConditionalExecutionWithMergek state conditionInvocation thenBranch elseBranch id

    // ------------------------------- Unsafe reading -------------------------------

    let private readAddressUnsafe address startByte endByte =
        let size = Terms.sizeOf address
        match startByte.term, endByte with
        | Concrete(:? int as s, _), Some({term = Concrete(:? int as e, _)}) when s = 0 && size = e -> List.singleton address
        // CASE: reading ref (ldind.ref, ldelem.ref ..) by pointer from concolic
        | Concrete(:? int as s, _), None when s = 0 -> List.singleton address
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
        | Expression _ -> Slice term startByte (Option.get endByte) startByte |> List.singleton // TODO: fix style #style
        | _ -> __unreachable__()

    and private readStructUnsafe fields structType startByte endByte =
        let readField fieldId = fields.[fieldId]
        readFieldsUnsafe readField false structType startByte endByte

    and private getAffectedFields readField isStatic (blockType : symbolicType) startByte endByte =
        let t = toDotNetType blockType
        let blockSize = TypeUtils.internalSizeOf t |> int
        let fields = Reflection.fieldsOf isStatic t
        let getOffsetAndSize (fieldId, fieldInfo : Reflection.FieldInfo) =
            fieldId, CSharpUtils.LayoutUtils.GetFieldOffset fieldInfo, TypeUtils.internalSizeOf fieldInfo.FieldType |> int
        let fieldIntervals = Array.map getOffsetAndSize fields |> Array.sortBy snd3
        let betweenField = {name = ""; declaringType = t; typ = typeof<byte>}
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
            if fieldId.name = "" then makeDefaultValue (Numeric fieldId.typ)
            else readField fieldId
        let getField (fieldId, fieldOffset, _) =
            let fieldValue = readFieldOrZero fieldId
            let fieldOffset = makeNumber fieldOffset
            let startByte = sub startByte fieldOffset
            let endByte = Option.map (fun endByte -> sub endByte fieldOffset) endByte
            fieldId, fieldValue, startByte, endByte
        match startByte.term, endByte with
        | Concrete(:? int as o, _), Some({term = Concrete(:? int as s, _)}) ->
            let concreteGetField (_, fieldOffset, fieldSize as field) affectedFields =
                if (o + s > fieldOffset && o < fieldOffset + fieldSize) then
                    getField field :: affectedFields
                else affectedFields
            List.foldBack concreteGetField allFields List.empty
        // TODO: fix style #style
        | Concrete(:? int as o, _), None ->
            let concreteGetField (_, fieldOffset, fieldSize as field) affectedFields =
                if (o >= fieldOffset && o < fieldOffset + fieldSize) then
                    getField field :: affectedFields
                else affectedFields
            List.foldBack concreteGetField allFields List.empty
        | _ -> List.map getField allFields

    and private readFieldsUnsafe readField isStatic (blockType : symbolicType) startByte endByte =
        let affectedFields = getAffectedFields readField isStatic blockType startByte endByte
        List.collect (fun (_, v, s, e) -> readTermUnsafe v s e) affectedFields

    let private checkUnsafeRead state reportError failCondition =
        commonStatedConditionalExecutionk state
            (fun state k -> k (!!failCondition, state))
            (fun _ k -> k ())
            (fun state k -> k (reportError state))
            (fun _ _ -> [])
            ignore

    let private checkClassUnsafeRead state reportError classType offset viewSize =
        let t = toDotNetType classType
        let classSize = TypeUtils.internalSizeOf t |> int |> makeNumber
        let failCondition = simplifyGreater (add offset viewSize) classSize id ||| simplifyLess offset (makeNumber 0) id
        // NOTE: disables overflow in solver
        state.pc <- PC.add state.pc (makeExpressionNoOvf failCondition id) // TODO: think more about overflow
        checkUnsafeRead state reportError failCondition

    // TODO: Add undefined behaviour:
    // TODO: 1. when reading info between fields
    // TODO: 3. when reading info outside block
    // TODO: 3. reinterpreting ref or ptr should return symbolic ref or ptr
    let private readClassUnsafe state reportError address classType offset sightType =
        checkClassUnsafeRead state reportError classType offset (Option.get sightType |> sizeOf |> makeNumber)
        let endByte = Option.map (sizeOf >> makeNumber >> add offset) sightType
        let readField fieldId = readClassField state address fieldId
        readFieldsUnsafe readField false classType offset endByte

    let private getAffectedIndices state reportError address (elementType, dim, _ as arrayType) offset size =
        let concreteElementSize = sizeOf elementType
        let elementSize = makeNumber concreteElementSize
        let lens = List.init dim (fun dim -> readLength state address (makeNumber dim) arrayType)
        let lbs = List.init dim (fun dim -> readLowerBound state address (makeNumber dim) arrayType)
        let arraySize = List.fold mul elementSize lens
        let failCondition = simplifyGreater (Option.get size |> makeNumber |> add offset) arraySize id ||| simplifyLess offset (makeNumber 0) id
        // NOTE: disables overflow in solver
        state.pc <- PC.add state.pc (makeExpressionNoOvf failCondition id)
        checkUnsafeRead state reportError failCondition
        let firstElement = div offset elementSize
        let elementOffset = rem offset elementSize
        let countToRead =
            // TODO: fix style #style
            match elementOffset.term, size with
            | Concrete(:? int as i, _), Some size when i = 0 -> (size / concreteElementSize)
            | Concrete(:? int, _), None -> 1
            // NOTE: if offset inside element > 0 then one more element is needed
            | _, Some size -> (size / concreteElementSize) + 1
            | _ -> internalfail "reading array using pointer Void*"
        let getElement currentOffset i =
            let linearIndex = makeNumber i |> add firstElement
            let indices = delinearizeArrayIndex linearIndex lens lbs
            let element = readArrayIndex state address indices arrayType
            let startByte = sub offset currentOffset
            let endByte = Option.map (makeNumber >> add startByte) size
            (indices, element, startByte, endByte), add currentOffset elementSize
        List.mapFold getElement (mul firstElement elementSize) [0 .. countToRead - 1] |> fst

    let private readArrayUnsafe state reportError address arrayType offset sightType =
        let size = Option.map sizeOf sightType
        let offset = sub offset (makeNumber CSharpUtils.LayoutUtils.ArrayElementsOffset)
        let indices = getAffectedIndices state reportError address (symbolicTypeToArrayType arrayType) offset size
        List.collect (fun (_, elem, s, e) -> readTermUnsafe elem s e) indices

    let private readStringUnsafe state reportError address offset sightType =
        let size = Option.map sizeOf sightType
         // TODO: handle case, when reading string length
        let offset = sub offset (makeNumber CSharpUtils.LayoutUtils.StringElementsOffset)
        let indices = getAffectedIndices state reportError address (Char, 1, true) offset size
        List.collect (fun (_, elem, s, e) -> readTermUnsafe elem s e) indices

    let private readStaticUnsafe state reportError t offset sightType =
        checkClassUnsafeRead state reportError t offset (Option.get sightType |> sizeOf |> makeNumber)
        let endByte = Option.map (sizeOf >> makeNumber >> add offset) sightType
        let readField fieldId = readStaticField state t fieldId
        readFieldsUnsafe readField true t offset endByte

    let private readUnsafe state reportError baseAddress offset sightType =
        // TODO: fix style #style
        let nonVoidType, combine = // TODO: this can be in case of reading ref or ptr, so size will be IntPtr.Size #do
            match sightType with
            | Void when isConcrete offset -> None, fun slices -> assert(List.length slices = 1); List.head slices
            | Void -> internalfail "reading pointer Void* with symbolic offset"
            | _ -> Some sightType, fun slices -> combine slices sightType
        let slices =
            match baseAddress with
            | HeapLocation loc ->
                let typ = typeOfHeapLocation state loc
                match typ with
                | StringType -> readStringUnsafe state reportError loc offset nonVoidType
                | ClassType _ -> readClassUnsafe state reportError loc typ offset nonVoidType
                | ArrayType _ -> readArrayUnsafe state reportError loc typ offset nonVoidType
                | StructType _ -> __notImplemented__() // TODO: boxed location?
                | _ -> internalfailf "expected complex type, but got %O" typ
            | StackLocation loc ->
                // TODO: check bounds here #do
                let term = readStackLocation state loc
                let locSize = term |> typeOf |> sizeOf |> int |> makeNumber
                let viewSize = nonVoidType |> Option.get |> sizeOf |> int |> makeNumber
                let failCondition = simplifyGreater (add offset viewSize) locSize id &&& simplifyLess offset (makeNumber 0) id
                state.pc <- PC.add state.pc (makeExpressionNoOvf failCondition id) // TODO: think more about overflow
                checkUnsafeRead state reportError failCondition
                let endByte = Option.map (sizeOf >> makeNumber >> add offset) nonVoidType
                readTermUnsafe term offset endByte
            | StaticLocation loc ->
                readStaticUnsafe state reportError loc offset nonVoidType
        combine slices

    let rec readIndirection state reportError reference =
        match reference.term with
        | Ref address -> readSafe state address
        | Ptr(baseAddress, sightType, offset) -> readUnsafe state reportError baseAddress offset sightType
        | Union gvs -> gvs |> List.map (fun (g, v) -> (g, read state v)) |> Merging.merge
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
        let symbolicType = fromDotNetType field.typ
        ensureConcreteType symbolicType
        let mr = accessRegion state.classFields field symbolicType
        let key = {address = address}
        let mr' = MemoryRegion.write mr key value
        state.classFields <- PersistentDict.add field mr' state.classFields

    let private writeArrayIndexSymbolic state address indices arrayType value =
        let elementType = fst3 arrayType
        ensureConcreteType elementType
        let mr = accessRegion state.arrays arrayType elementType
        let key = {address = address; indices = indices}
        let mr' = MemoryRegion.write mr key value
        state.arrays <- PersistentDict.add arrayType mr' state.arrays

    let writeStaticField state typ (field : fieldId) value =
        let symbolicType = fromDotNetType field.typ
        ensureConcreteType symbolicType
        let mr = accessRegion state.staticFields field symbolicType
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
        let mr = accessRegion state.stackBuffers stackKey (Numeric typeof<int8>)
        let key : stackBufferIndexKey = {index = index}
        let mr' = MemoryRegion.write mr key value
        state.stackBuffers <- PersistentDict.add stackKey mr' state.stackBuffers

    let writeBoxedLocation state (address : concreteHeapAddress) value =
        state.boxedLocations <- PersistentDict.add address value state.boxedLocations
        state.allocatedTypes <- PersistentDict.add address (typeOf value) state.allocatedTypes

// ----------------- Unmarshalling: from concrete to symbolic memory -----------------

    let private unmarshallClass (state : state) address obj =
        let writeField state (fieldId, fieldInfo : Reflection.FieldInfo) =
            let value = fieldInfo.GetValue obj |> objToTerm state fieldInfo.FieldType
            writeClassFieldSymbolic state address fieldId value
        let fields = obj.GetType() |> Reflection.fieldsOf false
        Array.iter (writeField state) fields

    let private unmarshallArray (state : state) address (array : Array) =
        let elemType, dim, _ as arrayType = array.GetType() |> fromDotNetType |> symbolicTypeToArrayType
        let lbs = List.init dim array.GetLowerBound
        let lens = List.init dim array.GetLength
        let writeIndex state (indices : int list) =
            let value = array.GetValue(Array.ofList indices) |> objToTerm state (toDotNetType elemType)
            let termIndices = List.map makeNumber indices
            writeArrayIndexSymbolic state address termIndices arrayType value
        let allIndices = List.map2 (fun lb len -> [lb .. lb + len - 1]) lbs lens |> List.cartesian
        Seq.iter (writeIndex state) allIndices
        let termLBs = List.map (objToTerm state typeof<int>) lbs
        let termLens = List.map (objToTerm state typeof<int>) lens
        fillArrayBoundsSymbolic state address termLens termLBs arrayType

    let private unmarshallString (state : state) address (string : string) =
        let concreteStringLength = string.Length
        let stringLength = makeNumber concreteStringLength
        let arrayLength = makeNumber (concreteStringLength + 1)
        let arrayType = (Char, 1, true)
        let zero = makeNumber 0
        let writeChars state i value =
            writeArrayIndexSymbolic state address [Concrete i lengthType] arrayType (Concrete value Char)
        Seq.iteri (writeChars state) string
        writeLengthSymbolic state address zero arrayType arrayLength
        writeLowerBoundSymbolic state address zero arrayType zero
        writeArrayIndexSymbolic state address [stringLength] (Char, 1, true) (Concrete '\000' Char)
        writeClassFieldSymbolic state address Reflection.stringLengthField stringLength

    let unmarshall (state : state) concreteAddress =
        let address = ConcreteHeapAddress concreteAddress
        let obj = ConcreteMemory.readObject state.concreteMemory concreteAddress
        assert(box obj <> null)
        match obj with
        | :? Array as array -> unmarshallArray state address array
        | :? String as string -> unmarshallString state address string
        | _ -> unmarshallClass state address obj
        ConcreteMemory.remove state concreteAddress

// ------------------------------- Writing -------------------------------

    let writeClassField state address (field : fieldId) value =
        let cm = state.concreteMemory
        let concreteValue = tryTermToObj state value
        match address.term, concreteValue with
        | ConcreteHeapAddress concreteAddress, Some obj when ConcreteMemory.contains cm concreteAddress ->
            ConcreteMemory.writeClassField state concreteAddress field obj
        | ConcreteHeapAddress concreteAddress, None when ConcreteMemory.contains cm concreteAddress ->
            unmarshall state concreteAddress
            writeClassFieldSymbolic state address field value
        | _ -> writeClassFieldSymbolic state address field value

    let writeArrayIndex state address indices arrayType value =
        let cm = state.concreteMemory
        let concreteValue = tryTermToObj state value
        let concreteIndices = tryIntListFromTermList indices
        match address.term, concreteValue, concreteIndices with
        | ConcreteHeapAddress a, Some obj, Some concreteIndices when ConcreteMemory.contains cm a ->
            ConcreteMemory.writeArrayIndex state a concreteIndices obj
        | ConcreteHeapAddress a, _, None
        | ConcreteHeapAddress a, None, _ when ConcreteMemory.contains cm a ->
            unmarshall state a
            writeArrayIndexSymbolic state address indices arrayType value
        | _ -> writeArrayIndexSymbolic state address indices arrayType value

// ------------------------------- Unsafe writing -------------------------------

    let private writeAddressUnsafe address startByte value =
        let addressSize = Terms.sizeOf address
        let valueSize = Terms.sizeOf value
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
            let termSize = sizeOf termType
            let valueSize = Terms.sizeOf value
            match startByte.term with
            | Concrete(:? int as startByte, _) when startByte = 0 && valueSize = termSize -> value
            | _ ->
                let zero = makeNumber 0
                let termSize = sizeOf termType |> makeNumber
                let valueSize = Terms.sizeOf value |> makeNumber
                let left = Slice term zero startByte zero
                let valueSlices = readTermUnsafe value (neg startByte) (sub termSize startByte |> Some)
                let right = Slice term (add startByte valueSize) termSize zero
                combine ([left] @ valueSlices @ [right]) termType
        | _ -> __unreachable__()

    and private writeStructUnsafe structTerm fields structType startByte value =
        let readField fieldId = fields.[fieldId]
        let updatedFields = writeFieldsUnsafe readField false structType startByte value
        let writeField structTerm (fieldId, value) = writeStruct structTerm fieldId value
        List.fold writeField structTerm updatedFields

    and private writeFieldsUnsafe readField isStatic (blockType : symbolicType) startByte value =
        let endByte = Terms.sizeOf value |> makeNumber |> add startByte
        let affectedFields = getAffectedFields readField isStatic blockType startByte (Some endByte)
        List.map (fun (id, v, s, _) -> id, writeTermUnsafe v s value) affectedFields

    let writeClassUnsafe state reportError address typ offset value =
        checkClassUnsafeRead state reportError typ offset (value |> Terms.sizeOf |> makeNumber)
        let readField fieldId = readClassField state address fieldId
        let updatedFields = writeFieldsUnsafe readField false typ offset value
        let writeField (fieldId, value) = writeClassField state address fieldId value
        List.iter writeField updatedFields

    let writeArrayUnsafe state reportError address arrayType offset value =
        let size = Terms.sizeOf value
        let arrayType = symbolicTypeToArrayType arrayType
        let offset = sub offset (makeNumber CSharpUtils.LayoutUtils.ArrayElementsOffset)
        let affectedIndices = getAffectedIndices state reportError address arrayType offset (Some size)
        let writeElement (index, element, startByte, _) =
            let updatedElement = writeTermUnsafe element startByte value
            writeArrayIndex state address index arrayType updatedElement
        List.iter writeElement affectedIndices

    let private writeStringUnsafe state reportError address offset value =
        let size = Terms.sizeOf value
        let arrayType = Char, 1, true
        let offset = sub offset (makeNumber CSharpUtils.LayoutUtils.StringElementsOffset)
        let affectedIndices = getAffectedIndices state reportError address arrayType offset (Some size)
        let writeElement (index, element, startByte, _) =
            let updatedElement = writeTermUnsafe element startByte value
            writeArrayIndex state address index arrayType updatedElement
        List.iter writeElement affectedIndices

    let writeStaticUnsafe state reportError staticType offset value =
        checkClassUnsafeRead state reportError staticType offset (value |> Terms.sizeOf |> makeNumber)
        let readField fieldId = readStaticField state staticType fieldId
        let updatedFields = writeFieldsUnsafe readField true staticType offset value
        let writeField (fieldId, value) = writeStaticField state staticType fieldId value
        List.iter writeField updatedFields

    let private writeUnsafe state reportError baseAddress offset sightType value =
        assert(sightType = symbolicType.Void && isConcrete offset || sizeOf sightType = Terms.sizeOf value)
        match baseAddress with
        | HeapLocation loc ->
            let typ = typeOfHeapLocation state loc
            match typ with
            | StringType -> writeStringUnsafe state reportError loc offset value
            | ClassType _ -> writeClassUnsafe state reportError loc typ offset value
            | ArrayType _ -> writeArrayUnsafe state reportError loc typ offset value
            | StructType _ -> __notImplemented__() // TODO: boxed location?
            | _ -> internalfailf "expected complex type, but got %O" typ
        | StackLocation loc ->
            let term = readStackLocation state loc
            let locSize = term |> typeOf |> sizeOf |> int |> makeNumber
            let viewSize = value |> Terms.sizeOf |> int |> makeNumber
            let failCondition = simplifyGreater (add offset viewSize) locSize id &&& simplifyLess offset (makeNumber 0) id
            state.pc <- PC.add state.pc (makeExpressionNoOvf failCondition id) // TODO: think more about overflow
            checkUnsafeRead state reportError failCondition
            let updatedTerm = writeTermUnsafe term offset value
            writeStackLocation state loc updatedTerm
        | StaticLocation loc ->
            writeStaticUnsafe state reportError loc offset value

// ------------------------------- General writing -------------------------------

    // NOTE: using unsafe write instead of safe, when field intersects,
    // because need to write to all fields, which intersects with 'field'
    let private writeIntersectingField state address (field : fieldId) value =
        let typ = fromDotNetType field.typ
        let baseAddress, offset = Pointers.addressToBaseAndOffset address
        let ptr = Ptr baseAddress typ offset
        match ptr.term with
        | Ptr(baseAddress, sightType, offset) -> writeUnsafe state (fun _ -> ()) baseAddress offset sightType value
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

    let write state reference value =
        guardedStatedMap
            (fun state reference ->
                match reference.term with
                | Ref address -> writeSafe state address value
                | Ptr _ -> internalfailf "writing by ptr %O from safe context" reference
                | _ -> internalfailf "Writing: expected reference, but got %O" reference
                state)
            state reference

    let writeIndirection state reportError reference value =
        guardedStatedMap
            (fun state reference ->
                match reference.term with
                | Ref address -> writeSafe state address value
                | Ptr(address, sightType, offset) -> writeUnsafe state reportError address offset sightType value
                | _ -> internalfailf "Writing: expected reference, but got %O" reference
                state)
            state reference

// ------------------------------- Allocation -------------------------------

    let freshAddress state =
        state.currentTime <- VectorTime.advance state.currentTime
        state.currentTime

    let allocateType state typ =
        let concreteAddress = freshAddress state
        assert(not <| PersistentDict.contains concreteAddress state.allocatedTypes)
        state.allocatedTypes <- PersistentDict.add concreteAddress typ state.allocatedTypes
        concreteAddress

    let allocateOnStack state key term =
        state.stack <- CallStack.allocate state.stack key term

    // Strings and delegates should be allocated using the corresponding functions (see allocateString and allocateDelegate)!
    let allocateClass state typ =
        let dotNetType = toDotNetType typ
        assert (not <| TypeUtils.isSubtypeOrEqual dotNetType typeof<String>)
        assert (not <| TypeUtils.isSubtypeOrEqual dotNetType typeof<Delegate>)
        let concreteAddress = allocateType state typ
        match memoryMode with
        // TODO: it's hack for reflection, remove it after concolic will be implemented
        | _ when TypeUtils.isSubtypeOrEqual dotNetType typeof<Type> -> ()
        | ConcreteMemory -> Reflection.createObject dotNetType |> ConcreteMemory.allocate state concreteAddress
        | SymbolicMemory -> ()
        HeapRef (ConcreteHeapAddress concreteAddress) typ

    // TODO: unify allocation with unmarshalling
    let allocateArray state typ lowerBounds lengths =
        let dotNetType = toDotNetType typ
        assert (TypeUtils.isSubtypeOrEqual dotNetType typeof<Array>)
        let concreteAddress = allocateType state typ
        let arrayType = symbolicTypeToArrayType typ
        let address = ConcreteHeapAddress concreteAddress
        let concreteLengths = tryIntListFromTermList lengths
        let concreteLowerBounds = tryIntListFromTermList lowerBounds
        match memoryMode, concreteLengths, concreteLowerBounds with
        | ConcreteMemory, Some concreteLengths, Some concreteLBs ->
            let elementDotNetType = elementType typ |> toDotNetType
            let array = Array.CreateInstance(elementDotNetType, Array.ofList concreteLengths, Array.ofList concreteLBs) :> obj
            ConcreteMemory.allocate state concreteAddress array
        | _ -> fillArrayBoundsSymbolic state address lengths lowerBounds arrayType
        address

    let allocateVector state elementType length =
        let typ = ArrayType(elementType, Vector)
        allocateArray state typ [makeNumber 0] [length]

    let allocateConcreteVector state elementType length contents =
        match memoryMode, length.term with
        | ConcreteMemory, Concrete(:? int as intLength, _) ->
            let concreteAddress = allocateType state (ArrayType(elementType, Vector))
            let array = Array.CreateInstance(toDotNetType elementType, intLength)
            Seq.iteri (fun i value -> array.SetValue(value, i)) contents
            ConcreteMemory.allocate state concreteAddress (array :> obj)
            ConcreteHeapAddress concreteAddress
        | _ ->
            let address = allocateVector state elementType length
            // TODO: optimize this for large concrete arrays (like images)!
            let writeIndex state i value =
                 writeArrayIndex state address [Concrete i lengthType] (elementType, 1, true) (Concrete value elementType)
            Seq.iteri (writeIndex state) contents
            address

    // TODO: unify allocation with unmarshalling
    let private commonAllocateString state length contents =
        match memoryMode, length.term with
        | ConcreteMemory, Concrete(:? int as intLength, _) ->
            let charArray : char array = Array.zeroCreate intLength
            Seq.iteri (fun i char -> charArray.SetValue(char, i)) contents
            let string = new string(charArray) :> obj
            let concreteAddress = allocateType state String
            ConcreteMemory.allocate state concreteAddress string
            ConcreteHeapAddress concreteAddress
        | _ ->
            let arrayLength = add length (Concrete 1 lengthType)
            let address = allocateConcreteVector state Char arrayLength contents
            writeArrayIndexSymbolic state address [length] (Char, 1, true) (Concrete '\000' Char)
            let heapAddress = getConcreteHeapAddress address
            writeClassField state address Reflection.stringLengthField length
            state.allocatedTypes <- PersistentDict.add heapAddress String state.allocatedTypes
            address

    let allocateEmptyString state length =
        let address = commonAllocateString state length Seq.empty
        HeapRef address String
    let allocateString state (str : string) =
        let address = commonAllocateString state (Concrete str.Length lengthType) str
        HeapRef address String

    let createStringFromChar state char =
        match char.term with
        | Concrete(:? char as c, _) ->
            let string = c.ToString()
            allocateString state string
        | _ ->
            let address = commonAllocateString state (makeNumber 1) " "
            writeArrayIndexSymbolic state address [Concrete 0 indexType] (Char, 1, true) char
            HeapRef address String

    let allocateDelegate state delegateTerm =
        let concreteAddress = freshAddress state
        let address = ConcreteHeapAddress concreteAddress
        state.delegates <- PersistentDict.add concreteAddress delegateTerm state.delegates
        state.allocatedTypes <- PersistentDict.add concreteAddress (typeOf delegateTerm) state.allocatedTypes
        HeapRef address (typeOf delegateTerm)

    let rec lengthOfString state heapRef =
        match heapRef.term with
        | HeapRef(address, typ) ->
            assert(typ = String)
            readClassField state address Reflection.stringLengthField
        | Union gvs -> gvs |> List.map (fun (g, v) -> (g, lengthOfString state v)) |> Merging.merge
        | _ -> internalfail "Getting length of string: expected heap reference, but got %O" heapRef

    let initializeStaticMembers state typ =
        if typ = String then
            let reference = allocateString state ""
            writeStaticField state String Reflection.emptyStringField reference
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
        else state.currentTime @ time

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
                let read region = MemoryRegion.read region key (x.picker.isDefaultKey state) (makeSymbolicHeapRead x.picker key state.startingTime)
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

    let private composeConcreteMemory mapKey (cm : concreteMemory) (cm' : concreteMemory) =
        let write (kvp : KeyValuePair<_,_>) =
            let k' = mapKey kvp.Key
            if cm.ContainsKey k' then
                cm.[k'] <- kvp.Value
            else cm.Add(k', kvp.Value)
        Seq.iter write cm'

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
            let physToVirt = composeConcreteDictionaries id (composeTime state) state.physToVirt state'.physToVirt
            let allocatedTypes = composeConcreteDictionaries (composeTime state) (substituteTypeVariables state) state.allocatedTypes state'.allocatedTypes
            let typeVariables = composeTypeVariablesOf state state'
            let delegates = composeConcreteDictionaries (composeTime state) id state.delegates state'.delegates
            let currentTime = composeTime state state'.currentTime
            let g = g1 &&& g2 &&& g3 &&& g4 &&& g5 &&& g6
            if not <| isFalse g then
                return {
                    pc = if isTrue g then pc else PC.add pc g
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
                    physToVirt = physToVirt
                    allocatedTypes = allocatedTypes
                    typeVariables = typeVariables
                    delegates = delegates
                    currentTime = currentTime
                    startingTime = state.startingTime
                    model = state.model // TODO: compose models?
                }
        }

    type typeInitialized with
        interface IStatedSymbolicConstantSource with
            override x.Compose state =
                let typ = substituteTypeVariables state x.typ
                let newTypes = composeInitializedTypes state x.matchingTypes
                isTypeInitialized {state with initializedTypes = newTypes} typ

    type hashCodeSource with
        interface IStatedSymbolicConstantSource with
            override x.Compose state =
                let object' = fillHoles state x.object
                getHashCode object'

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
        let sb = dumpDict "Concrete memory" sortVectorTime VectorTime.print toString sb (s.concreteMemory |> Seq.map (fun kvp -> (kvp.Key, kvp.Value)) |> PersistentDict.ofSeq)
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
