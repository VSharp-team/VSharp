namespace VSharp.Core

open System
open System.Collections.Generic
open System.Runtime.CompilerServices
open VSharp
open VSharp.Utils

type private ChildKind =
    | Field of fieldId
    | Index of int[] * Type
    with
    member x.Type with get() =
        match x with
        | Field f -> f.typ
        | Index(_, t) -> t

type private ChildLocation =
    { childKind : ChildKind; structFields : fieldId list }
    with
    static member Create(childKind : ChildKind) =
        { childKind = childKind; structFields = List.Empty }

    member x.Type with get() =
        match x.structFields with
        | [] -> x.childKind.Type
        | field :: _ -> field.typ

    member x.AddStructField(fieldId : fieldId) =
        assert x.Type.IsValueType
        { x with structFields = fieldId :: x.structFields }

    member x.Includes(childKind : ChildKind) = x.childKind = childKind

    member x.Includes(childLoc : ChildLocation) =
        x.childKind = childLoc.childKind
        &&
        let structFields = x.structFields
        let otherStructFields = childLoc.structFields
        let length = List.length structFields
        let otherLength = List.length otherStructFields
        length = otherLength && otherStructFields = structFields
        || length > otherLength && otherStructFields = List.skip (length - otherLength) structFields

type private Cell =
    | Concrete of physicalAddress
    | Symbolic

type private childDictType = Dictionary<ChildLocation, Cell>
type private childrenType = Dictionary<physicalAddress, childDictType>
type private parentDictType = Dictionary<physicalAddress, ChildLocation>
type private parentsType = Dictionary<physicalAddress, parentDictType>

type public ConcreteMemory private (physToVirt, virtToPhys, children, parents, changedStaticFields, fullyConcretes) =

// ----------------------------- Constructor -----------------------------

    internal new () =
        let physToVirt = Dictionary<physicalAddress, concreteHeapAddress>()
        let virtToPhys = Dictionary<concreteHeapAddress, physicalAddress>()
        let fullyConcretes = HashSet<physicalAddress>()
        let children = childrenType()
        let parents = parentsType()
        let changedStaticFields = HashSet<fieldId>()
        ConcreteMemory(physToVirt, virtToPhys, children, parents, changedStaticFields, fullyConcretes)

// ----------------------------- Primitives -----------------------------

    member private x.HasFullyConcreteParent (phys : physicalAddress) =
        let tested = HashSet<physicalAddress>()
        let queue = Queue<physicalAddress>()
        queue.Enqueue phys
        let child = ref physicalAddress.Empty
        let mutable contains = false
        while not contains && queue.TryDequeue child do
            let child = child.Value
            if tested.Add child then
                contains <- fullyConcretes.Contains child
                let exists, parentDict = parents.TryGetValue child
                if exists then
                    for KeyValue(parent, _) in parentDict do
                        queue.Enqueue parent
        contains

    member private x.AddToParents(parents : parentsType, parent : physicalAddress, child : physicalAddress, childLoc : ChildLocation) =
        assert(parent.object <> null)
        assert(child.object <> null && not childLoc.Type.IsValueType)
        let exists, parentDict = parents.TryGetValue child
        if exists then
            parentDict[parent] <- childLoc
        else
            let parentDict = parentDictType()
            parentDict.Add(parent, childLoc)
            parents.Add(child, parentDict)

    member private x.AddToParents(parent : physicalAddress, child : physicalAddress, childLoc : ChildLocation) =
        x.AddToParents(parents, parent, child, childLoc)

    member private x.AddRefChild(parent : physicalAddress, child : physicalAddress, childLoc : ChildLocation, update : bool) =
        assert(not childLoc.Type.IsValueType)
        let exists, childDict = children.TryGetValue parent
        if exists && update then childDict[childLoc] <- Concrete child
        elif exists then
            let exists, cell = childDict.TryGetValue childLoc
            if exists then
                match cell with
                | Concrete oldChild ->
                    assert(oldChild = child)
                    childDict[childLoc] <- Concrete child
                | Symbolic -> ()
            else childDict.Add(childLoc, Concrete child)
        else
            let childDict = childDictType()
            childDict.Add(childLoc, Concrete child)
            children.Add(parent, childDict)
        if update && x.HasFullyConcreteParent parent && not (x.CheckConcreteness child) then
            x.RemoveFromFullyConcretesRec parent
        x.AddToParents(parent, child, childLoc)

    member private x.AddChild(parent : physicalAddress, child : physicalAddress, childLoc : ChildLocation, update : bool) =
        if parent <> child then
            assert(parent.object <> null)
            let childObj = child.object
            let childIsNull = childObj = null
            if childIsNull && update then
                let exists, childDict = children.TryGetValue parent
                if exists then
                    for KeyValue(childLoc, _) in childDict do
                        if childLoc.Includes childLoc then
                            childDict.Remove childLoc |> ignore
            elif not childIsNull then
                let t = childLoc.Type
                if t.IsValueType then
                    assert(childObj :? ValueType)
                    let fields = Reflection.fieldsOf false t
                    for fieldId, fieldInfo in fields do
                        if Reflection.isReferenceOrContainsReferences fieldId.typ then
                            let child = { object = fieldInfo.GetValue childObj }
                            let childLoc = childLoc.AddStructField fieldId
                            x.AddChild(parent, child, childLoc, update)
                else x.AddRefChild(parent, child, childLoc, update)

    member private x.TrackChild(parent : physicalAddress, child : physicalAddress, childKind : ChildKind) =
        x.AddChild(parent, child, ChildLocation.Create childKind, false)

    member private x.TrackChild(parent : obj, child : obj, childKind : ChildKind) =
        assert(not (parent :? physicalAddress) && not (child :? physicalAddress))
        let parent = { object = parent }
        let child = { object = child }
        x.TrackChild(parent, child, childKind)

    member private x.SetChild(parent : physicalAddress, child : physicalAddress, childKind : ChildKind) =
        x.AddChild(parent, child, ChildLocation.Create childKind, true)

    member private x.SetChild(parent : obj, child : obj, childKind : ChildKind) =
        assert(not (parent :? physicalAddress) && not (child :? physicalAddress))
        let parent = { object = parent }
        let child = { object = child }
        x.SetChild(parent, child, childKind)

    member private x.ReadObject address =
        assert(virtToPhys.ContainsKey address)
        virtToPhys[address].object

    member private x.WriteObject address obj =
        assert(virtToPhys.ContainsKey address)
        let physicalAddress = {object = obj}
        virtToPhys[address] <- physicalAddress

    member private x.CastIfNeed value containerType =
        if box value <> null then
            let valueType = value.GetType()
            if valueType <> containerType && TypeUtils.canConvert valueType containerType then
                // This is done mostly because of signed to unsigned conversions (Example: int16 -> char)
                TypeUtils.convert value containerType
            else value
        else value

// ------------------------------- Copying -------------------------------

    member internal x.Copy() =
        let physToVirt' = Dictionary<physicalAddress, concreteHeapAddress>()
        let virtToPhys' = Dictionary<concreteHeapAddress, physicalAddress>()
        let children' = childrenType()
        let parents' = parentsType()
        let changedStaticFields' = HashSet<fieldId>(changedStaticFields)
        let fullyConcretes' = HashSet<physicalAddress>()
        // Need to copy all addresses from physToVirt, because:
        // 1. let complex object (A) contains another object (B),
        // if object (B) was unmarshalled, physToVirt will contain mapping
        // between old object (B) and virtual address of it (addr);
        // symbolic memory will contain info of symbolic object (B) by it's address (addr)
        // So, reading from object (A) object (B) will result in HeapRef (addr),
        // which will be read from symbolic memory
        let copier = Copier()
        for KeyValue(phys, virt) in physToVirt do
            let phys' = copier.DeepCopy phys
            let exists, oldPhys = virtToPhys.TryGetValue(virt)
            if exists && oldPhys = phys then
                virtToPhys'.Add(virt, phys')
            // Empty string is interned
            if phys' = {object = String.Empty} then physToVirt'[phys'] <- virt
            else physToVirt'.Add(phys', virt)

        let copyingObjects = HashSet<physicalAddress>(copier.CopyingObjects)
        for KeyValue(parent, childDict) in children do
            if copyingObjects.Contains parent then
                let parent' = copier.DeepCopy parent
                let childDict' = childDictType()
                for KeyValue(childLoc, child) in childDict do
                    match child with
                    | Concrete child ->
                        if copyingObjects.Contains child then
                            let child' = copier.DeepCopy child
                            x.AddToParents(parents', parent', child', childLoc)
                            childDict'.Add(childLoc, Concrete child')
                    | Symbolic -> childDict'.Add(childLoc, child)
                children'.Add(parent', childDict')

        for phys in fullyConcretes do
            if copyingObjects.Contains phys then
                fullyConcretes'.Add(copier.DeepCopy phys) |> ignore

        ConcreteMemory(physToVirt', virtToPhys', children', parents', changedStaticFields', fullyConcretes')

// ----------------------------- Primitives -----------------------------

    member public x.Contains address =
        virtToPhys.ContainsKey address

    member private x.CheckConcreteness (phys : physicalAddress) =
        let tracked = HashSet<physicalAddress>()
        x.CheckConcretenessRec tracked phys

    member private x.CheckConcretenessRec (tracked : HashSet<physicalAddress>) (phys : physicalAddress) =
        // TODO: cache all symbolic objects
        if fullyConcretes.Contains phys || not (tracked.Add phys) then true
        else
            let mutable allConcrete = true
            let exists, childDict = children.TryGetValue phys
            if exists then
                for KeyValue(_, child) in childDict do
                    if allConcrete then
                        match child with
                        | Symbolic -> allConcrete <- false
                        | Concrete child -> allConcrete <- x.CheckConcretenessRec tracked child
            if allConcrete then fullyConcretes.Add phys |> ignore
            allConcrete

    member public x.TryFullyConcrete (address : concreteHeapAddress) =
        let exists, phys = virtToPhys.TryGetValue address
        if exists then
            if x.CheckConcreteness phys then
                Some phys.object
            else None
        else None

    member public x.VirtToPhys virtAddress =
        x.ReadObject virtAddress

    member public x.TryVirtToPhys virtAddress =
        let exists, result = virtToPhys.TryGetValue(virtAddress)
        if exists then Some result.object
        else None

    member internal x.PhysToVirt (physAddress : obj) =
        match x.TryPhysToVirt physAddress with
        | Some address -> address
        | None -> internalfail $"PhysToVirt: unable to get virtual address for object {physAddress}"

    member internal x.TryPhysToVirt (physAddress : obj) =
        let result = ref List.empty
        if physToVirt.TryGetValue({object = physAddress}, result) then
            Some result.Value
        else None

// ----------------------------- Allocation -----------------------------

    member internal x.Allocate address (obj : obj) =
        assert(obj <> null)
        assert(virtToPhys.ContainsKey address |> not)
        // Suppressing finalize, because 'obj' may implement 'Dispose()' method, which should not be invoked,
        // because object may be in incorrect state (statics, for example)
        GC.SuppressFinalize(obj)
        let physicalAddress = {object = obj}
        virtToPhys.Add(address, physicalAddress)
        physToVirt[physicalAddress] <- address

    member internal x.AllocateDelegate address (d : Delegate) =
        x.Allocate address d
        let targetField = Reflection.delegateTargetField.Value |> Reflection.wrapField
        x.TrackChild(d, d.Target, Field targetField)

    member internal x.AllocateBoxedLocation address (value : obj) (t : Type) =
        assert(value :? ValueType && t.IsValueType)
        x.Allocate address value
        let fields = Reflection.fieldsOf false t
        for fieldId, fieldInfo in fields do
            if Reflection.isReferenceOrContainsReferences fieldId.typ then
                let child = fieldInfo.GetValue value
                x.TrackChild(value, child, Field fieldId)

// ------------------------------- Reading -------------------------------

    member internal x.ReadClassField address (field : fieldId) =
        let object = x.ReadObject address
        let fieldInfo = Reflection.getFieldInfo field
        let fieldValue = fieldInfo.GetValue(object)
        if Reflection.isReferenceOrContainsReferences field.typ then
            // TODO: cache it or move to allocation
            x.TrackChild(object, fieldValue, Field field)
        fieldValue

    member internal x.ReadArrayIndex address (indices : int list) =
        match x.ReadObject address with
        | :? Array as array ->
            let elemType = array.GetType().GetElementType()
            let indices = Array.ofList indices
            let elem = array.GetValue(indices)
            if Reflection.isReferenceOrContainsReferences elemType then
                // TODO: cache it or move to allocation
                x.TrackChild(array, elem, Index(indices, elemType))
            elem
        | :? String as string when List.length indices = 1 ->
            let index = List.head indices
            // Case 'index = string.Length' is needed for unsafe string reading: string contents end with null terminator
            // In safe context this case will be filtered out in 'Interpreter', which checks indices before memory access
            if index = string.Length then Char.MinValue :> obj
            else string[index] :> obj
        | obj -> internalfail $"reading array index from concrete memory: expected to read array, but got {obj}"

    member internal x.GetAllArrayData address =
        match x.ReadObject address with
        | :? Array as array -> Array.getArrayIndicesWithValues array
        | :? String as string -> string.ToCharArray() |> Array.getArrayIndicesWithValues
        | obj -> internalfail $"reading array data concrete memory: expected to read array, but got {obj}"

    member internal x.ReadArrayLowerBound address dimension =
        match x.ReadObject address with
        | :? Array as array -> array.GetLowerBound(dimension)
        | :? String when dimension = 0 -> 0
        | obj -> internalfail $"reading array lower bound from concrete memory: expected to read array, but got {obj}"

    member internal x.ReadArrayLength address dimension =
        match x.ReadObject address with
        | :? Array as array -> array.GetLength(dimension)
        | :? String as string when dimension = 0 -> 1 + string.Length
        | obj -> internalfail $"reading array length from concrete memory: expected to read array, but got {obj}"

    member internal x.ReadBoxedLocation address =
        let obj = x.ReadObject address
        assert(obj :? ValueType)
        obj :?> ValueType

    member internal x.ReadDelegate address : Delegate =
        let obj = x.ReadObject address
        assert(obj :? Delegate)
        obj :?> Delegate

// ------------------------------- Writing -------------------------------

    member internal x.WriteClassField address (field : fieldId) (value : obj) =
        let object = x.ReadObject address
        let fieldInfo = Reflection.getFieldInfo field
        let fieldType = fieldInfo.FieldType
        let value = x.CastIfNeed value fieldType
        fieldInfo.SetValue(object, value)
        if Reflection.isReferenceOrContainsReferences fieldType then
            x.SetChild(object, value, Field field)

    member internal x.WriteArrayIndex address (indices : int list) value =
        match x.ReadObject address with
        | :? Array as array ->
            let elemType = array.GetType().GetElementType()
            let castedValue =
                if value <> null then x.CastIfNeed value elemType
                else value
            let indices = Array.ofList indices
            array.SetValue(castedValue, indices)
            if Reflection.isReferenceOrContainsReferences elemType then
                x.SetChild(array, value, Index(indices, elemType))
        // TODO: strings must be immutable! This is used by copying, so copy string another way #hack
        | :? String as string when List.length indices = 1 ->
            let charArray = string.ToCharArray()
            assert(value <> null)
            let castedValue = x.CastIfNeed value typeof<char>
            charArray.SetValue(castedValue, List.head indices)
            let newString = String(charArray)
            x.WriteObject address newString
        | obj -> internalfail $"writing array index to concrete memory: expected to read array, but got {obj}"

    member internal x.WriteBoxedLocation (address : concreteHeapAddress) (obj : obj) : unit =
        let phys = virtToPhys[address]
        let oldObj = phys.object
        assert(oldObj <> null && obj <> null)
        let t = oldObj.GetType()
        assert(obj.GetType() = t)
        let fields = Reflection.fieldsOf false t
        for fieldId, fieldInfo in fields do
            let newFieldValue = fieldInfo.GetValue obj
            fieldInfo.SetValue(oldObj, newFieldValue)
            let child = { object = newFieldValue }
            x.SetChild(phys, child, Field fieldId)

    member internal x.InitializeArray address (rfh : RuntimeFieldHandle) =
        match x.ReadObject address with
        | :? Array as array -> RuntimeHelpers.InitializeArray(array, rfh)
        | obj -> internalfail $"initializing array in concrete memory: expected to read array, but got {obj}"

    member internal x.FillArray address index length (value : obj) =
        match x.ReadObject address with
        | :? Array as array when array.Rank = 1 ->
            for i = index to index + length - 1 do
                array.SetValue(value, i)
        | :? Array -> internalfail "filling array in concrete memory: multidimensional arrays are not supported yet"
        | obj -> internalfail $"filling array in concrete memory: expected to read array, but got {obj}"

    member private x.TrackCopy (array : Array) (dstIndex : int64) length =
        let arrayType = array.GetType()
        let elemType = arrayType.GetElementType()
        if Reflection.isReferenceOrContainsReferences elemType then
            let dstIndex = int dstIndex
            let rank = array.Rank
            let lengths = Array.init rank array.GetLength
            let lowerBounds = Array.init rank array.GetLowerBound
            for i = 0 to int length - 1 do
                let index = Array.delinearizeArrayIndex (dstIndex + i) lengths lowerBounds
                let child = array.GetValue index
                let childKind = Index(index, elemType)
                x.SetChild(array, child, childKind)

    member internal x.CopyArray srcAddress dstAddress (srcIndex : int64) (dstIndex : int64) length =
        match x.ReadObject srcAddress, x.ReadObject dstAddress with
        | :? Array as srcArray, (:? Array as dstArray) ->
            Array.Copy(srcArray, srcIndex, dstArray, dstIndex, length)
            x.TrackCopy dstArray dstIndex length
        | :? String as srcString, (:? String as dstString) ->
            let srcArray = srcString.ToCharArray()
            let dstArray = dstString.ToCharArray()
            Array.Copy(srcArray, srcIndex, dstArray, dstIndex, length)
            let newString = String(dstArray)
            x.WriteObject dstAddress newString
        | :? String as srcString, (:? Array as dstArray) ->
            let srcArray = srcString.ToCharArray()
            Array.Copy(srcArray, srcIndex, dstArray, dstIndex, length)
        | :? Array as srcArray, (:? String as dstString) ->
            let dstArray = dstString.ToCharArray()
            Array.Copy(srcArray, srcIndex, dstArray, dstIndex, length)
            let newString = String(dstArray)
            x.WriteObject dstAddress newString
        | obj -> internalfail $"copying array in concrete memory: expected to read array, but got {obj}"

    member internal x.CopyCharArrayToString arrayAddress stringAddress startIndex =
        let array =
            match x.ReadObject arrayAddress with
            | :? array<char> as array -> array
            | :? string as str -> str.ToCharArray()
            | obj -> internalfail $"CopyCharArrayToString: unexpected array {obj}"
        let string = new string(array[startIndex..]) :> obj
        x.WriteObject stringAddress string
        let physAddress = {object = string}
        physToVirt[physAddress] <- stringAddress

    member internal x.CopyCharArrayToStringLen arrayAddress stringAddress startIndex length =
        let array =
            match x.ReadObject arrayAddress with
            | :? array<char> as array -> array
            | :? string as str -> str.ToCharArray()
            | obj -> internalfail $"CopyCharArrayToStringLen: unexpected array {obj}"
        let string = new string(array[startIndex..(length - 1)]) :> obj
        x.WriteObject stringAddress string
        let physAddress = {object = string}
        physToVirt[physAddress] <- stringAddress

    // ------------------------- Updating children -------------------------

    member internal x.ReTrackObject (obj : obj) =
        let queue = Queue<physicalAddress>()
        let phys = { object = obj }
        let tracked = HashSet<physicalAddress>()
        queue.Enqueue phys
        let phys = ref physicalAddress.Empty
        while queue.TryDequeue phys do
            let phys = phys.Value
            if tracked.Add phys then
                let obj = phys.object
                let typ = TypeUtils.getTypeOfConcrete obj
                match obj with
                | null -> ()
                | _ when TypeUtils.isSolidType typ -> ()
                | :? Array as a ->
                    let elemType = typ.GetElementType()
                    if Reflection.isReferenceOrContainsReferences elemType then
                        let indicesWithValues = Array.getArrayIndicesWithValues a
                        for index, v in indicesWithValues do
                            let index = List.toArray index
                            let child = { object = v }
                            queue.Enqueue child
                            x.SetChild(phys, child, Index(index, elemType))
                | _ when TypeUtils.isDelegate typ ->
                    assert(obj :? Delegate)
                    let target = (obj :?> Delegate).Target
                    let child = { object = target }
                    queue.Enqueue child
                    let targetField = Reflection.delegateTargetField.Value
                    let fieldId = Reflection.wrapField targetField
                    x.SetChild(phys, child, Field fieldId)
                | _ when typ.IsClass ->
                    let fields = Reflection.fieldsOf false typ
                    for fieldId, field in fields do
                        if Reflection.isReferenceOrContainsReferences field.FieldType then
                            let v = field.GetValue obj
                            let child = { object = v }
                            queue.Enqueue child
                            x.SetChild(phys, child, Field fieldId)
                | _ -> ()

    // --------------------------- Static Fields ---------------------------

    member internal x.StaticFieldChanged fieldId =
        changedStaticFields.Add fieldId |> ignore

    member public x.ChangedStaticFields() = changedStaticFields

    // ------------------------------- Remove -------------------------------

    member internal x.Remove address =
        // No need to remove physical addresses from physToVirt, because
        // all objects must contain virtual address, even if they were unmarshalled
        let phys = virtToPhys[address]
        let removed = virtToPhys.Remove address
        assert removed
        x.RemoveFromFullyConcretesRec phys

    member private x.RemoveFromFullyConcretesRec phys =
        let removed = HashSet<physicalAddress>()
        let queue = Queue<physicalAddress>()
        queue.Enqueue phys
        let child = ref physicalAddress.Empty
        while queue.TryDequeue child do
            let child = child.Value
            if removed.Add child then
                fullyConcretes.Remove child |> ignore
                let exists, parentDict = parents.TryGetValue child
                if exists then
                    for KeyValue(parent, childKind) in parentDict do
                        children[parent][childKind] <- Symbolic
                        queue.Enqueue parent
