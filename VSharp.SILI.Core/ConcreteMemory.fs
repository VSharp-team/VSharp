namespace VSharp.Core

open System
open System.Collections.Generic
open System.Runtime.Serialization
open System.Runtime.CompilerServices
open System.Threading
open VSharp

type public ConcreteMemory private (physToVirt, virtToPhys) =

// ----------------------------- Helpers -----------------------------

    static let nonCopyableTypes = [
        typeof<Type>
        typeof<Thread>
    ]

    let cannotBeCopied (typ : Type) = List.exists typ.IsAssignableTo nonCopyableTypes

    let indexedArrayElemsCommon (arr : Array) =
        let ubs = Array.init arr.Rank arr.GetUpperBound
        let lbs = Array.init arr.Rank arr.GetLowerBound
        let idx = Array.copy lbs
        let rec incrementIdx d =
            if d >= 0 then
                if idx[d] = ubs[d] then
                    idx[d] <- lbs[d]
                    incrementIdx (d - 1)
                else
                    idx[d] <- idx[d] + 1
        seq {
            for element in arr do
                yield idx |> Array.toList, element
                incrementIdx <| arr.Rank - 1
        }

    let indexedArrayElemsLin (arr : Array) =
        let mutable idx = arr.GetLowerBound(0)
        seq {
            for element in arr do
                yield idx |> List.singleton, element
                idx <- idx + 1
        }

    let getArrayIndicesWithValues (array : Array) =
        assert(array <> null)
        match array with
        // Any T[] when T is reference type is matched with 'array<obj>'
        | :? array<obj> as a -> Array.mapi (fun i x -> (List.singleton i, x :> obj)) a :> seq<int list * obj>
        | :? array<bool> as a -> Array.mapi (fun i x -> (List.singleton i, x :> obj)) a :> seq<int list * obj>
        | :? array<int8> as a -> Array.mapi (fun i x -> (List.singleton i, x :> obj)) a :> seq<int list * obj>
        | :? array<uint8> as a -> Array.mapi (fun i x -> (List.singleton i, x :> obj)) a :> seq<int list * obj>
        | :? array<int16> as a -> Array.mapi (fun i x -> (List.singleton i, x :> obj)) a :> seq<int list * obj>
        | :? array<uint16> as a -> Array.mapi (fun i x -> (List.singleton i, x :> obj)) a :> seq<int list * obj>
        | :? array<int> as a -> Array.mapi (fun i x -> (List.singleton i, x :> obj)) a :> seq<int list * obj>
        | :? array<uint> as a -> Array.mapi (fun i x -> (List.singleton i, x :> obj)) a :> seq<int list * obj>
        | :? array<int64> as a -> Array.mapi (fun i x -> (List.singleton i, x :> obj)) a :> seq<int list * obj>
        | :? array<uint64> as a -> Array.mapi (fun i x -> (List.singleton i, x :> obj)) a :> seq<int list * obj>
        | :? array<single> as a -> Array.mapi (fun i x -> (List.singleton i, x :> obj)) a :> seq<int list * obj>
        | :? array<double> as a -> Array.mapi (fun i x -> (List.singleton i, x :> obj)) a :> seq<int list * obj>
        | _ when array.GetType().IsSZArray -> indexedArrayElemsLin array
        | _ -> indexedArrayElemsCommon array

    let copiedObjects = Dictionary<physicalAddress, physicalAddress>()

    let rec deepCopyObject (phys : physicalAddress) =
        let obj = phys.object
        let typ = TypeUtils.getTypeOfConcrete obj
        let shouldNotCopy typ =
            cannotBeCopied typ || TypeUtils.isPrimitive typ
            || typ.IsEnum || typ.IsPointer || typ = typeof<System.Reflection.Pointer>
            || typ = typeof<IntPtr> || typ = typeof<UIntPtr>
        match obj with
        | null -> phys
        | _ when shouldNotCopy typ -> phys
        | _ -> deepCopyComplex phys typ

    and deepCopyComplex (phys : physicalAddress) typ =
        let copied = ref {object = null}
        if copiedObjects.TryGetValue(phys, copied) then copied.Value
        else createCopyComplex phys typ

    and createCopyComplex (phys : physicalAddress) typ =
        let obj = phys.object
        match obj with
        | :? Array as a when typ.GetElementType().IsPrimitive ->
            let phys' = {object = a.Clone()}
            copiedObjects.Add(phys, phys')
            phys'
        | :? Array as a ->
            let rank = a.Rank
            let dims = Array.init rank id
            let lengths = Array.map a.GetLength dims
            let lowerBounds = Array.map a.GetLowerBound dims
            let a' = Array.CreateInstance(typ.GetElementType(), lengths, lowerBounds)
            let phys' = {object = a'}
            copiedObjects.Add(phys, phys')
            let indices = Array.allIndicesViaLens (Array.toList lowerBounds) (Array.toList lengths)
            for index in indices do
                let index = List.toArray index
                let v' = deepCopyObject {object = a.GetValue index}
                a'.SetValue(v'.object, index)
            phys'
        | :? String as s ->
            let phys' = {object = String(s)}
            copiedObjects.Add(phys, phys')
            phys'
        | _ when typ.IsClass || typ.IsValueType ->
            let obj' = FormatterServices.GetUninitializedObject typ
            let phys' = {object = obj'}
            copiedObjects.Add(phys, phys')
            let fields = Reflection.fieldsOf false typ
            for _, field in fields do
                let v' = deepCopyObject {object = field.GetValue obj}
                field.SetValue(obj', v'.object)
            phys'
        | _ -> internalfailf "ConcreteMemory, deepCopyObject: unexpected object %O" obj

// ----------------------------- Constructor -----------------------------

    new () =
        let physToVirt = Dictionary<physicalAddress, concreteHeapAddress>()
        let virtToPhys = Dictionary<concreteHeapAddress, physicalAddress>()
        ConcreteMemory(physToVirt, virtToPhys)

// ----------------------------- Primitives -----------------------------

    member private x.ReadObject address =
        assert(virtToPhys.ContainsKey address)
        virtToPhys[address].object

    member private x.WriteObject address obj =
        assert(virtToPhys.ContainsKey address)
        let physicalAddress = {object = obj}
        virtToPhys[address] <- physicalAddress

// ------------------------------- Copying -------------------------------

    interface IConcreteMemory with

        override x.Copy() =
            let physToVirt' = Dictionary<physicalAddress, concreteHeapAddress>()
            let virtToPhys' = Dictionary<concreteHeapAddress, physicalAddress>()
            copiedObjects.Clear()
            for kvp in physToVirt do
                let phys, virt = kvp.Key, kvp.Value
                let phys' = deepCopyObject phys
                if virtToPhys.ContainsKey virt then
                    virtToPhys'.Add(virt, phys')
                physToVirt'.Add(phys', virt)
            ConcreteMemory(physToVirt', virtToPhys')

// ----------------------------- Primitives -----------------------------

        override x.Contains address =
            virtToPhys.ContainsKey address

        // TODO: leave only one function #refactor
        override x.VirtToPhys virtAddress = x.ReadObject virtAddress

        override x.TryVirtToPhys virtAddress =
            let result = ref {object = null}
            if virtToPhys.TryGetValue(virtAddress, result) then
                Some result.Value.object
            else None

        override x.PhysToVirt physAddress =
            let cm = x :> IConcreteMemory
            match cm.TryPhysToVirt physAddress with
            | Some address -> address
            | None -> internalfailf "PhysToVirt: unable to get virtual address for object %O" physAddress

        override x.TryPhysToVirt physAddress =
            let result = ref List.empty
            if physToVirt.TryGetValue({object = physAddress}, result) then
                Some result.Value
            else None

// ----------------------------- Allocation -----------------------------

        override x.Allocate address (obj : obj) =
            assert(virtToPhys.ContainsKey address |> not)
            // Suppressing finalize, because 'obj' may implement 'Dispose()' method, which should not be invoked,
            // because object may be in incorrect state (statics, for example)
            GC.SuppressFinalize(obj)
            let physicalAddress = {object = obj}
            virtToPhys.Add(address, physicalAddress)
            physToVirt[physicalAddress] <- address

// ------------------------------- Reading -------------------------------

        override x.ReadClassField address (field : fieldId) =
            let object = x.ReadObject address
            let fieldInfo = Reflection.getFieldInfo field
            fieldInfo.GetValue(object)

        // TODO: catch possible exceptions and raise exception register
        override x.ReadArrayIndex address (indices : int list) =
            match x.ReadObject address with
            | :? Array as array -> array.GetValue(Array.ofList indices)
            | :? String as string when List.length indices = 1 ->
                let index = List.head indices
                // Case 'index = string.Length' is needed for unsafe string reading: string contents end with null terminator
                // In safe context this case will be filtered out in 'Interpreter', which checks indices before memory access
                if index = string.Length then Char.MinValue :> obj
                else string[index] :> obj
            | obj -> internalfailf "reading array index from concrete memory: expected to read array, but got %O" obj

        override x.GetAllArrayData address =
            match x.ReadObject address with
            | :? Array as array -> getArrayIndicesWithValues array
            | :? String as string -> string.ToCharArray() |> getArrayIndicesWithValues
            | obj -> internalfailf "reading array data concrete memory: expected to read array, but got %O" obj

        override x.ReadArrayLowerBound address dimension =
            match x.ReadObject address with
            | :? Array as array -> array.GetLowerBound(dimension)
            | :? String when dimension = 0 -> 0
            | obj -> internalfailf "reading array lower bound from concrete memory: expected to read array, but got %O" obj

        override x.ReadArrayLength address dimension =
            match x.ReadObject address with
            | :? Array as array -> array.GetLength(dimension)
            | :? String as string when dimension = 0 -> string.Length
            | obj -> internalfailf "reading array length from concrete memory: expected to read array, but got %O" obj

// ------------------------------- Writing -------------------------------

        override x.WriteClassField address (field : fieldId) value =
            let object = x.ReadObject address
            let fieldInfo = Reflection.getFieldInfo field
            fieldInfo.SetValue(object, value)

        override x.WriteArrayIndex address (indices : int list) value =
            let castElement value elemType =
                let valueType = value.GetType()
                if valueType <> elemType && TypeUtils.canConvert valueType elemType then
                    // This is done mostly because of signed to unsigned conversions (Example: int16 -> char)
                    TypeUtils.convert value elemType
                else value
            match x.ReadObject address with
            | :? Array as array ->
                let elemType = array.GetType().GetElementType()
                let castedValue =
                    if value <> null then castElement value elemType
                    else value
                array.SetValue(castedValue, Array.ofList indices)
            // TODO: strings must be immutable! This is used by copying, so copy string another way #hack
            | :? String as string when List.length indices = 1 ->
                let charArray = string.ToCharArray()
                assert(value <> null)
                let castedValue = castElement value typeof<char>
                charArray.SetValue(castedValue, List.head indices)
                let newString = String(charArray)
                x.WriteObject address newString
            | obj -> internalfailf "writing array index to concrete memory: expected to read array, but got %O" obj

        override x.InitializeArray address (rfh : RuntimeFieldHandle) =
            match x.ReadObject address with
            | :? Array as array -> RuntimeHelpers.InitializeArray(array, rfh)
            | obj -> internalfailf "initializing array in concrete memory: expected to read array, but got %O" obj

        override x.FillArray address index length value =
            match x.ReadObject address with
            | :? Array as array when array.Rank = 1 ->
                for i = index to index + length do
                    array.SetValue(value, i)
            | :? Array -> internalfail "filling array in concrete memory: multidimensional arrays are not supported yet"
            | obj -> internalfailf "filling array in concrete memory: expected to read array, but got %O" obj

        override x.CopyArray srcAddress dstAddress srcIndex dstIndex length =
            match x.ReadObject srcAddress, x.ReadObject dstAddress with
            | :? Array as srcArray, (:? Array as dstArray) ->
                Array.Copy(srcArray, srcIndex, dstArray, dstIndex, length)
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
            | obj -> internalfailf "copying array in concrete memory: expected to read array, but got %O" obj

        override x.CopyCharArrayToString arrayAddress stringAddress =
            let array = x.ReadObject arrayAddress :?> char array
            let string = new string(array) :> obj
            x.WriteObject stringAddress string
            let physAddress = {object = string}
            physToVirt[physAddress] <- stringAddress

    // ------------------------------- Remove -------------------------------

        override x.Remove address =
            let removed = virtToPhys.Remove address
            assert removed
