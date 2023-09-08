namespace VSharp.Core

open System
open System.Collections.Generic
open System.Runtime.CompilerServices
open VSharp

[<CustomEquality; NoComparison>]
type private concreteMemoryKey =
    | RefKey of physicalAddress
    | ValueKey of concreteMemorySource * physicalAddress
    with
    override x.GetHashCode() =
        match x with
        | RefKey phys -> phys.GetHashCode()
        | ValueKey(source, _) -> source.GetHashCode()

    override x.Equals(other) =
        match other, x with
        | :? concreteMemoryKey as RefKey otherPhys, RefKey phys -> otherPhys.Equals phys
        | :? concreteMemoryKey as ValueKey(otherSource, _), ValueKey(source, _) -> otherSource.Equals source
        | _ -> false

    member x.ToPhysicalAddress() =
        match x with
        | RefKey phys -> phys
        | ValueKey(_, phys) -> phys

    member x.FromPhysicalAddress (phys : physicalAddress) =
        match x with
        | RefKey _ -> RefKey phys
        | ValueKey(source, _) -> ValueKey(source, phys)

type public ConcreteMemory private (physToVirt, virtToPhys) =

// ----------------------------- Constructor -----------------------------

    new () =
        let physToVirt = Dictionary<concreteMemoryKey, concreteHeapAddress>()
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
            let physToVirt' = Dictionary<concreteMemoryKey, concreteHeapAddress>()
            let virtToPhys' = Dictionary<concreteHeapAddress, physicalAddress>()
            // Need to copy all addresses from physToVirt, because:
            // 1. let complex object (A) contains another object (B),
            // if object (B) was unmarshalled, physToVirt will contain mapping
            // between old object (B) and virtual address of it (addr);
            // symbolic memory will contain info of symbolic object (B) by it's address (addr)
            // So, reading from object (A) object (B) will result in HeapRef (addr),
            // which will be read from symbolic memory
            let copier = Utils.Copier()
            for KeyValue(key, virt) in physToVirt do
                let phys = key.ToPhysicalAddress()
                let phys' = copier.DeepCopy phys
                let exists, oldPhys = virtToPhys.TryGetValue(virt)
                if exists && oldPhys = phys then
                    virtToPhys'.Add(virt, phys')
                let key' = key.FromPhysicalAddress phys'
                // Empty string is interned
                if key' = RefKey {object = String.Empty} then physToVirt'[key'] <- virt
                else physToVirt'.Add(key', virt)
            ConcreteMemory(physToVirt', virtToPhys')

// ----------------------------- Primitives -----------------------------

        override x.Contains address =
            virtToPhys.ContainsKey address

        override x.VirtToPhys virtAddress =
            x.ReadObject virtAddress

        override x.TryVirtToPhys virtAddress =
            let exists, result = virtToPhys.TryGetValue(virtAddress)
            if exists then Some result.object
            else None

        override x.PhysToVirt physAddress =
            let cm = x :> IConcreteMemory
            match cm.TryPhysToVirt physAddress with
            | Some address -> address
            | None -> internalfailf "PhysToVirt: unable to get virtual address for object %O" physAddress

        override x.TryPhysToVirt (physAddress : obj) =
            assert(physAddress :? ValueType |> not)
            let result = ref List.empty
            if physToVirt.TryGetValue(RefKey {object = physAddress}, result) then
                Some result.Value
            else None

        override x.TryPhysToVirt (source : concreteMemorySource) =
            let result = ref List.empty
            let key = ValueKey(source, physicalAddress.Empty)
            if physToVirt.TryGetValue(key, result) then
                Some result.Value
            else None

// ----------------------------- Allocation -----------------------------

        override x.AllocateRefType address (obj : obj) =
            assert(obj <> null)
            assert(obj :? ValueType |> not)
            assert(virtToPhys.ContainsKey address |> not)
            // Suppressing finalize, because 'obj' may implement 'Dispose()' method, which should not be invoked,
            // because object may be in incorrect state (statics, for example)
            GC.SuppressFinalize(obj)
            let physicalAddress = {object = obj}
            virtToPhys.Add(address, physicalAddress)
            if obj = String.Empty then
                physToVirt[RefKey physicalAddress] <- address
            else physToVirt.Add(RefKey physicalAddress, address)

        override x.AllocateValueType address source (obj : obj) =
            assert(obj <> null)
            assert(obj :? ValueType)
            GC.SuppressFinalize(obj)
            let physicalAddress = {object = obj}
            virtToPhys.Add(address, physicalAddress)
            // Rewriting old unmarshalled values
            physToVirt[ValueKey(source, physicalAddress)] <- address

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
            | :? Array as array -> Array.getArrayIndicesWithValues array
            | :? String as string -> string.ToCharArray() |> Array.getArrayIndicesWithValues
            | obj -> internalfailf "reading array data concrete memory: expected to read array, but got %O" obj

        override x.ReadArrayLowerBound address dimension =
            match x.ReadObject address with
            | :? Array as array -> array.GetLowerBound(dimension)
            | :? String when dimension = 0 -> 0
            | obj -> internalfailf "reading array lower bound from concrete memory: expected to read array, but got %O" obj

        override x.ReadArrayLength address dimension =
            match x.ReadObject address with
            | :? Array as array -> array.GetLength(dimension)
            | :? String as string when dimension = 0 -> (1 + string.Length) :> obj
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
            physToVirt[RefKey physAddress] <- stringAddress

        override x.CopyCharArrayToStringLen arrayAddress stringAddress length =
            let array = x.ReadObject arrayAddress :?> char array
            let string = new string(array[0..(length - 1)]) :> obj
            x.WriteObject stringAddress string
            let physAddress = {object = string}
            physToVirt[RefKey physAddress] <- stringAddress

    // ------------------------------- Remove -------------------------------

        override x.Remove address =
            // No need to remove physical addresses from physToVirt, because
            // all objects must contain virtual address, even if they were unmarshalled
            let removed = virtToPhys.Remove address
            assert removed
