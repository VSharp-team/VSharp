namespace VSharp.Core

open System
open System.Collections.Generic
open System.Runtime.CompilerServices
open VSharp

type public ConcreteMemory private (physToVirt, virtToPhys) =

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

    member private x.CastIfNeed value containerType =
        if box value <> null then
            let valueType = value.GetType()
            if valueType <> containerType && TypeUtils.canConvert valueType containerType then
                // This is done mostly because of signed to unsigned conversions (Example: int16 -> char)
                TypeUtils.convert value containerType
            else value
        else value

// ------------------------------- Copying -------------------------------

    interface IConcreteMemory with

        override x.Copy() =
            let physToVirt' = Dictionary<physicalAddress, concreteHeapAddress>()
            let virtToPhys' = Dictionary<concreteHeapAddress, physicalAddress>()
            // Need to copy all addresses from physToVirt, because:
            // 1. let complex object (A) contains another object (B),
            // if object (B) was unmarshalled, physToVirt will contain mapping
            // between old object (B) and virtual address of it (addr);
            // symbolic memory will contain info of symbolic object (B) by it's address (addr)
            // So, reading from object (A) object (B) will result in HeapRef (addr),
            // which will be read from symbolic memory
            let copier = Utils.Copier()
            for KeyValue(phys, virt) in physToVirt do
                let phys' = copier.DeepCopy phys
                let exists, oldPhys = virtToPhys.TryGetValue(virt)
                if exists && oldPhys = phys then
                    virtToPhys'.Add(virt, phys')
                // Empty string is interned
                if phys' = {object = String.Empty} then physToVirt'[phys'] <- virt
                else physToVirt'.Add(phys', virt)
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
            | None -> internalfail $"PhysToVirt: unable to get virtual address for object {physAddress}"

        override x.TryPhysToVirt (physAddress : obj) =
            let result = ref List.empty
            if physToVirt.TryGetValue({object = physAddress}, result) then
                Some result.Value
            else None

// ----------------------------- Allocation -----------------------------

        override x.Allocate address (obj : obj) =
            assert(obj <> null)
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
            | obj -> internalfail $"reading array index from concrete memory: expected to read array, but got {obj}"

        override x.GetAllArrayData address =
            match x.ReadObject address with
            | :? Array as array -> Array.getArrayIndicesWithValues array
            | :? String as string -> string.ToCharArray() |> Array.getArrayIndicesWithValues
            | obj -> internalfail $"reading array data concrete memory: expected to read array, but got {obj}"

        override x.ReadArrayLowerBound address dimension =
            match x.ReadObject address with
            | :? Array as array -> array.GetLowerBound(dimension)
            | :? String when dimension = 0 -> 0
            | obj -> internalfail $"reading array lower bound from concrete memory: expected to read array, but got {obj}"

        override x.ReadArrayLength address dimension =
            match x.ReadObject address with
            | :? Array as array -> array.GetLength(dimension)
            | :? String as string when dimension = 0 -> (1 + string.Length) :> obj
            | obj -> internalfail $"reading array length from concrete memory: expected to read array, but got {obj}"

// ------------------------------- Writing -------------------------------

        override x.WriteClassField address (field : fieldId) value =
            let object = x.ReadObject address
            let fieldInfo = Reflection.getFieldInfo field
            let fieldType = fieldInfo.FieldType
            let value = x.CastIfNeed value fieldType
            fieldInfo.SetValue(object, value)

        override x.WriteArrayIndex address (indices : int list) value =
            match x.ReadObject address with
            | :? Array as array ->
                let elemType = array.GetType().GetElementType()
                let castedValue =
                    if value <> null then x.CastIfNeed value elemType
                    else value
                array.SetValue(castedValue, Array.ofList indices)
            // TODO: strings must be immutable! This is used by copying, so copy string another way #hack
            | :? String as string when List.length indices = 1 ->
                let charArray = string.ToCharArray()
                assert(value <> null)
                let castedValue = x.CastIfNeed value typeof<char>
                charArray.SetValue(castedValue, List.head indices)
                let newString = String(charArray)
                x.WriteObject address newString
            | obj -> internalfail $"writing array index to concrete memory: expected to read array, but got {obj}"

        override x.InitializeArray address (rfh : RuntimeFieldHandle) =
            match x.ReadObject address with
            | :? Array as array -> RuntimeHelpers.InitializeArray(array, rfh)
            | obj -> internalfail $"initializing array in concrete memory: expected to read array, but got {obj}"

        override x.FillArray address index length value =
            match x.ReadObject address with
            | :? Array as array when array.Rank = 1 ->
                for i = index to index + length - 1 do
                    array.SetValue(value, i)
            | :? Array -> internalfail "filling array in concrete memory: multidimensional arrays are not supported yet"
            | obj -> internalfail $"filling array in concrete memory: expected to read array, but got {obj}"

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
            | obj -> internalfail $"copying array in concrete memory: expected to read array, but got {obj}"

        override x.CopyCharArrayToString arrayAddress stringAddress startIndex =
            let array =
                match x.ReadObject arrayAddress with
                | :? array<char> as array -> array
                | :? string as str -> str.ToCharArray()
                | obj -> internalfail $"CopyCharArrayToString: unexpected array {obj}"
            let string = new string(array[startIndex..]) :> obj
            x.WriteObject stringAddress string
            let physAddress = {object = string}
            physToVirt[physAddress] <- stringAddress

        override x.CopyCharArrayToStringLen arrayAddress stringAddress startIndex length =
            let array =
                match x.ReadObject arrayAddress with
                | :? array<char> as array -> array
                | :? string as str -> str.ToCharArray()
                | obj -> internalfail $"CopyCharArrayToStringLen: unexpected array {obj}"
            let string = new string(array[startIndex..(length - 1)]) :> obj
            x.WriteObject stringAddress string
            let physAddress = {object = string}
            physToVirt[physAddress] <- stringAddress

    // ------------------------------- Remove -------------------------------

        override x.Remove address =
            // No need to remove physical addresses from physToVirt, because
            // all objects must contain virtual address, even if they were unmarshalled
            let removed = virtToPhys.Remove address
            assert removed
