namespace VSharp.Core

open System
open System.Collections.Generic
open System.Runtime.Serialization
open System.Runtime.CompilerServices
open System.Threading
open VSharp

type public ConcreteMemory private (physToVirt, virtToPhys) =

    let mutable physToVirt = physToVirt
    let mutable virtToPhys = virtToPhys

// ----------------------------- Helpers -----------------------------

    static let nonCopyableTypes = [
        typeof<Type>
        typeof<Thread>
    ]

    let cannotBeCopied typ = List.contains typ nonCopyableTypes

    let getArrayIndicesWithValues (array : Array) =
        let ubs = List.init array.Rank array.GetUpperBound
        let lbs = List.init array.Rank array.GetLowerBound
        let indices = List.map2 (fun lb ub -> [lb .. ub]) lbs ubs |> List.cartesian
        indices |> Seq.map (fun index -> index, array.GetValue(Array.ofList index))

    let copiedObjects = Dictionary<physicalAddress, physicalAddress>()

    let rec deepCopyObject (phys : physicalAddress) =
        let obj = phys.object
        let typ = TypeUtils.getTypeOfConcrete obj
        match obj with
        | null -> phys
        | _ when cannotBeCopied typ || TypeUtils.isPrimitive typ || typ.IsEnum || typ.IsPointer -> phys
        | :? System.Reflection.Pointer -> phys
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
            let indices = Array.allIndicesOfArray (Array.toList lowerBounds) (Array.toList lengths)
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
            let physicalAddress = {object = obj}
            virtToPhys.Add(address, physicalAddress)
            if obj = String.Empty then physToVirt[physicalAddress] <- address
            else physToVirt.Add(physicalAddress, address)

// ------------------------------- Reading -------------------------------

        override x.ReadClassField address (field : fieldId) =
            let object = x.ReadObject address
            let fieldInfo = Reflection.getFieldInfo field
            fieldInfo.GetValue(object)

        // TODO: catch possible exceptions and raise exception register
        override x.ReadArrayIndex address (indices : int list) =
            match x.ReadObject address with
            | :? Array as array -> array.GetValue(Array.ofList indices)
            | :? String as string when List.length indices = 1 -> string.[List.head indices] :> obj
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
            match x.ReadObject address with
            | :? Array as array ->
                array.SetValue(value, Array.ofList indices)
            // TODO: strings must be immutable! This is used by copying, so copy string another way #hack
            | :? String as string when List.length indices = 1 ->
                let charArray = string.ToCharArray()
                charArray.SetValue(value, List.head indices)
                let newString = String(charArray)
                x.WriteObject address newString
            | obj -> internalfailf "writing array index to concrete memory: expected to read array, but got %O" obj

        override x.InitializeArray address (rfh : RuntimeFieldHandle) =
            match x.ReadObject address with
            | :? Array as array -> RuntimeHelpers.InitializeArray(array, rfh)
            | obj -> internalfailf "initializing array in concrete memory: expected to read array, but got %O" obj

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
