namespace VSharp.Concolic

open System
open System.Collections.Generic
open System.Reflection
open VSharp
open VSharp.Core
open VSharp.CSharpUtils

type ConcolicMemory(communicator : Communicator) =
    let physicalAddresses = Dictionary<UIntPtr, concreteHeapAddress Lazy>()
    let virtualAddresses = Dictionary<concreteHeapAddress, UIntPtr>()
    let unmarshalledAddresses = HashSet()

    let zeroHeapAddress = VectorTime.zero

    let arrayRefOffsets (elemType : Type) =
        if not elemType.IsValueType then [| LayoutUtils.ArrayElementsOffset |] else Array.empty

    // ------------------------- Parsing objects from concolic memory -------------------------

    let parseVectorArray bytes elemType =
        let elemSize = TypeUtils.internalSizeOf elemType |> int
        // NOTE: skipping array header
        let mutable offset = LayoutUtils.ArrayLengthOffset(true, 0)
        let length = BitConverter.ToInt64(bytes, offset) |> int
        offset <- offset + 8
        let parseOneElement i =
            let offset = offset + i * elemSize
            Reflection.bytesToObj bytes[offset .. offset + elemSize - 1] elemType
        let array = Array.CreateInstance(elemType, length)
        for i = 0 to length - 1 do
            array.SetValue(parseOneElement i, i)
        array

    let parseString bytes =
        let mutable offset = LayoutUtils.StringLengthOffset
        let length = BitConverter.ToInt32(bytes, offset) |> int
        offset <- LayoutUtils.StringElementsOffset
        let elemSize = sizeof<char>
        let parseOneChar i =
            let offset = offset + i * elemSize
            let obj = Reflection.bytesToObj bytes[offset .. offset + elemSize - 1] typeof<char>
            obj :?> char
        let array : char array = Array.init length parseOneChar
        String(array)

    let parseClass (bytes : byte array) fieldOffsets typ =
        let obj = Reflection.createObject typ
        let parseOneField (info : FieldInfo, offset) =
            let fieldSize = TypeUtils.internalSizeOf info.FieldType |> int
            let value = Reflection.bytesToObj bytes[offset .. offset + fieldSize - 1] info.FieldType
            info.SetValue(obj, value)
        Array.iter parseOneField fieldOffsets
        obj

    interface IConcreteMemory with
        // TODO: support non-vector arrays
        member x.Contains address =
            virtualAddresses.ContainsKey address && not (unmarshalledAddresses.Contains address)

        member x.GetPhysicalAddress virtAddress =
            if virtAddress = zeroHeapAddress then UIntPtr.Zero
            else virtualAddresses[virtAddress]

        member x.ReadArrayIndex address indices arrayType isSting =
            let cm = (x :> IConcreteMemory)
            let elemType, dims, isVector = arrayType
            let readElement linearIndex =
                let t = Types.ToDotNetType elemType
                let isRef = Types.IsValueType elemType |> not
                let size = TypeUtils.internalSizeOf t |> int
                let metadata = if isSting then LayoutUtils.StringElementsOffset else LayoutUtils.ArrayElementsOffset
                let offset = linearIndex * size + metadata
                let address = cm.GetPhysicalAddress address
                let bytes = communicator.ReadHeapBytes address offset size isRef
                Reflection.bytesToObj bytes t
            if isVector then
                assert(List.length indices = 1)
                readElement (List.head indices)
            else
                let lens = Array.init dims (fun dim -> cm.ReadArrayLength address dim arrayType :?> int)
                let lbs = Array.init dims (fun dim -> cm.ReadArrayLowerBound address dim arrayType :?> int)
                let linearIndex = ArrayModule.linearizeArrayIndex indices lens lbs
                readElement linearIndex

        member x.ReadArrayLength address dim arrayType =
            let _, _, isVector = arrayType
            let address = (x :> IConcreteMemory).GetPhysicalAddress address
            let offset = LayoutUtils.ArrayLengthOffset(isVector, dim)
            if isVector then
                let bytes = communicator.ReadHeapBytes address offset sizeof<int> false
                Reflection.bytesToObj bytes typeof<int>
            else internalfail "Length reading for non-vector array is not implemented!"

        member x.ReadArrayLowerBound _ _ arrayType =
            let _, _, isVector = arrayType
            if isVector then 0 :> obj
            else internalfail "Lower bound reading for non-vector array is not implemented!"

        member x.ReadClassField address fieldId =
            let address = (x :> IConcreteMemory).GetPhysicalAddress address
            let fieldInfo = Reflection.getFieldInfo fieldId
            let t = fieldInfo.FieldType
            let offset = Reflection.memoryFieldOffset fieldInfo
            let size = TypeUtils.internalSizeOf t |> int
            let bytes = communicator.ReadHeapBytes address offset size (not t.IsValueType)
            Reflection.bytesToObj bytes t

        member x.GetAllArrayData address arrayType =
            let cm = x :> IConcreteMemory
            let elemType, dims, isVector = arrayType
            let elemType = Types.ToDotNetType elemType
            let physAddress = cm.GetPhysicalAddress address
            let refOffsets = arrayRefOffsets elemType
            let bytes = communicator.ReadWholeObject physAddress true refOffsets
            if isVector then
                let array = parseVectorArray bytes elemType
                List.init array.Length (fun i -> List.singleton i, array.GetValue i)
            else internalfailf "GetAllArrayData: getting array data from non-vector array (rank = %O) is not implemented!" dims

        // NOTE: 'Unmarshall' function gets all bytes from concolic memory and gives control of 'address' to SILI
        member x.Unmarshall address typ =
            let success = unmarshalledAddresses.Add address
            assert(success)
            let address = (x :> IConcreteMemory).GetPhysicalAddress address
            match typ with
            // NOTE: sending references offsets to resolve references' bytes inside concolic
            | _ when typ.IsSZArray ->
                let elemType = typ.GetElementType()
                let refOffsets = arrayRefOffsets elemType
                let bytes = communicator.Unmarshall address true refOffsets
                parseVectorArray bytes elemType
            | _ when typ.IsArray ->
                let rank = typ.GetArrayRank()
                internalfailf "Unmarshalling non-vector array (rank = %O) is not implemented!" rank
            | _ when typ = typeof<String> ->
                let bytes = communicator.Unmarshall address false Array.empty
                parseString bytes
            | _ ->
                assert(not typ.IsValueType)
                let fields = Reflection.fieldsOf false typ
                let fieldOffsets = Array.map (fun (_, info) -> info, Reflection.memoryFieldOffset info) fields
                let getRefOffset (f : FieldInfo, offset) = if not f.FieldType.IsValueType then Some offset else None
                let refOffsets = fieldOffsets |> Array.choose getRefOffset
                let bytes = communicator.Unmarshall address false refOffsets
                parseClass bytes fieldOffsets typ

        member x.Allocate physAddress virtAddress =
            physicalAddresses.Add(physAddress, virtAddress)

        member x.GetVirtualAddress physAddress =
            if physAddress = UIntPtr.Zero then zeroHeapAddress
            else
                let virtualAddress = physicalAddresses[physAddress].Value
                if virtualAddresses.ContainsKey virtualAddress |> not then
                    virtualAddresses.Add(virtualAddress, physAddress)
                virtualAddress
