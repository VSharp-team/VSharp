namespace VSharp.Core

open System
open System.Reflection
open System.Runtime.InteropServices
open VSharp

module internal ArrayInitialization =

    let private reinterpretValueTypeAsByteArray (value : obj) size =
        let rawData = Array.create size Byte.MinValue
        let handle = GCHandle.Alloc(rawData, GCHandleType.Pinned)
        Marshal.StructureToPtr(value, handle.AddrOfPinnedObject(), false)
        handle.Free()
        rawData

    let boolTermCreator (rawData : byte []) index =
        match rawData.[index] with
        | 0uy -> False
        | 1uy -> True
        | _ -> __unreachable__()
    let byteTermCreator (rawData : byte []) index =
        rawData.[index] |> makeNumber

    let signedByteTermCreator (rawData : byte []) index =
        rawData.[index] |> sbyte |> makeNumber

    let charTermCreator (rawData : byte []) index =
        BitConverter.ToChar(rawData, index) |> makeNumber

    let int32TermCreator (rawData : byte []) index =
        BitConverter.ToInt32(rawData, index) |> makeNumber

    let unsignedInt32TermCreator (rawData : byte []) index =
        BitConverter.ToUInt32(rawData, index) |> makeNumber

    let int16TermCreator (rawData : byte []) index =
        BitConverter.ToInt16(rawData, index) |> makeNumber

    let unsignedInt16TermCreator (rawData : byte []) index =
        BitConverter.ToUInt16(rawData, index) |> makeNumber

    let int64TermCreator (rawData : byte []) index =
        BitConverter.ToUInt64(rawData, index) |> makeNumber

    let unsignedInt64TermCreator (rawData : byte []) index =
        BitConverter.ToUInt64(rawData, index) |> makeNumber

    let float32TermCreator (rawData : byte []) index =
        BitConverter.ToSingle(rawData, index) |> makeNumber

    let doubleTermCreator (rawData : byte []) index =
        BitConverter.ToDouble(rawData, index) |> makeNumber

    let private fillInArray termCreator (state : state) address typeOfArray (size : int) (rawData : byte[]) =
        let extractIntFromTerm (term : term) =
            match term.term with
            | Concrete (:? int as v, _) -> v
            | Concrete (:? int16 as v, _) -> int v
            | Concrete (:? uint16 as v, _) -> int v
            | Concrete (:? byte as v, _) -> int v
            | Concrete (:? sbyte as v, _) -> int v
            | Concrete (:? char as v, _) -> int v
            | Concrete (:? int64 as v, _) when v <= int64 System.Int32.MaxValue -> int v
            | Concrete (:? int64, _) -> internalfail "int64 array size is not handled"
            | _ -> __notImplemented__()
        assert (rawData.Length % size = 0)
        let dims = Types.rankOf typeOfArray
        let arrayType = symbolicTypeToArrayType typeOfArray
        let lbs = List.init dims (fun dim -> Memory.readLowerBound state address (makeNumber dim) arrayType |> extractIntFromTerm)
        let lens = List.init dims (fun dim -> Memory.readLength state address (makeNumber dim) arrayType |> extractIntFromTerm)
        let allIndices = Memory.allIndicesOfArray lbs lens
        let indicesAndValues = allIndices |> Seq.mapi (fun i indices -> List.map makeNumber indices, termCreator rawData (i * size)) // TODO: sort if need
        Memory.initializeArray state address indicesAndValues arrayType

    let initializeArray state arrayRef handleTerm =
        match arrayRef.term, handleTerm.term with
        | HeapRef({term = ConcreteHeapAddress address}, _), Concrete (:? RuntimeFieldHandle as rfh, _)
            when ConcreteMemory.contains state.concreteMemory address ->
                ConcreteMemory.initializeArray state address rfh
        | HeapRef(address, sightType), Concrete (:? RuntimeFieldHandle as rfh, _) ->
            let fieldInfo = FieldInfo.GetFieldFromHandle rfh
            let arrayType = Memory.mostConcreteTypeOfHeapRef state address sightType
            let t = Types.elementType arrayType |> Types.toDotNetType
            assert(t.IsValueType) // TODO: be careful about type variables

            let fieldValue : obj = fieldInfo.GetValue null
            let size = ClassType (Id fieldInfo.FieldType, []) |> Types.sizeOf
            let rawData = reinterpretValueTypeAsByteArray fieldValue size
            let fillArray termCreator t = fillInArray termCreator state address arrayType t rawData
            match t with
            | _ when t = typedefof<byte> -> fillArray byteTermCreator sizeof<byte>
            | _ when t = typedefof<sbyte> -> fillArray signedByteTermCreator sizeof<sbyte>
            | _ when t = typedefof<int16> -> fillArray int16TermCreator sizeof<int16>
            | _ when t = typedefof<uint16> -> fillArray unsignedInt16TermCreator sizeof<uint16>
            | _ when t = typedefof<int> -> fillArray int32TermCreator sizeof<int>
            | _ when t = typedefof<uint32> -> fillArray unsignedInt32TermCreator sizeof<uint32>
            | _ when t = typedefof<int64> -> fillArray int64TermCreator sizeof<int64>
            | _ when t = typedefof<uint64> -> fillArray unsignedInt64TermCreator sizeof<uint64>
            | _ when t = typedefof<float32> -> fillArray float32TermCreator sizeof<float32>
            | _ when t = typedefof<double> -> fillArray doubleTermCreator sizeof<double>
            | _ when t = typedefof<bool> -> fillArray boolTermCreator sizeof<bool>
            | _ when t = typedefof<char> -> fillArray charTermCreator sizeof<char>
            | _ -> __notImplemented__()
        | _ -> __notImplemented__()
