namespace VSharp.Core

open System
open System.Reflection
open System.Runtime.InteropServices
open VSharp
open TypeUtils

module internal ArrayInitialization =

    let private boolTermCreator (rawData : byte []) index =
        match rawData.[index] with
        | 0uy -> False()
        | 1uy -> True()
        | _ -> __unreachable__()
    let private byteTermCreator (rawData : byte []) index =
        rawData[index] |> makeNumber

    let private signedByteTermCreator (rawData : byte []) index =
        rawData[index] |> sbyte |> makeNumber

    let private charTermCreator (rawData : byte []) index =
        BitConverter.ToChar(rawData, index) |> makeNumber

    let private int32TermCreator (rawData : byte []) index =
        BitConverter.ToInt32(rawData, index) |> makeNumber

    let private unsignedInt32TermCreator (rawData : byte []) index =
        BitConverter.ToUInt32(rawData, index) |> makeNumber

    let private int16TermCreator (rawData : byte []) index =
        BitConverter.ToInt16(rawData, index) |> makeNumber

    let private unsignedInt16TermCreator (rawData : byte []) index =
        BitConverter.ToUInt16(rawData, index) |> makeNumber

    let private int64TermCreator (rawData : byte []) index =
        BitConverter.ToUInt64(rawData, index) |> makeNumber

    let private unsignedInt64TermCreator (rawData : byte []) index =
        BitConverter.ToUInt64(rawData, index) |> makeNumber

    let private float32TermCreator (rawData : byte []) index =
        BitConverter.ToSingle(rawData, index) |> makeNumber

    let private doubleTermCreator (rawData : byte []) index =
        BitConverter.ToDouble(rawData, index) |> makeNumber

    let private fillInArray termCreator (state : state) address typeOfArray (size : int) (rawData : byte[]) =
        let extractIntFromTerm (term : term) =
            match term.term with
            | Concrete (v, _) -> NumericUtils.ObjToInt v
            | _ -> __notImplemented__()
        assert (rawData.Length % size = 0)
        let dims = rankOf typeOfArray
        let arrayType = symbolicTypeToArrayType typeOfArray
        let memory = state.memory
        let lbs = List.init dims (fun dim -> memory.ReadLowerBound address (makeNumber dim) arrayType |> extractIntFromTerm)
        let lens = List.init dims (fun dim -> memory.ReadLength address (makeNumber dim) arrayType |> extractIntFromTerm)
        let allIndices = Array.allIndicesViaLens lbs lens
        let indicesAndValues =
            Seq.mapi (fun i indices -> List.map makeNumber indices, termCreator rawData (i * size)) allIndices
        memory.InitializeArray address indicesAndValues arrayType

    let commonInitializeArray (state : state) address typ (handle : RuntimeFieldHandle) =
        let memory = state.memory
        let cm = memory.ConcreteMemory
        match address.term with
        | ConcreteHeapAddress a when cm.Contains a ->
            cm.InitializeArray a handle
        | _ ->
            let fieldInfo = FieldInfo.GetFieldFromHandle handle
            let arrayType = memory.MostConcreteTypeOfHeapRef address typ
            let t = arrayType.GetElementType()
            assert t.IsValueType // TODO: be careful about type variables
            let rawData = Reflection.byteArrayFromField fieldInfo
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

    let initializeArray (state : state) arrayRef handleTerm =
        assert(Terms.isStruct handleTerm)
        match arrayRef.term, state.memory.TryTermToObj handleTerm with
        | HeapRef(address, typ), Some(:? RuntimeFieldHandle as rfh) ->
            commonInitializeArray state address typ rfh
        | _ -> internalfailf $"initializeArray: case for (arrayRef = {arrayRef}), (handleTerm = {handleTerm}) is not implemented"

    let allocateOptimizedArray (state : state) (fieldInfo : FieldInfo) =
        let arrayType = typeof<byte>.MakeArrayType()
        let lb = makeNumber 0
        let length = internalSizeOf fieldInfo.FieldType |> makeNumber
        let array = state.memory.AllocateArray arrayType [lb] [length]
        commonInitializeArray state array arrayType fieldInfo.FieldHandle
        HeapRef array arrayType
