namespace VSharp.System

open global.System
open VSharp
open VSharp.Core
open System.Runtime.InteropServices
open System.Reflection

module Runtime_CompilerServices_RuntimeHelpers =

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
        rawData.[index] |> MakeNumber

    let signedByteTermCreator (rawData : byte []) index =
        rawData.[index] |> sbyte |> MakeNumber

    let charTermCreator (rawData : byte []) index =
        rawData.[index] |> char |> MakeNumber

    let int32TermCreator (rawData : byte []) index =
        BitConverter.ToInt32(rawData, index) |> MakeNumber

    let unsignedInt32TermCreator (rawData : byte []) index =
        BitConverter.ToUInt32(rawData, index) |> MakeNumber

    let int16TermCreator (rawData : byte []) index =
        BitConverter.ToInt16(rawData, index) |> MakeNumber

    let unsignedInt16TermCreator (rawData : byte []) index =
        BitConverter.ToUInt16(rawData, index) |> MakeNumber

    let int64TermCreator (rawData : byte []) index =
        BitConverter.ToUInt64(rawData, index) |> MakeNumber

    let unsignedInt64TermCreator (rawData : byte []) index =
        BitConverter.ToUInt64(rawData, index) |> MakeNumber

    let float32TermCreator (rawData : byte []) index =
        BitConverter.ToSingle(rawData, index) |> MakeNumber

    let doubleTermCreator (rawData : byte []) index =
        BitConverter.ToDouble(rawData, index) |> MakeNumber

    let private fillInArray termCreator (state : state) arrayRef (size : int) (rawData : byte[]) = // TODO: move this code to Core
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
//        let array = Memory.ReadSafe state arrayRef
        let dimensionsNumberTerm = Memory.ArrayRank state arrayRef
        let dimensionsNumber = extractIntFromTerm dimensionsNumberTerm
        let rec helper currentDimension (multiIndex : term list) (state, j) =
            if currentDimension = dimensionsNumber then
                let valueTerm = termCreator rawData (j * size)
//                let ref = Memory.ReferenceArrayIndex state arrayRef multiIndex
                let states = Memory.WriteArrayIndex state arrayRef multiIndex valueTerm
                match states with
                | [state] -> (state, j + 1)
                | _ -> __notImplemented__()
            else
                let currentDimensionTerm = MakeNumber currentDimension
                let currentLengthTerm = Memory.ArrayLengthByDimension state arrayRef currentDimensionTerm
                let currentLength = extractIntFromTerm currentLengthTerm
                List.init currentLength id |>
                List.fold (fun (state, j) i ->
                    let indexTerm = MakeNumber i
                    helper (currentDimension + 1) (List.append multiIndex [indexTerm]) (state, j)) (state, j)
        helper 0 [] (state, 0) |> fst

    let initializeArray state arrayRef handleTerm =
        match handleTerm.term with
        | Concrete (:? RuntimeFieldHandle as rfh, _) ->
            let fieldInfo = FieldInfo.GetFieldFromHandle rfh
            let elemType = MostConcreteTypeOfHeapRef state arrayRef |> Types.ElementType
            let t = Types.ToDotNetType elemType
            assert (t.IsValueType) // TODO: be careful about type variables

            let fieldValue : obj = fieldInfo.GetValue null
            let size = ClassType (Id fieldInfo.FieldType, []) |> API.Types.SizeOf
            let rawData = reinterpretValueTypeAsByteArray fieldValue size
            let state =
                match t with
                | _ when t = typedefof<byte> ->
                    fillInArray byteTermCreator state arrayRef sizeof<byte> rawData
                | _ when t = typedefof<sbyte> ->
                    fillInArray signedByteTermCreator state arrayRef sizeof<sbyte> rawData
                | _ when t = typedefof<int16> ->
                    fillInArray int16TermCreator state arrayRef sizeof<int16> rawData
                | _ when t = typedefof<uint16> ->
                    fillInArray unsignedInt16TermCreator state arrayRef sizeof<uint16> rawData
                | _ when t = typedefof<int> ->
                    fillInArray int32TermCreator state arrayRef sizeof<int> rawData
                | _ when t = typedefof<uint32> ->
                    fillInArray unsignedInt32TermCreator state arrayRef sizeof<uint32> rawData
                | _ when t = typedefof<int64> ->
                    fillInArray int64TermCreator state arrayRef sizeof<int64> rawData
                | _ when t = typedefof<uint64> ->
                    fillInArray unsignedInt64TermCreator state arrayRef sizeof<uint64> rawData
                | _ when t = typedefof<float32> ->
                    fillInArray float32TermCreator state arrayRef sizeof<float32> rawData
                | _ when t = typedefof<double> ->
                    fillInArray doubleTermCreator state arrayRef sizeof<double> rawData
                | _ when t = typedefof<bool> ->
                    fillInArray boolTermCreator state arrayRef sizeof<bool> rawData
                | _ when t = typedefof<char> ->
                    fillInArray charTermCreator state arrayRef sizeof<char> rawData
                | _ -> __notImplemented__()
            Nop, {state with returnRegister = None}
        | _ -> __notImplemented__(), state

    let InitializeArray (state : state) arrayRef handleTerm : state list =
        GuardedStatedApplyStatementK state arrayRef (fun state arrayRef k ->
        GuardedStatedApplyStatementK state handleTerm (fun state handleTerm k ->
        initializeArray state arrayRef handleTerm |> k) (List.map k >> List.concat)) (List.map snd)

    // This function checks, whether type can be checked on equality using only it's bits
    // Example: any value type, because it doesn't have metadata
    let IsBitwiseEquatable (state : state) (args : term list) : term * state =
        assert(List.length args = 1)
        let typ = List.head args
        match typ with
        | {term = Concrete(:? System.Type as typ, _)} -> MakeBool typ.IsValueType, state
        | _ -> __unreachable__()
