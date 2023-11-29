namespace VSharp.System

open System
open System.Collections.Generic
open VSharp
open VSharp.Core

module internal Enum =

    let InternalGetCorElementType (state : state) (args : term list) =
        assert(List.length args = 1)
        let enum = args[0]
        let enumType =
            if IsReference enum then MostConcreteTypeOfRef state enum
            else TypeOf enum
        if enumType.Equals(typeof<Enum>) then
            __insufficientInformation__ $"InternalGetCorElementType: type of enum {enum} is unknown"
        else
            assert enumType.IsEnum
            let underlyingType = enumType.GetEnumUnderlyingType()
            let value =
                match underlyingType with
                | _ when underlyingType = typeof<bool> -> 0x02
                | _ when underlyingType = typeof<char> -> 0x03
                | _ when underlyingType = typeof<int8> -> 0x04
                | _ when underlyingType = typeof<uint8> -> 0x05
                | _ when underlyingType = typeof<int16> -> 0x06
                | _ when underlyingType = typeof<uint16> -> 0x07
                | _ when underlyingType = typeof<int32> -> 0x08
                | _ when underlyingType = typeof<uint32> -> 0x09
                | _ when underlyingType = typeof<int64> -> 0x0A
                | _ when underlyingType = typeof<uint64> -> 0x0B
                | _ when underlyingType = typeof<single> -> 0x0C
                | _ when underlyingType = typeof<double> -> 0x0D
                | _ when underlyingType = typeof<IntPtr> -> 0x18
                | _ when underlyingType = typeof<UIntPtr> -> 0x19
                | _ -> internalfail $"InternalGetCorElementType: unexpected underlying type {underlyingType}"
            let corType = Reflection.mscorlibAssembly.GetType("System.Reflection.CorElementType")
            Enum.ToObject(corType, value) |> MakeNumber

    let GetEnumValuesAndNames (state : state) (args : term list) : (term * state) list =
        assert(List.length args = 4)
        let enumTypeHandle = args[0]
        let valuesRef = args[1]
        let namesRef = args[2]
        let getNamesFlag = args[3]
        let assembly = Reflection.mscorlibAssembly
        let handleType = assembly.GetType("System.Runtime.CompilerServices.QCallTypeHandle")
        let refOnStackType = assembly.GetType("System.Runtime.CompilerServices.ObjectHandleOnStack")
        let handleFields = Reflection.fieldsOf false handleType
        assert(Array.length handleFields = 2)
        let refFields = Reflection.fieldsOf false refOnStackType
        assert(Array.length refFields = 1)
        let ptrHandleField = handleFields |> Array.find (fun (fieldId, _) -> fieldId.name = "_ptr") |> fst
        let refField = refFields |> Array.find (fun (fieldId, _) -> fieldId.name = "_ptr") |> fst
        let ptrHandle = Memory.ReadField state enumTypeHandle ptrHandleField
        let valuesPtr = Memory.ReadField state valuesRef refField
        let ptrHandle = Types.Cast ptrHandle typeof<Type>
        let enumType = Memory.Read state ptrHandle |> TryTermToObj state
        match enumType with
        | Some (:? Type as enumType) ->
            assert enumType.IsEnum
            let names = Enum.GetNames enumType
            let values = Enum.GetValues enumType
            assert(values.Rank = 1)
            let valuesList = List<UInt64>()
            for v in values do
                valuesList.Add(TypeUtils.convert v typeof<UInt64> :?> UInt64)
            let values = Memory.ObjectToTerm state (valuesList.ToArray()) typeof<UInt64[]>
            let valuesCase state k =
                Memory.Write state valuesPtr values |> k
            let namesAndValuesCase state k =
                let names = Memory.ObjectToTerm state names typeof<string[]>
                let namesPtr = Memory.ReadField state namesRef refField
                let states = Memory.Write state valuesPtr values
                List.collect (fun state -> Memory.Write state namesPtr names) states |> k
            let needNames = (Types.Cast getNamesFlag typeof<int>) === (MakeNumber 1)
            StatedConditionalExecutionAppend state
                (fun state k -> k (needNames, state))
                namesAndValuesCase
                valuesCase
                (List.map (withFst <| Nop()))
        | _ -> internalfail $"GetEnumValuesAndNames: unexpected type {enumType}"

    let InternalBoxEnum (state : state) (args : term list) =
        assert(List.length args = 2)
        let typ, value = args[0], args[1]
        let t = Memory.TryTermToObject state typ |> Option.get :?> Type
        let enum = Types.Cast value t
        Memory.BoxValueType state enum

    let HasFlag (state : state) (args : term list) =
        assert(List.length args = 2)
        let thisRef, valueRef = args[0], args[1]
        let thisEnum = Memory.Read state thisRef
        let valueEnum = Memory.Read state valueRef
        let flags = PerformBinaryOperation OperationType.BitwiseAnd thisEnum valueEnum id
        flags === valueEnum
