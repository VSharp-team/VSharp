namespace VSharp.System

open System
open global.System
open System.Reflection

open VSharp
open VSharp.Core
open VSharp.Interpreter.IL
open VSharp.Interpreter.IL.CilState

// ------------------------------ System.ReadOnlySpan --------------------------------

module internal ReadOnlySpan =

    let private isContentReferenceField fieldId =
        let name = fieldId.name
        name = "_pointer" || name = "_reference"

    let private isLengthField fieldId =
        fieldId.name = "_length"

    let private ptrFieldIsByRef (ptrFieldType : Type) =
        ptrFieldType.FullName.Contains("System.ByReference")

    let private readOnlySpanType = lazy typeof<int>.Assembly.GetType("System.ReadOnlySpan`1")
    let private createReadOnlySpanType (elemType : Type) =
        readOnlySpanType.Value.MakeGenericType elemType

    let private spanType = lazy typeof<int>.Assembly.GetType("System.Span`1")
    let private createSpanType (elemType : Type) =
        spanType.Value.MakeGenericType elemType

    let CommonGetLength (cilState : cilState) (spanStruct : term) =
        let spanFields = Terms.TypeOf spanStruct |> Reflection.fieldsOf false
        assert(Array.length spanFields = 2)
        let lenField = spanFields |> Array.find (fst >> isLengthField) |> fst
        cilState.ReadField spanStruct lenField

    let GetContentsRef (cilState : cilState) (spanStruct : term) =
        let spanFields = Terms.TypeOf spanStruct |> Reflection.fieldsOf false
        assert(Array.length spanFields = 2)
        let ptrField = spanFields |> Array.find (fst >> isContentReferenceField) |> fst
        let ptrFieldValue = cilState.ReadField spanStruct ptrField
        let ptrFieldType = ptrField.typ
        if ptrFieldIsByRef ptrFieldType then
            // Case for .NET 6, where Span contains 'System.ByReference' field
            let byRefFields = Terms.TypeOf ptrFieldValue |> Reflection.fieldsOf false
            assert(Array.length byRefFields = 1)
            let byRefField = byRefFields |> Array.find (fst >> ByReference.isValueField) |> fst
            cilState.ReadField ptrFieldValue byRefField
        else
            // Case for .NET 7, where Span contains 'Byte&' field
            ptrFieldValue

    let private IsArrayContents ref =
        match ref.term with
        | HeapRef(_, t)
        | Ptr(HeapLocation(_, t), _, _) ->
            TypeUtils.isArrayType t || t = typeof<string>
        | Ref(ArrayIndex _) -> true
        | _ -> false

    let GetItem (i : IInterpreter) (cilState : cilState) (args : term list) =
        assert(List.length args = 3)
        let this, wrappedType, index = args[0], args[1], args[2]
        let t = Helpers.unwrapType wrappedType
        let spanStruct = cilState.Read this
        let len = CommonGetLength cilState spanStruct
        let ref = GetContentsRef cilState spanStruct
        let checkIndex (cilState : cilState) k =
            let readIndex (cilState : cilState) k =
                let ref =
                    match ref.term with
                    | _ when IsArrayContents ref ->
                        Memory.ReferenceArrayIndex cilState.state ref [index] (Some t)
                    | _ when index = MakeNumber 0 ->
                        assert(TypeOfLocation ref = t)
                        ref
                    | Ref address ->
                        let pointerBase, offset = AddressToBaseAndOffset address
                        let size = TypeUtils.internalSizeOf t |> MakeNumber
                        let offset = Arithmetics.Add offset (Arithmetics.Mul index size)
                        Ptr pointerBase t offset
                    | Ptr(pointerBase, _, offset) ->
                        let size = TypeUtils.internalSizeOf t |> MakeNumber
                        let offset = Arithmetics.Add offset (Arithmetics.Mul index size)
                        Ptr pointerBase t offset
                    | _ -> internalfail $"GetItemFromReadOnlySpan: unexpected contents ref {ref}"
                cilState.Push ref
                List.singleton cilState |> k
            cilState.StatedConditionalExecutionCIL
                (fun state k -> k (Arithmetics.Less index len, state))
                readIndex
                (i.Raise i.IndexOutOfRangeException)
                k
        i.NpeOrInvoke cilState ref checkIndex id

    let GetLength (_ : IInterpreter) (cilState : cilState) (args : term list) =
        assert(List.length args = 2)
        let this = args[0]
        let spanStruct = cilState.Read this
        let length = CommonGetLength cilState spanStruct
        cilState.Push length
        List.singleton cilState

    let private PrepareRefField state ref : term =
        let cases = Memory.TryAddressFromRef state ref
        assert(List.length cases = 1)
        let address, _ = List.head cases
        match address with
        | Some address -> Ref address
        | None -> ref

    let private InitSpanStruct (cilState : cilState) spanStruct refToFirst length =
        let spanFields = Terms.TypeOf spanStruct |> Reflection.fieldsOf false
        assert(Array.length spanFields = 2)
        let refToFirst = PrepareRefField cilState.state refToFirst
        let ptrField, ptrFieldInfo = spanFields |> Array.find (fst >> isContentReferenceField)
        let ptrFieldType = ptrFieldInfo.FieldType
        let spanWithPtrField =
            if ptrFieldIsByRef ptrFieldType then
                // Case for .NET 6, where Span contains 'System.ByReference' field
                let byRef = Memory.DefaultOf ptrFieldType
                let byRefFields = Reflection.fieldsOf false ptrFieldType
                assert(Array.length byRefFields = 1)
                let valueField = byRefFields |> Array.find (fst >> ByReference.isValueField) |> fst
                let initializedByRef = cilState.WriteStructField byRef valueField refToFirst
                cilState.WriteStructField spanStruct ptrField initializedByRef
            else
                // Case for .NET 7, where Span contains 'Byte&' field
                cilState.WriteStructField spanStruct ptrField refToFirst
        let lengthField = spanFields |> Array.find (fst >> isLengthField) |> fst
        cilState.WriteStructField spanWithPtrField lengthField length

    let private CommonCtor (cilState : cilState) this refToFirst length =
        let span = cilState.Read this
        let initializedSpan = InitSpanStruct cilState span refToFirst length
        cilState.Write this initializedSpan
        List.singleton cilState

    let CtorFromPtr (i : IInterpreter) (cilState : cilState) (args : term list) : cilState list =
        assert(List.length args = 4)
        let this, wrappedType, ptr, size = args[0], args[1], args[2], args[3]
        let t = Helpers.unwrapType wrappedType
        let ctor (cilState : cilState) k =
            let state = cilState.state
            let ptr =
                if cilState.IsStackArray ptr then
                    // Ptr came from localloc instruction
                    Memory.AllocateVectorArray state size t
                elif MostConcreteTypeOfRef state ptr = t then ptr
                else Types.Cast ptr (t.MakePointerType())
            CommonCtor cilState this ptr size |> k
        cilState.StatedConditionalExecutionCIL
            (fun state k -> k (Arithmetics.GreaterOrEqual size (MakeNumber 0), state))
            ctor
            (i.Raise i.ArgumentOutOfRangeException)
            id

    let CtorFromArray (_ : IInterpreter) (cilState : cilState) (args : term list) : cilState list =
        assert(List.length args = 3)
        let this, arrayRef = args[0], args[2]
        let state = cilState.state
        let nullCase (cilState : cilState) k =
            let t = MostConcreteTypeOfRef cilState.state arrayRef
            let ref = NullRef t
            CommonCtor cilState this ref (MakeNumber 0) |> k
        let nonNullCase cilState k =
            let refToFirstElement = Memory.ReferenceArrayIndex state arrayRef [MakeNumber 0] None
            let lengthOfArray = Memory.ArrayLengthByDimension state arrayRef (MakeNumber 0)
            CommonCtor cilState this refToFirstElement lengthOfArray |> k
        cilState.StatedConditionalExecutionCIL
            (fun state k -> k (IsNullReference arrayRef, state))
            nullCase
            nonNullCase
            id

    let private CreateReadOnlySpan elemType =
        createReadOnlySpanType elemType
        |> Memory.DefaultOf

    let private CreateSpan elemType =
        createSpanType elemType
        |> Memory.DefaultOf

    let CreateFromString (_ : IInterpreter) (cilState : cilState) (args : term list) : cilState list =
        assert(List.length args = 1)
        let string = args[0]
        let span = CreateReadOnlySpan typeof<char>
        let state = cilState.state
        let length = Memory.StringLength state string
        let nullCase cilState k =
            let ref = NullRef typeof<char[]>
            let span = InitSpanStruct cilState span ref (MakeNumber 0)
            cilState.Push span
            List.singleton cilState |> k
        let nonNullCase cilState k =
            let refToFirst = Memory.ReferenceField state string Reflection.stringFirstCharField
            let span = InitSpanStruct cilState span refToFirst length
            cilState.Push span
            List.singleton cilState |> k
        cilState.StatedConditionalExecutionCIL
            (fun state k -> k (IsNullReference string, state))
            nullCase
            nonNullCase
            id

    let CreateSpanFromFieldHandle (_ : IInterpreter) (cilState : cilState) (args : term list) : cilState list =
        assert(List.length args = 2)
        let t = Helpers.unwrapType args[0]
        let fieldHandleTerm = args[1]
        let state = cilState.state
        match TryTermToObj state fieldHandleTerm with
        | Some (:? RuntimeFieldHandle as rfh) ->
            let fieldInfo = FieldInfo.GetFieldFromHandle rfh
            let rawData = Reflection.arrayFromField fieldInfo t
            let arrayType = t.MakeArrayType()
            let arrayRef = Memory.AllocateConcreteObject state rawData arrayType
            let refToFirstElement = Memory.ReferenceArrayIndex state arrayRef [MakeNumber 0] None
            let lengthOfArray = MakeNumber rawData.Length
            let span = CreateReadOnlySpan t
            let span = InitSpanStruct cilState span refToFirstElement lengthOfArray
            cilState.Push span
            List.singleton cilState
        | _ -> internalfail $"CreateSpanFromFieldHandle: unexpected field handle {fieldHandleTerm}"

    let GetSpanDataFrom (_ : IInterpreter) (cilState : cilState) (args : term list) : cilState list =
        assert(List.length args = 3)
        let fieldHandleTerm, typeHandleTerm, countRef = args[0], args[1], args[2]
        let state = cilState.state
        match TryTermToObj state fieldHandleTerm, TryTermToObj state typeHandleTerm with
        | Some (:? RuntimeFieldHandle as rfh), Some (:? RuntimeTypeHandle as rth) ->
            let fieldInfo = FieldInfo.GetFieldFromHandle rfh
            let t = global.System.Type.GetTypeFromHandle rth
            let rawData = Reflection.arrayFromField fieldInfo t
            let arrayType = t.MakeArrayType()
            let arrayRef = Memory.AllocateConcreteObject state rawData arrayType
            let refToFirstElement = Memory.ReferenceArrayIndex state arrayRef [MakeNumber 0] None
            let count = MakeNumber rawData.Length
            cilState.Write countRef count
            cilState.Push refToFirstElement
            List.singleton cilState
        | _ -> internalfail $"GetSpanDataFrom: unexpected field handle {fieldHandleTerm} or type handle {typeHandleTerm}"
