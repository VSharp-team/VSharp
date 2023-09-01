namespace VSharp.System

open System
open global.System
open VSharp
open VSharp.Core

// ------------------------------ System.ReadOnlySpan --------------------------------

module ReadOnlySpan =

    let private isContentReferenceField fieldId =
        let name = fieldId.name
        name = "_pointer" || name = "_reference"

    let private isLengthField fieldId =
        fieldId.name = "_length"

    let ptrFieldIsByRef (ptrFieldType : Type) =
        ptrFieldType.FullName.Contains("System.ByReference")

    let readOnlySpanType() =
        typeof<int>.Assembly.GetType("System.ReadOnlySpan`1")

    let private CreateArrayRef address indices (eType : Type, dim, isVector as arrayType) =
        if isVector && indices = [MakeNumber 0] then
            HeapRef address (eType.MakeArrayType())
        elif indices = List.init dim (fun _ -> MakeNumber 0) then
            HeapRef address (eType.MakeArrayType dim)
        else ArrayIndex(address, indices, arrayType) |> Ref

    let internal GetLength (state : state) (spanStruct : term) =
        let spanFields = Terms.TypeOf spanStruct |> Reflection.fieldsOf false
        assert(Array.length spanFields = 2)
        let lenField = spanFields |> Array.find (fst >> isLengthField) |> fst
        Memory.ReadField state spanStruct lenField

    let internal GetContentsRef (state : state) (spanStruct : term) =
        let spanFields = Terms.TypeOf spanStruct |> Reflection.fieldsOf false
        assert(Array.length spanFields = 2)
        let ptrField = spanFields |> Array.find (fst >> isContentReferenceField) |> fst
        // TODO: throw ThrowIndexOutOfRangeException if len is less or equal to index
        let lenField = spanFields |> Array.find (fst >> isLengthField) |> fst
        let len = Memory.ReadField state spanStruct lenField
        let ptrFieldValue = Memory.ReadField state spanStruct ptrField
        let ptrFieldType = ptrField.typ
        let ptr =
            if ptrFieldIsByRef ptrFieldType then
                // Case for .NET 6, where Span contains 'System.ByReference' field
                let byRefFields = Terms.TypeOf ptrFieldValue |> Reflection.fieldsOf false
                assert(Array.length byRefFields = 1)
                let byRefField = byRefFields |> Array.find (fst >> ByReference.isValueField) |> fst
                Memory.ReadField state ptrFieldValue byRefField
            else
                // Case for .NET 7, where Span contains 'Byte&' field
                ptrFieldValue
        match ptr.term with
        // Case for char span made from string
        | Ref(ClassField(address, field)) when field = Reflection.stringFirstCharField ->
            HeapRef address typeof<char[]>
        | Ref(ArrayIndex(addr, indices, arrayType)) when indices = [MakeNumber 0] ->
            CreateArrayRef addr indices arrayType
        | Ptr(HeapLocation(address, t), sightType, offset) ->
            match TryPtrToArrayInfo t sightType offset with
            | Some(indices, arrayType) -> CreateArrayRef address indices arrayType
            | None when t.IsSZArray || t = typeof<string> -> ptr
            | None -> internalfail $"GetContentsRef: unexpected pointer to contents {ptr}"
        | Ptr _ when len = MakeNumber 1 -> ptr
        | _ -> internalfail $"GetContentsRef: unexpected reference to contents {ptr}"

    let internal GetItemFromReadOnlySpan (state : state) (args : term list) : term =
        assert(List.length args = 3)
        let this, wrappedType, index = args[0], args[1], args[2]
        let t = Helpers.unwrapType wrappedType
        let span = Memory.Read state this
        let ref = GetContentsRef state span
        let isArrayContents =
            match ref.term with
            | HeapRef(_, t)
            | Ptr(HeapLocation(_, t), _, _) ->
                TypeUtils.isArrayType t || t = typeof<string>
            | Ref(ArrayIndex _) -> true
            | _ -> false
        if isArrayContents then
            Memory.ReferenceArrayIndex state ref [index] (Some t)
        elif index = MakeNumber 0 then
            assert(TypeOfLocation ref = t)
            ref
        else internalfail $"GetItemFromReadOnlySpan: unexpected contents ref {ref}"

    let internal GetItemFromSpan (state : state) (args : term list) : term =
        GetItemFromReadOnlySpan state args

    let private InitSpanStruct spanStruct refToFirst length =
        let spanFields = Terms.TypeOf spanStruct |> Reflection.fieldsOf false
        assert(Array.length spanFields = 2)
        let ptrField, ptrFieldInfo = spanFields |> Array.find (fst >> isContentReferenceField)
        let ptrFieldType = ptrFieldInfo.FieldType
        let spanWithPtrField =
            if ptrFieldIsByRef ptrFieldType then
                // Case for .NET 6, where Span contains 'System.ByReference' field
                let byRef = Memory.DefaultOf ptrFieldType
                let byRefFields = Reflection.fieldsOf false ptrFieldType
                assert(Array.length byRefFields = 1)
                let valueField = byRefFields |> Array.find (fst >> ByReference.isValueField) |> fst
                let initializedByRef = Memory.WriteStructField byRef valueField refToFirst
                Memory.WriteStructField spanStruct ptrField initializedByRef
            else
                // Case for .NET 7, where Span contains 'Byte&' field
                Memory.WriteStructField spanStruct ptrField refToFirst
        let lengthField = spanFields |> Array.find (fst >> isLengthField) |> fst
        Memory.WriteStructField spanWithPtrField lengthField length

    let private CommonCtor (state : state) this refToFirst length =
        let span = Memory.Read state this
        let initializedSpan = InitSpanStruct span refToFirst length
        Memory.Write state this initializedSpan |> List.map (withFst (Nop()))

    let internal CtorFromFromArray (state : state) this arrayRef =
        let refToFirstElement = Memory.ReferenceArrayIndex state arrayRef [MakeNumber 0] None
        let lengthOfArray = Memory.ArrayLengthByDimension state arrayRef (MakeNumber 0)
        CommonCtor state this refToFirstElement lengthOfArray

    let internal CtorFromPtrForSpan (state : state) (args : term list) : (term * state) list =
        assert(List.length args = 4)
        let this, wrappedType, ptr, size = args[0], args[1], args[2], args[3]
        if ptr = MakeNullPtr typeof<Void> then
            // Ptr came from localloc instruction
            let elementType = Helpers.unwrapType wrappedType
            let arrayRef = Memory.AllocateVectorArray state size elementType
            CtorFromFromArray state this arrayRef
        else
            CommonCtor state this ptr size

    let internal CtorFromPtrForReadOnlySpan (state : state) (args : term list) : (term * state) list =
        CtorFromPtrForSpan state args

    let internal CtorFromArrayForReadOnlySpan (state : state) (args : term list) : (term * state) list =
        assert(List.length args = 3)
        let this, arrayRef = args[0], args[2]
        CtorFromFromArray state this arrayRef

    let ReadOnlySpanCreateFromString (state : state) (args : term list) : term =
        assert(List.length args = 1)
        let string = args[0]
        let spanType = readOnlySpanType().MakeGenericType(typeof<char>)
        let span = Memory.DefaultOf spanType
        let arrayType = typeof<char>, 1, true
        let refToFirst =
            match string.term with
            | HeapRef(address, t) when t = typeof<string> ->
                Ref (ArrayIndex(address, [MakeNumber 0], arrayType))
            | _ -> internalfail $"Span.CreateFromString: unexpected string ref {string}"
        let length = Memory.StringLength state string
        InitSpanStruct span refToFirst length
