namespace VSharp.System

open System
open global.System
open VSharp
open VSharp.Core

// ------------------------------ System.ReadOnlySpan --------------------------------

module ReadOnlySpan =

    let internal GetContentsRef (state : state) (spanStruct : term) =
        let spanFields = Terms.TypeOf spanStruct |> Reflection.fieldsOf false
        assert(Array.length spanFields = 2)
        let ptrField = spanFields |> Array.find (fun (fieldId, _) -> fieldId.name = "_pointer") |> fst
        // TODO: throw ThrowIndexOutOfRangeException if len is less or equal to index
        // let lenField = spanFields |> Array.find (fun (fieldId, _) -> fieldId.name = "_length") |> fst
        // let len = Memory.ReadField state span lenField
        let byRefStruct = Memory.ReadField state spanStruct ptrField
        let byRefFields = Terms.TypeOf byRefStruct |> Reflection.fieldsOf false
        assert(Array.length byRefFields = 1)
        let byRefField = byRefFields |> Array.find (fun (fieldId, _) -> fieldId.name = "_value") |> fst
        let ptrToArray = Memory.ReadField state byRefStruct byRefField
        match ptrToArray.term with
        // Case for char span made from string
        | Ref(ClassField(address, field)) when field = Reflection.stringFirstCharField ->
            HeapRef address typeof<char[]>
        | Ref(ArrayIndex(addr, indices, (eType, dim, isVector))) when indices = [MakeNumber 0] ->
            let t = if isVector then eType.MakeArrayType() else eType.MakeArrayType dim
            HeapRef addr t
        | Ptr(HeapLocation(address, t), sightType, offset) ->
            match TryPtrToArrayInfo t sightType offset with
            | Some(index, arrayType) ->
                ArrayIndex(address, index, arrayType) |> Ref
            | None when t.IsSZArray || t = typeof<string> -> ptrToArray
            | None -> internalfail $"GetContentsRef: unexpected pointer to contents {ptrToArray}"
        | _ -> internalfail $"GetContentsRef: unexpected reference to contents {ptrToArray}"

    let internal GetItemFromReadOnlySpan (state : state) (args : term list) : term =
        assert(List.length args = 3)
        let this, index = List.item 0 args, List.item 2 args
        let span = Memory.Read state this
        let ref = GetContentsRef state span
        Memory.ReferenceArrayIndex state ref [index] None

    let internal GetItemFromSpan (state : state) (args : term list) : term =
        GetItemFromReadOnlySpan state args

    let private CommonCtor (state : state) this refToFirst length =
        let span = Memory.Read state this
        let spanFields = Terms.TypeOf span |> Reflection.fieldsOf false
        assert(Array.length spanFields = 2)
        let ptrField, ptrFieldInfo = spanFields |> Array.find (fun (fieldId, _) -> fieldId.name = "_pointer")
        let ptrFieldType = ptrFieldInfo.FieldType
        let byRef = Memory.DefaultOf ptrFieldType
        let byRefFields = Reflection.fieldsOf false ptrFieldType
        assert(Array.length byRefFields = 1)
        let valueField = byRefFields |> Array.find (fun (fieldId, _) -> fieldId.name = "_value") |> fst
        let initializedByRef = Memory.WriteStructField byRef valueField refToFirst
        let spanWithPtrField = Memory.WriteStructField span ptrField initializedByRef
        let lengthField = spanFields |> Array.find (fun (fieldId, _) -> fieldId.name = "_length") |> fst
        let initializedSpan = Memory.WriteStructField spanWithPtrField lengthField length
        Memory.Write state this initializedSpan |> List.map (withFst Nop)

    let internal CtorFromFromArray (state : state) this arrayRef =
        let refToFirstElement = Memory.ReferenceArrayIndex state arrayRef [MakeNumber 0] None
        let lengthOfArray = Memory.ArrayLengthByDimension state arrayRef (MakeNumber 0)
        CommonCtor state this refToFirstElement lengthOfArray

    let internal CtorFromPtrForSpan (state : state) (args : term list) : (term * state) list =
        assert(List.length args = 4)
        let this, wrappedType, ptr, size = args.[0], args.[1], args.[2], args.[3]
        if ptr = MakeNullPtr typeof<Void> then
            // Ptr came from localloc instruction
            let elementType =
                match wrappedType.term with
                | Concrete(:? Type as t, _) -> t
                | _ -> __unreachable__()
            let arrayRef = Memory.AllocateVectorArray state size elementType
            CtorFromFromArray state this arrayRef
        else
            CommonCtor state this ptr size

    let internal CtorFromPtrForReadOnlySpan (state : state) (args : term list) : (term * state) list =
        CtorFromPtrForSpan state args

    let internal CtorFromArrayForReadOnlySpan (state : state) (args : term list) : (term * state) list =
        assert(List.length args = 3)
        let this, arrayRef = args.[0], args.[2]
        CtorFromFromArray state this arrayRef
