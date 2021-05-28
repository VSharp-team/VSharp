namespace VSharp.System

open global.System
open VSharp
open VSharp.Core

// ------------------------------ System.ReadOnlySpan --------------------------------

module ReadOnlySpan =

    let internal GetItemFromReadOnlySpan (state : state) (args : term list) : term * state =
        assert(List.length args = 3)
        let this, index = List.item 0 args, List.item 2 args
        let span = Memory.ReadSafe state this
        let spanFields = Terms.TypeOf span |> Types.ToDotNetType |> Reflection.fieldsOf false
        assert(Array.length spanFields = 2)
        let ptrField = fst spanFields.[0]
        // TODO: throw ThrowIndexOutOfRangeException if len is less or equal to index
        // let lenField = fst spanFields.[1]
        // let len = Memory.ReadField state span lenField
        let byRefStruct = Memory.ReadField state span ptrField
        let byRefFields = Terms.TypeOf byRefStruct |> Types.ToDotNetType |> Reflection.fieldsOf false
        assert(Array.length byRefFields = 1)
        let byRefField = fst byRefFields.[0]
        let ptrToArray = Memory.ReadField state byRefStruct byRefField
        let ref =
            match ptrToArray.term with
            // Case for char span made from string
            | Ptr(Some(ClassField(addr, field)), _, None) when field = Reflection.stringFirstCharField ->
                HeapRef addr (ArrayType(Types.FromDotNetType typeof<char>, Vector))
            | Ptr(Some(ArrayIndex(addr, indices, (eType, dim, isVector))), _, None) when indices = [MakeNumber 0] ->
                let dim = if isVector then Vector else ConcreteDimension dim
                HeapRef addr (ArrayType(eType, dim))
            | _ -> __notImplemented__()
        Memory.ReferenceArrayIndex ref [index], state

    let internal GetItemFromSpan (state : state) (args : term list) : term * state =
        GetItemFromReadOnlySpan state args

    let internal CtorFromPtrForSpan (state : state) (args : term list) : (term * state) list =
        assert(List.length args = 4)
        let this, wrappedType, ptr, size = args.[0], args.[1], args.[2], args.[3]
        // Check, that this ptr came from localloc instruction
        assert(ptr = Ptr None Void None)
        let elementType =
            match wrappedType.term with
            | Concrete(:? Type as t, _) -> t
            | _ -> __unreachable__()
        let span = Memory.ReadSafe state this
        let spanFields = Terms.TypeOf span |> Types.ToDotNetType |> Reflection.fieldsOf false
        assert(Array.length spanFields = 2)
        let ptrField, ptrFieldType = spanFields.[0]
        let byRefType = Types.FromDotNetType ptrFieldType
        let arrayRef, state = Memory.AllocateVectorArray state size (Types.FromDotNetType elementType)
        let refToFirstElement = Memory.ReferenceArrayIndex arrayRef [MakeNumber 0]
        let ptrToFirstElement = Types.CastReferenceToPointer state refToFirstElement
        let byRef = Memory.DefaultOf byRefType
        let byRefFields = Reflection.fieldsOf false ptrFieldType
        assert(Array.length byRefFields = 1)
        let initializedByRef = Memory.WriteStructField byRef (fst byRefFields.[0]) ptrToFirstElement
        let spanWithPtrField = Memory.WriteStructField span ptrField initializedByRef
        let initializedSpan = Memory.WriteStructField spanWithPtrField (fst spanFields.[1]) size
        Memory.WriteSafe state this initializedSpan |> List.map (withFst Nop)

    let internal CtorFromPtrForReadOnlySpan (state : state) (args : term list) : (term * state) list =
        CtorFromPtrForSpan state args

    let internal CtorFromArrayForReadOnlySpan (state : state) (args : term list) : (term * state) list = // TODO: unify #do
        assert(List.length args = 3)
        let this, arrayRef = args.[0], args.[2]
        // Check, that this ptr came from localloc instruction
        let span = Memory.ReadSafe state this
        let spanFields = Terms.TypeOf span |> Types.ToDotNetType |> Reflection.fieldsOf false
        assert(Array.length spanFields = 2)
        let ptrField, ptrFieldType = spanFields.[0]
        let byRefType = Types.FromDotNetType ptrFieldType
        let refToFirstElement = Memory.ReferenceArrayIndex arrayRef [MakeNumber 0]
        let ptrToFirstElement = Types.CastReferenceToPointer state refToFirstElement
        let byRef = Memory.DefaultOf byRefType
        let byRefFields = Reflection.fieldsOf false ptrFieldType
        assert(Array.length byRefFields = 1)
        let initializedByRef = Memory.WriteStructField byRef (fst byRefFields.[0]) ptrToFirstElement
        let spanWithPtrField = Memory.WriteStructField span ptrField initializedByRef
        let lengthOfArray = Memory.ArrayLengthByDimension state arrayRef (MakeNumber 0)
        let initializedSpan = Memory.WriteStructField spanWithPtrField (fst spanFields.[1]) lengthOfArray
        Memory.WriteSafe state this initializedSpan |> List.map (withFst Nop)
