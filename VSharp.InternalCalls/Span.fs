namespace VSharp.System

open global.System
open VSharp
open VSharp.Core

// ------------------------------ System.ReadOnlySpan --------------------------------

module ReadOnlySpan =

    // TODO: now this works only for Span, made from String #do
    let internal get_Item (state : state) (args : term list) : term * state =
        assert(List.length args = 2)
        let this, index = List.item 0 args, List.item 1 args
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
        let ptrToStringArray = Memory.ReadField state byRefStruct byRefField
        match ptrToStringArray.term with
        | Ptr(Some(ClassField(addr, field)), _, None) when field = Reflection.stringFirstCharField ->
            let ref = HeapRef addr (ArrayType(Types.FromDotNetType typeof<char>, Vector))
            Memory.ReferenceArrayIndex ref [index], state
        | _ -> __insufficientInformation__ "now Span works only for String"
