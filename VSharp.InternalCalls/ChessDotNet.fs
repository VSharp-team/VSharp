namespace VSharp.System

open global.System
open VSharp
open VSharp.Core

// ------------------------------ ChessDotNet --------------------------------

module ChessDotNet =

    // TODO: now this works only for Span, made from String #do
    let internal Equals (state : state) (args : term list) : term * state =
        assert(List.length args = 2)
        let this, another = List.item 0 args, List.item 1 args
        let thisType = Terms.MostConcreteTypeOfHeapRef state this
        let anotherType = Terms.MostConcreteTypeOfHeapRef state another
        let checkContents () =
            let thisFields = Types.ToDotNetType thisType |> Reflection.fieldsOf false
            assert(Array.length thisFields = 2)
            let anotherFields = Types.ToDotNetType anotherType |> Reflection.fieldsOf false
            assert(Array.length anotherFields = 2)
            let thisFstField = fst thisFields.[0] |> Memory.ReadField state this
            let thisSndField = fst thisFields.[1] |> Memory.ReadField state this
            let anotherFstField = fst anotherFields.[0] |> Memory.ReadField state another
            let anotherSndField = fst anotherFields.[1] |> Memory.ReadField state another
            thisFstField === anotherFstField &&& thisSndField === anotherSndField, state
        if thisType <> anotherType then False, state // TODO: make this check symbolic #do
        else checkContents ()
