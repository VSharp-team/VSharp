namespace VSharp.System

open global.System
open VSharp
open VSharp.Core

// ------------------------------ ChessDotNet --------------------------------

module ChessDotNet =

    let internal PositionEquals (state : state) (args : term list) : term * state =
        assert(List.length args = 2)
        let this, another = List.item 0 args, List.item 1 args
        EqualityComparer.equalsForTwoFieldsBlock state this another, state
