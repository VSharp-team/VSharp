namespace VSharp.System

open global.System
open VSharp
open VSharp.Core

// ------------------------------ System.ByReference --------------------------------

module ByReference =

    let internal ctor (state : state) (args : term list) : (term * state) list =
        assert(List.length args = 3)
        let this, ref = List.item 0 args, List.item 2 args
        let ptr = Types.CastReferenceToPointer state ref
        let fields = Terms.TypeOf this |> Types.ToDotNetType |> Reflection.fieldsOf false
        assert(Array.length fields = 1)
        let field = Array.head fields |> fst
        let fieldRef = Memory.ReferenceField state this field
        Memory.WriteSafe state fieldRef ptr |> List.map (withFst Nop)
