namespace VSharp.System

open global.System
open VSharp
open VSharp.Core

// ------------------------------ System.ByReference --------------------------------

module ByReference =

    let private referenceValueField state this =
        let fields = Terms.TypeOfLocation this |> Reflection.fieldsOf false
        assert(Array.length fields = 1)
        let field = Array.head fields |> fst
        Memory.ReferenceField state this field

    let internal ctor (state : state) (args : term list) : (term * state) list =
        assert(List.length args = 3)
        let this, ref = List.item 0 args, List.item 2 args
        let fieldRef = referenceValueField state this
        Memory.Write state fieldRef ref |> List.map (withFst (Nop()))

    let internal getValue (state : state) (args : term list) : term =
        assert(List.length args = 2)
        let this = List.item 0 args
        let ref = referenceValueField state this
        Memory.Read state ref
