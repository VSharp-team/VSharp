namespace VSharp.System

open VSharp
open VSharp.Core

module internal ValueType =

    let Equals (state : state) (args : term list) =
        assert(List.length args = 2)
        EqualityComparer.structuralEquality state args[0] args[1]
