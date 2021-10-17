namespace VSharp.System

open global.System
open VSharp
open VSharp.Core

// ------------------------------ mscorlib.System.Threading.Volatile --------------------------------

module Volatile =

    let Read (state : state) (args : term list) : term =
        assert(List.length args = 2)
        let ref = List.item 1 args
        Memory.ReadSafe state ref
