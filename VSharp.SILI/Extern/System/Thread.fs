namespace VSharp.System

open global.System
open VSharp
open VSharp.Core

// ------------------------------ mscorlib.System.Threading.Thread --------------------------------

module Thread =

    let internal GetFastDomainInternal (state : state) (_ : term list) = Return <| Class Heap.empty, state

    let internal GetDomainInternal = GetFastDomainInternal
