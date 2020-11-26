namespace VSharp.System

open global.System
open VSharp
open VSharp.Core

// ------------------------------ mscorlib.System.Threading.Thread --------------------------------

module Thread =

    let internal GetFastDomainInternal (state : state) (_ : term list) = (NullRef, state)

    let internal GetDomainInternal state args = GetFastDomainInternal state args
