namespace VSharp.System

open global.System
open VSharp
open VSharp.Core

// ------------------------------ mscorlib.System.Threading.Thread --------------------------------

module Thread =

    let internal GetFastDomainInternal (state : state) (args : term list) = Return <| Struct Heap.empty (Types.FromDotNetType state typedefof<AppDomain>), state

    let internal GetDomainInternal = GetFastDomainInternal
