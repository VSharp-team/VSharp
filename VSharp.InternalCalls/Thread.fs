namespace VSharp.System

open global.System
open VSharp
open VSharp.Core

// ------------------------------ mscorlib.System.Threading.Thread --------------------------------

module Thread =

    let internal GetFastDomainInternal (state : state) (_ : term list) = (MakeNullRef (Types.FromDotNetType state typeof<System.AppDomain>), state)

    let internal GetDomainInternal state args = GetFastDomainInternal state args
