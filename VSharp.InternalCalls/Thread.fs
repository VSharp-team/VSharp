namespace VSharp.System

open global.System
open VSharp.Core

// ------------------------------ mscorlib.System.Threading.Thread --------------------------------

module Thread =

    let internal GetFastDomainInternal (_ : state) (_ : term list) = NullRef typeof<AppDomain>

    let internal GetDomainInternal state args = GetFastDomainInternal state args
