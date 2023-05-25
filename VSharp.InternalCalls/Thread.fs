namespace VSharp.System

open global.System
open VSharp.Core

// ------------------------------ mscorlib.System.Threading.Thread --------------------------------

module Thread =

    let internal GetFastDomainInternal (_ : state) (_ : term list) = NullRef typeof<AppDomain>

    let internal GetDomainInternal state args = GetFastDomainInternal state args

    let internal SpinWaitInternal (_ : state) (args : term list) =
        assert(List.length args = 1)
        Nop

    let internal SpinOnce (_ : state) (args : term list) =
        assert(List.length args = 1)
        Nop

    let internal Yield (_ : state) (args : term list) =
        assert(List.length args = 0)
        MakeBool true

    let internal SleepInternal (_ : state) (args : term list) =
        assert(List.length args = 1)
        Nop
