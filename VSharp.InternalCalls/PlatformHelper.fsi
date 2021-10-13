namespace VSharp.System

open global.System
open VSharp
open VSharp.Core

module internal PlatformHelper =

    [<Implements("System.Int32 System.Threading.PlatformHelper.get_ProcessorCount()")>]
    val internal get_ProcessorCount : state -> term list -> term
