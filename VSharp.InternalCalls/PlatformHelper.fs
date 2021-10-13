namespace VSharp.System

open global.System
open VSharp.Core

module internal PlatformHelper =

    let get_ProcessorCount (_ : state) (_ : term list) : term =
        MakeNumber 1
