namespace VSharp.System

open global.System
open VSharp.Core

module internal PlatformHelper =

    let get_ProcessorCount (_ : state) (_ : term list) : term =
        MakeNumber 1

    let lzcntIsSupported (_ : state) (_ : term list) : term =
        MakeBool false

    let armBaseIsSupported (_ : state) (_ : term list) : term =
        MakeBool false

    let avx2IsSupported (_ : state) (_ : term list) : term =
        MakeBool false

    let sse2IsSupported (_ : state) (_ : term list) : term =
        MakeBool false

    let x86BaseIsSupported (_ : state) (_ : term list) : term =
        MakeBool false

    let vector128IsHardwareAccelerated (_ : state) (_ : term list) : term =
        MakeBool false

    let quicListenerIsSupported (_ : state) (_ : term list) : term =
        MakeBool false
