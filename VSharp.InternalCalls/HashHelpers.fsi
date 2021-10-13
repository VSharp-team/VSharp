namespace VSharp.System

open global.System
open VSharp
open VSharp.Core

module HashHelpers =

    [<Implements("System.UInt32 System.Collections.HashHelpers.FastMod(System.UInt32, System.UInt32, System.UInt64)")>]
    val FastMod : state -> term list -> term
