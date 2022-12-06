namespace VSharp.System

open VSharp
open VSharp.Core

module internal InteropServices =

    [<Implements("T& System.Runtime.InteropServices.MemoryMarshal.GetArrayDataReference(T[])")>]
    val internal GetArrayDataReference : state -> term list -> term
