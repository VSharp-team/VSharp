namespace VSharp.System

open VSharp
open VSharp.Core

module internal InteropServices =

    [<Implements("T& System.Runtime.InteropServices.MemoryMarshal.GetArrayDataReference(T[])")>]
    val GetArrayDataReference : state -> term list -> term

    [<Implements("System.Void System.Runtime.InteropServices.NativeMemory.Free(System.Void*)")>]
    val Free : state -> term list -> term

    [<Implements("System.Void* System.Runtime.InteropServices.NativeMemory.AlignedAlloc(System.UIntPtr, System.UIntPtr)")>]
    val AlignedAlloc : state -> term list -> term
