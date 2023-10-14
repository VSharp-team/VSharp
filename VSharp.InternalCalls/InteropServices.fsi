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

    [<Implements("System.Runtime.InteropServices.GCHandle System.Runtime.InteropServices.GCHandle.Alloc(System.Object, System.Runtime.InteropServices.GCHandleType)")>]
    val AllocWithType : state -> term list -> term

    [<Implements("System.Runtime.InteropServices.GCHandle System.Runtime.InteropServices.GCHandle.Alloc(System.Object)")>]
    val Alloc : state -> term list -> term

    [<Implements("System.Void System.Runtime.InteropServices.GCHandle.Free(this)")>]
    val GCHandleFree : state -> term list -> term

    [<Implements("System.IntPtr System.Runtime.InteropServices.GCHandle.AddrOfPinnedObject(this)")>]
    val AddrOfPinnedObject : state -> term list -> term
