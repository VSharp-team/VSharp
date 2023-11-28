namespace VSharp.System

open VSharp
open VSharp.Core
open VSharp.Interpreter.IL

module internal InteropServices =

    [<Implements("T& System.Runtime.InteropServices.MemoryMarshal.GetArrayDataReference(T[])")>]
    val GetArrayDataReference : state -> term list -> term

    [<Implements("System.Void System.Runtime.InteropServices.NativeMemory.Free(System.Void*)")>]
    val Free : state -> term list -> term

    [<Implements("System.Void* System.Runtime.InteropServices.NativeMemory.AlignedAlloc(System.UIntPtr, System.UIntPtr)")>]
    val AlignedAlloc : state -> term list -> term

    [<Implements("System.Runtime.InteropServices.GCHandle System.Runtime.InteropServices.GCHandle.Alloc(System.Object, System.Runtime.InteropServices.GCHandleType)")>]
    val GCHandleAllocWithType : state -> term list -> term

    [<Implements("System.Boolean System.Runtime.InteropServices.GCHandle.IsPinned(System.IntPtr)")>]
    val GCHandleIsPinned : state -> term list -> term

    [<Implements("System.IntPtr System.Runtime.InteropServices.GCHandle.GetHandleValue(System.IntPtr)")>]
    val GCHandleGetHandleValue : state -> term list -> term

    [<Implements("System.Runtime.InteropServices.GCHandle System.Runtime.InteropServices.GCHandle.Alloc(System.Object)")>]
    val GCHandleAlloc : state -> term list -> term

    [<Implements("System.Object System.Runtime.InteropServices.GCHandle.InternalGet(System.IntPtr)")>]
    val GCHandleInternalGet : IInterpreter -> cilState -> term list -> cilState list

    [<Implements("System.Void System.Runtime.InteropServices.GCHandle.Free(this)")>]
    val GCHandleFree : state -> term list -> term

    [<Implements("System.IntPtr System.Runtime.InteropServices.GCHandle.AddrOfPinnedObject(this)")>]
    val AddrOfPinnedObject : state -> term list -> term

    [<Implements("System.IntPtr System.RuntimeTypeHandle.GetGCHandle(this, System.Runtime.InteropServices.GCHandleType)")>]
    val TypeHandleGetGCHandle : state -> term list -> term

    [<Implements("System.Void System.Runtime.InteropServices.Marshal.SetLastPInvokeError(System.Int32)")>]
    val SetLastPInvokeError : state -> term list -> term

    [<Implements("System.Int32 System.Runtime.InteropServices.Marshal.GetLastPInvokeError()")>]
    val GetLastPInvokeError : state -> term list -> term

    [<Implements("System.Void System.Runtime.InteropServices.Marshal.SetLastSystemError(System.Int32)")>]
    val SetLastSystemError : state -> term list -> term

    [<Implements("System.Int32 System.Runtime.InteropServices.Marshal.GetLastSystemError()")>]
    val GetLastSystemError : state -> term list -> term
