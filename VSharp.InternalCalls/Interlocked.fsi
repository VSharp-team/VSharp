namespace VSharp.System

open global.System
open VSharp
open VSharp.Core

module internal Interlocked =

    [<Implements("T System.Threading.Interlocked.CompareExchange(T&, T, T)")>]
    val internal genericCompareExchange : state -> term list -> (term * state) list

    [<Implements("System.Int32 System.Threading.Interlocked.CompareExchange(System.Int32&, System.Int32, System.Int32)")>]
    val internal intCompareExchange : state -> term list -> (term * state) list

    [<Implements("System.IntPtr System.Threading.Interlocked.CompareExchange(System.IntPtr&, System.IntPtr, System.IntPtr)")>]
    [<Implements("System.Object System.Runtime.InteropServices.GCHandle.InternalCompareExchange(System.IntPtr, System.Object, System.Object)")>]
    val internal intPtrCompareExchange : state -> term list -> (term * state) list

    [<Implements("T System.Threading.Interlocked.Exchange(T&, T)")>]
    val internal genericExchange : state -> term list -> (term * state) list

    [<Implements("System.Int32 System.Threading.Interlocked.Exchange(System.Int32&, System.Int32)")>]
    val internal intExchange : state -> term list -> (term * state) list

    [<Implements("System.Int32 System.Threading.Interlocked.ExchangeAdd(System.Int32&, System.Int32)")>]
    val internal intExchangeAdd : state -> term list -> (term * state) list

    [<Implements("System.Int64 System.Threading.Interlocked.ExchangeAdd(System.Int64&, System.Int64)")>]
    val internal longExchangeAdd : state -> term list -> (term * state) list

    [<Implements("System.Void System.Threading.Interlocked.MemoryBarrier()")>]
    val internal memoryBarrier : state -> term list -> term
