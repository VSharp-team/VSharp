namespace VSharp.System

open global.System
open VSharp
open VSharp.Core
open VSharp.Interpreter.IL

module internal Interlocked =

    [<Implements("T System.Threading.Interlocked.CompareExchange(T&, T, T)")>]
    val genericCompareExchange : IInterpreter -> cilState -> term list -> cilState list

    [<Implements("System.Int32 System.Threading.Interlocked.CompareExchange(System.Int32&, System.Int32, System.Int32)")>]
    [<Implements("System.Int64 System.Threading.Interlocked.CompareExchange(System.Int64&, System.Int64, System.Int64)")>]
    [<Implements("System.Object System.Threading.Interlocked.CompareExchange(System.Object&, System.Object, System.Object)")>]
    [<Implements("System.IntPtr System.Threading.Interlocked.CompareExchange(System.IntPtr&, System.IntPtr, System.IntPtr)")>]
    [<Implements("System.Object System.Runtime.InteropServices.GCHandle.InternalCompareExchange(System.IntPtr, System.Object, System.Object)")>]
    val compareExchange : IInterpreter -> cilState -> term list -> cilState list

    [<Implements("T System.Threading.Interlocked.Exchange(T&, T)")>]
    val genericExchange : IInterpreter -> cilState -> term list -> cilState list

    [<Implements("System.IntPtr System.Threading.Interlocked.Exchange(System.IntPtr&, System.IntPtr)")>]
    val intPtrExchange : IInterpreter -> cilState -> term list -> cilState list

    [<Implements("System.Int32 System.Threading.Interlocked.Exchange(System.Int32&, System.Int32)")>]
    val intExchange : IInterpreter -> cilState -> term list -> cilState list

    [<Implements("System.Int32 System.Threading.Interlocked.ExchangeAdd(System.Int32&, System.Int32)")>]
    val intExchangeAdd : IInterpreter -> cilState -> term list -> cilState list

    [<Implements("System.Int64 System.Threading.Interlocked.ExchangeAdd(System.Int64&, System.Int64)")>]
    val longExchangeAdd : IInterpreter -> cilState -> term list -> cilState list

    [<Implements("System.Void System.Threading.Interlocked.MemoryBarrier()")>]
    val memoryBarrier : state -> term list -> term
