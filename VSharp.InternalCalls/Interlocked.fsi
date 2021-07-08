namespace VSharp.System

open global.System
open VSharp
open VSharp.Core

module internal Interlocked =

    [<Implements("T System.Threading.Interlocked.CompareExchange(T&, T, T)")>]
    val internal compareExchange : state -> term list -> (term * state) list
