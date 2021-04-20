namespace VSharp.System

open global.System
open VSharp
open VSharp.Core

module internal IntPtr =

    val private ctor : state -> term -> term -> (term * state) list

    [<Implements("System.Void System.IntPtr..ctor(this, System.Int32)")>]
    val internal ctorFromInt : state -> term list -> (term * state) list

    [<Implements("System.Void System.IntPtr..ctor(this, System.Void*)")>]
    val internal ctorFromPtr : state -> term list -> (term * state) list

    [<Implements("System.Void System.IntPtr..ctor(this, System.Int64)")>]
    val internal ctorFromLong : state -> term list -> (term * state) list
