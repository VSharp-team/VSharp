namespace VSharp.System

open global.System
open VSharp
open VSharp.Core

module internal Runtime_CompilerServices_RuntimeHelpers =

    [<Implements("System.Void System.Runtime.CompilerServices.RuntimeHelpers.InitializeArray(System.Array, System.RuntimeFieldHandle)")>]
    val internal InitializeArray : state -> term list -> term * state

