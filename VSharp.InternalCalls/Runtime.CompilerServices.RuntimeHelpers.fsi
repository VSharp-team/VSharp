namespace VSharp.System

open global.System
open VSharp
open VSharp.Core

module Runtime_CompilerServices_RuntimeHelpers =

    val InitializeArray : state -> term -> term -> state list

    [<Implements("System.Boolean System.Runtime.CompilerServices.RuntimeHelpers.IsBitwiseEquatable()")>]
    val IsBitwiseEquatable : state -> term list -> term * state
