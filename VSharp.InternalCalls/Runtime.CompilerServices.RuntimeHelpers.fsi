namespace VSharp.System

open global.System
open VSharp
open VSharp.Core

module Runtime_CompilerServices_RuntimeHelpers =

    val InitializeArray : state -> term -> term -> state list

    [<Implements("System.Boolean System.Runtime.CompilerServices.RuntimeHelpers.IsBitwiseEquatable()")>]
    val IsBitwiseEquatable : state -> term list -> term * state

    [<Implements("System.Boolean System.Runtime.CompilerServices.RuntimeHelpers.IsReferenceOrContainsReferences()")>]
    val IsReferenceOrContainsReferences : state -> term list -> term * state

    [<Implements("System.Int32 System.Runtime.CompilerServices.RuntimeHelpers.GetHashCode(System.Object)")>]
    val GetHashCode : state -> term list -> term * state

    [<Implements("System.Boolean System.Runtime.CompilerServices.RuntimeHelpers.Equals(System.Object, System.Object)")>]
    val Equals : state -> term list -> term * state
