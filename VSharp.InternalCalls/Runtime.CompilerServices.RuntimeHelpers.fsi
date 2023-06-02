namespace VSharp.System

open global.System
open VSharp
open VSharp.Core

module Runtime_CompilerServices_RuntimeHelpers =

    [<Implements("System.Boolean System.Runtime.CompilerServices.RuntimeHelpers.IsBitwiseEquatable()")>]
    val IsBitwiseEquatable : state -> term list -> term

    [<Implements("System.Boolean System.Runtime.CompilerServices.RuntimeHelpers.IsReferenceOrContainsReferences()")>]
    val IsReferenceOrContainsReferences : state -> term list -> term

    [<Implements("System.Int32 System.Runtime.CompilerServices.RuntimeHelpers.GetHashCode(System.Object)")>]
    val CommonGetHashCode : state -> term list -> term

    [<Implements("System.Int32 System.ValueType.GetHashCode(this)")>]
    val ValueTypeGetHashCode : state -> term list -> term

    [<Implements("System.Boolean System.Runtime.CompilerServices.RuntimeHelpers.Equals(System.Object, System.Object)")>]
    val Equals : state -> term list -> term
