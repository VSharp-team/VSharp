namespace VSharp.System

open VSharp
open VSharp.Core

module internal ValueType =

    [<Implements("System.Boolean System.ValueType.Equals(this, System.Object)")>]
    val Equals : state -> term list -> term
