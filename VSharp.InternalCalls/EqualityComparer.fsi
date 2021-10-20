namespace VSharp.System

open global.System
open VSharp
open VSharp.Core

module internal EqualityComparer =

    [<Implements("System.Object System.Collections.Generic.ComparerHelpers.CreateDefaultEqualityComparer(System.Type)")>]
    val internal CreateDefaultEqualityComparer : state -> term list -> term

    [<Implements("System.Object System.Collections.Generic.ComparerHelpers.CreateDefaultComparer(System.Type)")>]
    val internal CreateDefaultComparer : state -> term list -> term

    [<Implements("System.Collections.Generic.EqualityComparer`1[T] System.Collections.Generic.EqualityComparer`1[T].get_Default()")>]
    val internal get_Default : state -> term list -> term

    val internal structuralEquality : state -> term -> term -> term
