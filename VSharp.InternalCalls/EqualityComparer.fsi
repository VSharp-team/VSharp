namespace VSharp.System

open global.System
open VSharp
open VSharp.Core

module internal EqualityComparer =

    [<Implements("System.Object System.Collections.Generic.ComparerHelpers.CreateDefaultEqualityComparer(System.Type)")>]
    val internal CreateDefaultEqualityComparer : state -> term list -> term * state

    [<Implements("System.Collections.Generic.EqualityComparer`1[ChessDotNet.Piece] System.Collections.Generic.EqualityComparer`1[ChessDotNet.Piece].get_Default()")>]
    val internal get_Default : state -> term list -> term * state

    [<Implements("System.Collections.Generic.EqualityComparer`1[System.String] System.Collections.Generic.EqualityComparer`1[System.String].get_Default()")>]
    val internal get_DefaultForString : state -> term list -> term * state

    val internal structuralEquality : state -> term -> term -> term
