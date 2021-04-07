namespace VSharp.System

open global.System
open VSharp
open VSharp.Core

module internal EqualityComparer =

    [<Implements("System.Object System.Collections.Generic.ComparerHelpers.CreateDefaultEqualityComparer(System.Type)")>]
    val internal CreateDefaultEqualityComparer : state -> term list -> term * state

    [<Implements("System.Collections.Generic.EqualityComparer`1[ChessDotNet.Piece] System.Collections.Generic.EqualityComparer`1[ChessDotNet.Piece].get_Default()")>]
    val internal get_Default : state -> term list -> term * state

    val internal equalsForTwoFieldsBlock : state -> term -> term -> term

    [<Implements("System.Boolean System.Collections.Generic.EqualityComparer`1[ChessDotNet.Piece].Equals(this, ChessDotNet.Piece, ChessDotNet.Piece)")>]
    val internal PieceEquals : state -> term list -> term * state
