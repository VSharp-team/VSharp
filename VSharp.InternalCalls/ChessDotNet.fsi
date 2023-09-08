namespace VSharp.System

open global.System
open VSharp
open VSharp.Core

module internal ChessDotNet =

    [<Implements("System.Boolean ChessDotNet.Position.Equals(this, System.Object)")>]
    val PositionEquals : state -> term list -> term

    [<Implements("System.Boolean ChessDotNet.Piece.Equals(this, System.Object)")>]
    val PieceEquals : state -> term list -> term
