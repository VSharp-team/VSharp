namespace VSharp.System

open global.System
open VSharp
open VSharp.Core

module internal ChessDotNet =

    [<Implements("System.Boolean ChessDotNet.Position.Equals(this, System.Object)")>]
    val internal PositionEquals : state -> term list -> term * state

    [<Implements("System.Boolean ChessDotNet.Piece.Equals(this, System.Object)")>]
    val internal PieceEquals : state -> term list -> term * state
