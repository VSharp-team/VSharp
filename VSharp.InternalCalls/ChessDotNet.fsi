namespace VSharp.System

open global.System
open VSharp
open VSharp.Core

module internal ChessDotNet =

    [<Implements("System.Boolean ChessDotNet.Position.Equals(this, System.Object)")>]
    val internal Equals : state -> term list -> term * state
