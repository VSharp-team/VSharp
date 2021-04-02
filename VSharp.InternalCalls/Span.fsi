namespace VSharp.System

open global.System
open VSharp
open VSharp.Core

module internal ReadOnlySpan =

    [<Implements("System.Char& System.ReadOnlySpan`1[System.Char].get_Item(this, System.Int32)")>]
    // TODO: make this method works not only for char Span #do
    val internal get_Item : state -> term list -> term * state
