namespace VSharp.System

open global.System
open VSharp
open VSharp.Core

module internal Buffer =

    val CommonMemmove : state -> term -> term option -> term -> term option -> term -> unit

    [<Implements("System.Void System.Buffer.Memmove(T&, T&, System.UIntPtr)")>]
    val GenericMemmove : state -> term list -> term

    [<Implements("System.Void System.Buffer.Memmove(System.Byte&, System.Byte&, System.UIntPtr)")>]
    val ByteMemmove : state -> term list -> term

    [<Implements("System.Void System.Buffer.MemoryCopy(System.Void*, System.Void*, System.Int64, System.Int64)")>]
    val MemoryCopy : state -> term list -> term
