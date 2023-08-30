namespace VSharp.System

open global.System
open VSharp
open VSharp.Core

module internal Buffer =

    [<Implements("System.Void System.Buffer.Memmove(T&, T&, System.UIntPtr)")>]
    val internal GenericMemmove : state -> term list -> term

    [<Implements("System.Void System.Buffer.Memmove(System.Byte&, System.Byte&, System.UIntPtr)")>]
    val internal ByteMemmove : state -> term list -> term

    [<Implements("System.Void System.Buffer.MemoryCopy(System.Void*, System.Void*, System.Int64, System.Int64)")>]
    val internal MemoryCopy : state -> term list -> term
