namespace VSharp.System

open global.System
open VSharp
open VSharp.Core

module internal ByReference =

    val isValueField : fieldId -> bool

    [<Implements("System.Void System.ByReference`1[T]..ctor(this, T&)")>]
    val ctor : state -> term list -> (term * state) list

    [<Implements("T& System.ByReference`1[T].get_Value(this)")>]
    val getValue : state -> term list -> term
