namespace VSharp.System

open global.System
open VSharp
open VSharp.Core
open VSharp.Interpreter.IL
open VSharp.Interpreter.IL.CilState

module internal Set =
    [<Implements("System.Boolean System.Collections.Generic.SortedSet`1[T].Add(this, T)")>]
    [<Implements("System.Boolean System.Collections.Generic.HashSet`1[T].Add(this, T)")>]
    val AddItem : state -> term list -> term

    [<Implements("System.Boolean System.Collections.Generic.SortedSet`1[T].Remove(this, T)")>]
    [<Implements("System.Boolean System.Collections.Generic.HashSet`1[T].Remove(this, T)")>]
    val RemoveItem : state -> term list -> term

    [<Implements("System.Boolean System.Collections.Generic.SortedSet`1[T].Contains(this, T)")>]
    [<Implements("System.Boolean System.Collections.Generic.HashSet`1[T].Contains(this, T)")>]
    val IsContainsItem : state -> term list -> term

    [<Implements("System.Int32 System.Collections.Generic.SortedSet`1[T].get_Count(this)")>]
    [<Implements("System.Int32 System.Collections.Generic.HashSet`1[T].get_Count(this)")>]
    val GetCount : state -> term list -> term
