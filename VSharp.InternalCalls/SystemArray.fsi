namespace VSharp.System

open global.System
open VSharp
open VSharp.Core

// ------------------------------- mscorlib.System.Array -------------------------------

module internal SystemArray =

    [<Implements("System.Int32 System.Array.GetRank(this)")>]
    val GetRank : state -> term list -> term * state

    [<Implements("System.Int32 System.Array.get_Rank(this)")>]
    val get_Rank : state -> term list -> term * state

    [<Implements("System.Int32 System.Array.get_Length(this)")>]
    val get_Length : state -> term list -> term * state

    [<Implements("System.Void System.Array.Copy(System.Array, System.Int32, System.Array, System.Int32, System.Int32, System.Boolean)")>]
    val Copy : state -> term list -> term * state
