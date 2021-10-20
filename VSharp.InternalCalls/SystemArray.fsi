namespace VSharp.System

open global.System
open VSharp
open VSharp.Core

// ------------------------------- mscorlib.System.Array -------------------------------

module internal SystemArray =

    [<Implements("System.Int32 System.Array.GetRank(this)")>]
    val GetRank : state -> term list -> term

    [<Implements("System.Int32 System.Array.get_Rank(this)")>]
    val get_Rank : state -> term list -> term

    [<Implements("System.Int32 System.Array.get_Length(this)")>]
    val get_Length : state -> term list -> term

    [<Implements("System.Boolean System.SZArrayHelper.Contains(this, T)")>]
    val ContainsChar : state -> term list -> term

    [<Implements("System.Int32 System.SZArrayHelper.get_Count(this)")>]
    val GetCount : state -> term list -> term

    [<Implements("T System.SZArrayHelper.get_Item(this, System.Int32)")>]
    val GetItem : state -> term list -> term
