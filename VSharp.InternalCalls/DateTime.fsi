namespace VSharp.System

open VSharp
open VSharp.Core

module internal DateTime =

    [<Implements("System.DateTime System.DateTime.get_UtcNow()")>]
    val internal get_UtcNow : state -> term list -> term

    [<Implements("System.DateTime System.DateTime.get_Now()")>]
    val internal get_Now : state -> term list -> term
