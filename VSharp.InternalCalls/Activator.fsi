namespace VSharp.System

open VSharp
open VSharp.Core

module internal Activator =

    [<Implements("System.Object System.Activator.CreateInstance()")>]
    val internal CreateInstance : state -> term list -> term
