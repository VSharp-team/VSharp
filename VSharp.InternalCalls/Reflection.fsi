namespace VSharp.System

open global.System
open VSharp
open VSharp.Core

module internal Reflection =
    [<Implements("System.Reflection.Assembly System.Reflection.Assembly.GetEntryAssembly()")>]
    val getEntryAssembly : state -> term list -> term
