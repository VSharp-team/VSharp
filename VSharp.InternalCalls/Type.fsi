namespace VSharp.System

open global.System
open VSharp
open VSharp.Core

module internal Type =
    [<Implements("System.Type System.Type.GetTypeFromHandle(System.RuntimeTypeHandle)")>]
    val internal GetTypeFromHandle : state -> term list -> term * state
