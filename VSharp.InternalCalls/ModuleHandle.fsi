namespace VSharp.System

open VSharp
open VSharp.Core

module internal ModuleHandle =

    [<Implements("System.Void System.ModuleHandle.GetModuleType(System.Runtime.CompilerServices.QCallModule, System.Runtime.CompilerServices.ObjectHandleOnStack)")>]
    val internal GetModuleType : state -> term list -> term
