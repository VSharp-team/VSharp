namespace VSharp.System

open System
open VSharp
open VSharp.Core

module internal ModuleHandle =

    let internal GetModuleType (state : state) (args : term list) : term =
        internalfail "System.Void System.ModuleHandle.GetModuleType(System.Runtime.CompilerServices.QCallModule, System.Runtime.CompilerServices.ObjectHandleOnStack) is not implemented"
