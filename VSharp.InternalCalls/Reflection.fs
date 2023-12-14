namespace VSharp.System

open System.Reflection
open global.System
open VSharp.Core

module internal Reflection =
    let getEntryAssembly (state : state) (_ : term list) =
        let m = Memory.EntryFunction state
        let asm = m.DeclaringType.Assembly
        Memory.ObjectToTerm state asm typeof<Assembly>
