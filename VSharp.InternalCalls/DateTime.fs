namespace VSharp.System

open System
open VSharp
open VSharp.Core

module internal DateTime =

    // TODO: these implementations cause invalid tests, implement via shims

    let internal get_UtcNow (state : state) (args : term list) =
        assert List.isEmpty args
        let actualTime = DateTime.UtcNow
        Memory.ObjectToTerm state actualTime typeof<DateTime>

    let internal get_Now (state : state) (args : term list) =
        assert List.isEmpty args
        let actualTime = DateTime.Now
        Memory.ObjectToTerm state actualTime typeof<DateTime>
