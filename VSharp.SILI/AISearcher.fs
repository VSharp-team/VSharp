namespace VSharp.Interpreter.IL

open System.Collections.Generic
open VSharp.Prelude

type internal AISearcher(oracle) =
    let availableStates = HashSet<_>()
    let init states = states |> Seq.iter (availableStates.Add >> ignore)
    let reset () = availableStates.Clear()
    let update (parent, newSates) =
        newSates |> Seq.iter (availableStates.Add >> ignore)
    let remove state =
        let removed = availableStates.Remove state
        assert removed
    let pick selector = 
        availableStates
        Unchecked.defaultof<_>
    
    interface IForwardSearcher with
        override x.Init states = init states
        override x.Pick() = pick (always true)
        override x.Pick selector = pick selector
        override x.Update (parent, newStates) = update (parent, newStates)
        override x.States() = availableStates
        override x.Reset() = reset()
        override x.Remove cilState = remove cilState
        override x.StatesCount with get() = availableStates.Count
