namespace VSharp.Interpreter.IL

open System.Collections.Generic
open Microsoft.FSharp.Core
open VSharp
open VSharp.Interpreter.IL
open System.Linq
open CilStateOperations

type DFSSortedByLessCoveredSearcher(maxBound, statistics) =
    inherit SimpleForwardSearcher(maxBound)
    
    let isStopped s = isStopped s || violatesLevel s maxBound
    
    let lessCoveredWeighter = LessCoveredByTestsWeighter(statistics) :> IWeighter
    
    let compareWeightOpts (one : uint option) (another : uint option) =
        match one, another with
        | Some oneWeight, Some anotherWeight -> oneWeight.CompareTo(anotherWeight)
        | Some _, None -> 1
        | None, Some _ -> -1
        | None, None -> 0
        
    let comparer = Comparer.Create(compareWeightOpts)    
    let getWeight = lessCoveredWeighter.Weight
    
    let add (states : List<cilState>) newState =
        if not <| isStopped newState then
            assert(states.Contains newState |> not)
            states.Add(newState)
    
    override x.Init states initStates =
        initStates.OrderBy(getWeight, comparer) |> Seq.iter (add states)
    
    override x.Insert states (parent, newStates) =
        if isStopped parent then
            states.Remove(parent) |> ignore
        newStates |> Seq.iter (add states)
        let sorted = states.OrderBy(getWeight, comparer).ToList()
        states.Clear()
        states.AddRange(sorted)
