namespace VSharp.Interpreter.IL

open System.Collections.Generic
open Microsoft.FSharp.Core
open VSharp
open VSharp.Interpreter.IL
open System.Linq
open CilStateOperations

/// <summary>
/// Works like DFS searcher, but on each update sorts its state storage by the number
/// of visited basic blocks not covered yet by tests. Stable sorting is used to maintain the
/// DFS-like logic.
/// </summary>
type DFSSortedByContributedCoverageSearcher(maxBound, statistics) =
    inherit SimpleForwardSearcher(maxBound)
    
    let isStopped s = isStopped s || violatesLevel s maxBound
    
    let contributedCoverageWeighter = ContributedCoverageWeighter(statistics) :> IWeighter
    
    let compareWeightOpts (one : uint option) (another : uint option) =
        match one, another with
        | Some oneWeight, Some anotherWeight -> oneWeight.CompareTo(anotherWeight)
        | Some _, None -> 1
        | None, Some _ -> -1
        | None, None -> 0
        
    let comparer = Comparer.Create(compareWeightOpts)    
    let getWeight = contributedCoverageWeighter.Weight
    
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
