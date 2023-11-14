namespace VSharp.Explorer

open System.Collections.Generic
open Microsoft.FSharp.Core
open VSharp
open VSharp.Interpreter.IL
open System.Linq

/// <summary>
/// Works like DFS searcher, but on each update sorts its state storage by the number
/// of visited basic blocks not covered yet by tests. Stable sorting is used to maintain the
/// DFS-like logic.
/// </summary>
type DFSSortedByContributedCoverageSearcher(statistics) =
    let mutable states = List<cilState>()
    let contributedCoverageWeighter = ContributedCoverageWeighter(statistics) :> IWeighter

    let compareWeightOpts (one : uint option) (another : uint option) =
        match one, another with
        | Some oneWeight, Some anotherWeight -> oneWeight.CompareTo(anotherWeight)
        | Some _, None -> 1
        | None, Some _ -> -1
        | None, None -> 0

    let comparer = Comparer.Create(compareWeightOpts)
    let getWeight = contributedCoverageWeighter.Weight

    let update newStates =
        states.AddRange newStates
        // Stable sorting
        states <- states.OrderBy(getWeight, comparer).ToList()

    interface IForwardSearcher with
        override x.Init initialStates = initialStates.OrderBy(getWeight, comparer) |> states.AddRange
        override x.Pick selector = Seq.tryFindBack selector states
        override x.Pick() = Seq.tryLast states
        override x.Update(_, newStates) = update newStates
        override x.States() = states
        override x.Reset() = states.Clear()
        override x.Remove cilState = states.Remove cilState |> ignore
        override x.StatesCount with get() = states.Count
