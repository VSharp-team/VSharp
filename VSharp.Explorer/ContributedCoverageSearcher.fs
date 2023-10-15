namespace VSharp.Explorer

open System
open VSharp.Utils

/// <summary>
/// Considers the number of visited basic blocks not covered yet by tests as a state's weight.
/// </summary>
type internal ContributedCoverageWeighter(statistics : SVMStatistics) =

    let weight state = Some(statistics.GetVisitedBlocksNotCoveredByTests(state).Count |> uint)

    interface IWeighter with
        override x.Weight(state) = weight state

type internal ContributedCoverageSearcher(statistics) =
    inherit WeightedSearcher(ContributedCoverageWeighter(statistics), BidictionaryPriorityQueue())

type internal ContributedCoverageWithDistanceAsFallbackWeighter(statistics) =
    inherit CombinedWeighter(
        ContributedCoverageWeighter(statistics),
        IntraproceduralShortestDistanceToUncoveredWeighter(statistics),
        UInt32.MaxValue,
        WeightOperations.withInverseLinearFallback 100u)

type internal ContributedCoverageWithDistanceAsFallbackSearcher(statistics) =
    inherit WeightedSearcher(ContributedCoverageWithDistanceAsFallbackWeighter(statistics), BidictionaryPriorityQueue())
