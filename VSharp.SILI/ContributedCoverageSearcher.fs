namespace VSharp.Interpreter.IL

open System
open VSharp.Utils

/// <summary>
/// Considers the number of visited basic blocks not covered yet by tests as a state's weight.
/// </summary>
type internal ContributedCoverageWeighter(statistics : SILIStatistics) =
    
    let weight state = Some(statistics.GetVisitedBlocksNotCoveredByTests(state).Count |> uint)
    
    interface IWeighter with
        override x.Weight(state) = weight state
        override x.Next() = UInt32.MaxValue
        
type internal ContributedCoverageSearcher(maxBound, statistics) =
    inherit WeightedSearcher(maxBound, ContributedCoverageWeighter(statistics), BidictionaryPriorityQueue())
    
type internal ContributedCoverageWithDistanceAsFallbackWeighter(statistics) =
    inherit CombinedWeighter(
        ContributedCoverageWeighter(statistics),
        IntraproceduralShortestDistanceToUncoveredWeighter(statistics),
        UInt32.MaxValue,
        WeightCombinators.withInverseLinearFallback 100u)
    
type internal ContributedCoverageWithDistanceAsFallbackSearcher(maxBound, statistics) =
    inherit WeightedSearcher(maxBound, ContributedCoverageWithDistanceAsFallbackWeighter(statistics), BidictionaryPriorityQueue())
