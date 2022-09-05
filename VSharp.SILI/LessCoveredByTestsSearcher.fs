namespace VSharp.Interpreter.IL

open System
open VSharp.Utils

/// <summary>
/// Considers the number of visited basic blocks not covered yet by tests as a state's weight.
/// </summary>
type internal LessCoveredByTestsWeighter(statistics : SILIStatistics) =
    
    let weight state = Some(statistics.GetVisitedBasicBlocksNotCoveredByTests(state).Count |> uint)
    
    interface IWeighter with
        override x.Weight(state) = weight state
        override x.Next() = UInt32.MaxValue
        
type internal LessCoveredByTestsSearcher(maxBound, statistics) =
    inherit WeightedSearcher(maxBound, LessCoveredByTestsWeighter(statistics), BidictionaryPriorityQueue())
    
type internal LessCoveredWithDistanceAsFallbackWeighter(statistics) =
    inherit CombinedWeighter(
        LessCoveredByTestsWeighter(statistics),
        IntraproceduralShortestDistanceToUncoveredWeighter(statistics),
        UInt32.MaxValue,
        WeightCombinators.withInverseLinearFallback 100u)
    
type internal LessCoveredWithDistanceAsFallbackSearcher(maxBound, statistics) =
    inherit WeightedSearcher(maxBound, LessCoveredWithDistanceAsFallbackWeighter(statistics), BidictionaryPriorityQueue())
