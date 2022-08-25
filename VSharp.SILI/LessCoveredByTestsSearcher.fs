namespace VSharp.Interpreter.IL

open System
open VSharp.Utils

type internal LessCoveredByTestsWeighter(statistics : SILIStatistics) =
    
    let weight state =
        match statistics.UncoveredByTestsLocationsCount state with
        | Some count -> Some(uint count)
        | _ -> Some 0u
    
    interface IWeighter with
        override x.Weight(state) = weight state
        override x.Next() = UInt32.MaxValue
        
type internal LessCoveredByTestsSearcher(maxBound, statistics) =
    inherit SampledWeightedSearcher(maxBound, LessCoveredByTestsWeighter(statistics))
    
type internal LessCoveredWithDistanceAsFallbackWeighter(statistics) =
    inherit CombinedWeighter(
        LessCoveredByTestsWeighter(statistics),
        IntraproceduralShortestDistanceToUncoveredWeighter(statistics),
        UInt32.MaxValue,
        WeightCombinators.withInverseLinearFallback 100u)
    
type internal LessCoveredWithDistanceAsFallbackSearcher(maxBound, statistics) =
    inherit WeightedSearcher(maxBound, LessCoveredWithDistanceAsFallbackWeighter(statistics), BidictionaryPriorityQueue())
