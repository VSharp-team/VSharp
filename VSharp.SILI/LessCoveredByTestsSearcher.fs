namespace VSharp.Interpreter.IL

open System

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
