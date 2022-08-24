namespace VSharp.Interpreter.IL

type internal LessCoveredByTestsWeighter(statistics : SILIStatistics) =
    
    let weight state =
        match statistics.UncoveredByTestsLocationsCount state with
        | Some count -> Some(uint count)
        | _ -> Some 0u
    
    interface IWeighter with
        override x.Weight(state) = weight state
        override x.Next() = 0u
        
type internal LessCoveredByTestsSearcher(maxBound, statistics) =
    inherit SampledWeightedSearcher(maxBound, LessCoveredByTestsWeighter(statistics))
