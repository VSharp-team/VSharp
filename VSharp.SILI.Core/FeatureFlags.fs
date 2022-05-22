namespace VSharp.Core

type public featureFlags = {
    isConstraintIndependenceEnabled : bool
    isConditionEvalEnabled : bool
    isIncrementalityEnabled : bool
}

module internal FeatureFlags =

    let mutable current = {
        isConstraintIndependenceEnabled = false
        isConditionEvalEnabled = false
        isIncrementalityEnabled = false
    }
    
    let public set flags =
        current <- flags