namespace VSharp.Core

type public featureFlags = {
    isConstraintIndependenceEnabled : bool
    isConditionEvaluationEnabled : bool
    isIncrementalityEnabled : bool
}

module internal FeatureFlags =

    let mutable current = {
        isConstraintIndependenceEnabled = false
        isConditionEvaluationEnabled = false
        isIncrementalityEnabled = false
    }
    
    let public set flags =
        current <- flags
