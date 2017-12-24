namespace VSharp

module internal Options =

    type ExplorationMode = TrustConventions | CompleteExploration
    let mutable private explorationMode = TrustConventions
    let public ExplorationMode() = explorationMode

    type RecursionUnrollingModeType = SmartUnrolling | AlwaysDisableUnrolling | AlwaysEnableUnrolling
    let mutable private recursionUnrollingMode = SmartUnrolling
    let public RecursionUnrollingMode () = recursionUnrollingMode
