namespace VSharp.Core

type ExplorationMode = TrustConventions | CompleteExploration
type RecursionUnrollingModeType = SmartUnrolling | NeverUnroll | AlwaysUnroll

module internal Options =

    let mutable private explorationMode = TrustConventions
    let public ExplorationMode() = explorationMode

    let mutable private recursionUnrollingMode = NeverUnroll
    let public RecursionUnrollingMode () = recursionUnrollingMode
