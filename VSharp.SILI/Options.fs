namespace VSharp.Interpreter.IL

type ExplorationMode = TrustConventions | CompleteExploration
type RecursionUnrollingModeType = SmartUnrolling | NeverUnroll | AlwaysUnroll
type NativeIntExplorationMode = Arch32 | Arch64 | Unknown

module internal Options =

    let mutable private invokeConcrete = true
    let public InvokeConcrete () = invokeConcrete

    let mutable private explorationMode = TrustConventions
    let public ExplorationMode () = explorationMode

    let mutable private recursionUnrollingMode = NeverUnroll
    let public RecursionUnrollingMode () = recursionUnrollingMode
