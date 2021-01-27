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
    let mutable private nativeIntExplorationMode = if System.Environment.Is64BitOperatingSystem then Arch64 else Arch32
    let public HandleNativeInt f g =
        match nativeIntExplorationMode with
        | Arch32 -> f
        | Arch64 -> g
        | Unknown -> VSharp.Prelude.__notImplemented__() // TODO: mbdo create constant source
