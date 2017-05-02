namespace VSharp

module internal Options =
    type StaticFieldsValuationType = SymbolizeStaticFields | DefaultStaticFields
    let mutable private staticFieldsValuation = DefaultStaticFields
    let public StaticFieldsValuation () = staticFieldsValuation

    type SymbolicArrayLowerBoundStrategyType = AlwaysZero | AlwaysSymbolic
    let mutable private symbolicArrayLowerBoundStrategy = AlwaysZero
    let public SymbolicArrayLowerBoundStrategy () = symbolicArrayLowerBoundStrategy

    type RecursionUnrollingModeType = SmartUnrolling | AlwaysDisableUnrolling | AlwaysEnableUnrolling
    let mutable private recursionUnrollingMode = SmartUnrolling
    let public RecursionUnrollingMode () = recursionUnrollingMode

    let mutable private writeDependenciesApproximationTreshold = 10
    let public WriteDependenciesApproximationTreshold () = writeDependenciesApproximationTreshold
