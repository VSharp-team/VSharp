namespace VSharp

module internal Options =
    type StaticFieldsValuationType = Overapproximate | Interpret
    let private staticFieldsValuation = ref Interpret
    let public StaticFieldsValuation = !staticFieldsValuation

    type SymbolicArrayLowerBoundStrategyType = AlwaysZero | AlwaysSymbolic
    let private symbolicArrayLowerBoundStrategy = ref AlwaysZero
    let public SymbolicArrayLowerBoundStrategy = !symbolicArrayLowerBoundStrategy
