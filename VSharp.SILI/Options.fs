namespace VSharp

module internal Options =
    type StaticFieldsValuationType = Overapproximate | Interpret
    let private staticFieldsValuation = ref Interpret
    let public StaticFieldsValuation = !staticFieldsValuation
