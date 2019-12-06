namespace VSharp
open VSharp.CSharpUtils

[<StructuralEquality;NoComparison>]
type public memoryCell<'a, 'fql, 'typ> when 'a : equality and 'fql : equality = // TODO: delete key and typ (use only FQL)
    { key : 'a; FQL : 'fql option; typ : 'typ }  // Key * Fully qualified location * termType
