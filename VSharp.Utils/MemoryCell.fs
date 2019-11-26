namespace VSharp
open VSharp.CSharpUtils

[<CustomEquality;NoComparison>]
type public memoryCell<'a, 'fql, 'typ> when 'a : equality and 'fql : equality = // TODO: delete key and typ (use only FQL)
    { key : 'a; FQL : 'fql option; typ : 'typ }  // Key * Fully qualified location * termType
    override x.GetHashCode() =
        match box x.key with
        | :? string as key -> key.GetDeterministicHashCode()
        | _ -> x.key.GetHashCode()

    // TODO: sometimes keys are equal, but FQLs are not
    override x.Equals(o) =
        match o with
        | :? memoryCell<'a, 'fql, 'typ> as other -> x.key.Equals(other.key) && x.FQL = other.FQL
        | _ -> false
