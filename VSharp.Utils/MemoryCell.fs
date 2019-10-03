namespace VSharp

[<CustomEquality;NoComparison>]
type public memoryCell<'a, 'fql, 'typ> when 'a : equality and 'fql : equality =
    { key : 'a; FQL : 'fql option; typ : 'typ }  // Key * Fully qualified location * termType
    override x.GetHashCode() = x.key.GetHashCode()

    // TODO: add check on FQL equality (now it fails on ClassesSimple.Test1, ClassesSimplePropertyAccess.TestProperty1, Strings.SymbolicString, Unsafe.CompilerHackLikePtrReturn)
    override x.Equals(o) =
        match o with
        | :? memoryCell<'a, 'fql, 'typ> as other -> x.key.Equals(other.key)
        | _ -> false
