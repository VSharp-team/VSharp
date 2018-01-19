namespace VSharp

[<CustomEquality;NoComparison>]
type public MemoryCell<'a> when 'a : equality =
    { value : 'a; created : timestamp; modified : timestamp }  // Value * Creation timestamp * Modification timestamp
    override x.GetHashCode() = x.value.GetHashCode()
    override x.Equals(y) =
        match y with
        | :? MemoryCell<'a> as other -> x.value = other.value
        | _ -> false
