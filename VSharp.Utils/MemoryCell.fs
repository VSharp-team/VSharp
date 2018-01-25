namespace VSharp

[<CustomEquality;NoComparison>]
type public 'a memoryCell when 'a : equality =
    { value : 'a; created : timestamp; modified : timestamp }  // Value * Creation timestamp * Modification timestamp
    override x.GetHashCode() = x.value.GetHashCode()
    override x.Equals(y) =
        match y with
        | :? memoryCell<'a> as other -> x.value = other.value
        | _ -> false
