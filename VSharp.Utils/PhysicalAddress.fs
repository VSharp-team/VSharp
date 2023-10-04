namespace VSharp

open System
open System.Runtime.CompilerServices

open VSharp.Utils

[<CustomEquality;NoComparison>]
type physicalAddress = {object : obj}
    with
    override x.GetHashCode() = RuntimeHelpers.GetHashCode(x.object)
    override x.Equals(o : obj) =
        match o with
        | :? physicalAddress as other ->
            Object.ReferenceEquals(x.object, other.object)
        | _ -> false
    override x.ToString() = PrettyPrinting.printConcrete x.object

    member x.IsEmpty = x.object = null
    static member Empty = {object = null}
