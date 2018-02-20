namespace VSharp

open System.Collections.Generic

type hierarchy(h: System.Type list) =
    member x.Inheritor = List.head h
    member x.Hierarchy = h
    member x.Name = x.Inheritor.ToString()
    member x.IsGround =
        (not x.Inheritor.IsGenericType && not x.Inheritor.IsGenericParameter) || (x.Inheritor.IsConstructedGenericType)
    member x.Is (r : hierarchy) = Seq.contains r.Inheritor h
    member x.Equals (r : hierarchy) = x.Inheritor = r.Inheritor
    member x.Equals (r : System.Type) = x.Inheritor = r
    override x.ToString() = x.Name
    override x.GetHashCode() =
        x.Hierarchy.GetHashCode()
    override x.Equals(o : obj) =
        match o with
        | :? hierarchy as other -> x.GetHashCode() = other.GetHashCode()
        | _ -> false

module private Hierarchy =
    let rec getBaseTypeFromGenericParameter (t : System.Type) =
        let mergeHierarchy (l : System.Type) (r : System.Type) =
            if l.IsAssignableFrom(r) then r else l
        let nakedTypes =
            t.GetGenericParameterConstraints() |>
            Seq.filter (fun nt -> nt.IsGenericParameter) |>
            Seq.map getBaseTypeFromGenericParameter |>
            List.ofSeq
        t.BaseType :: nakedTypes |> Seq.reduce mergeHierarchy

    and getInheritanceHierarchy (t : System.Type) =
        match t with
        | null -> List.empty
        | _ when t.IsGenericParameter -> t :: getInheritanceHierarchy (getBaseTypeFromGenericParameter t)
        | _ -> t :: getInheritanceHierarchy t.BaseType

type hierarchy with
    new (typ : System.Type) = hierarchy (Hierarchy.getInheritanceHierarchy typ)

module TypeUtils =
    let private integralTypes =
        new HashSet<System.Type>(
                          [typedefof<byte>; typedefof<sbyte>;
                           typedefof<int16>; typedefof<uint16>;
                           typedefof<int32>; typedefof<uint32>;
                           typedefof<int64>; typedefof<uint64>;
                           typedefof<char>])

    let private unsignedTypes =
        new HashSet<System.Type>(
                          [typedefof<byte>; typedefof<uint16>;
                           typedefof<uint32>; typedefof<uint64>;])

    let private realTypes =
        new HashSet<System.Type>([typedefof<single>; typedefof<double>; typedefof<decimal>])

    let private numericTypes = new HashSet<System.Type>(Seq.append integralTypes realTypes)

    let private primitiveTypes = new HashSet<System.Type>(Seq.append numericTypes [typedefof<bool>])

    let isNumeric = numericTypes.Contains
    let isPrimitive t = primitiveTypes.Contains t || t.IsEnum
    let isIntegral = integralTypes.Contains
    let isReal = realTypes.Contains
    let isUnsigned = unsignedTypes.Contains

