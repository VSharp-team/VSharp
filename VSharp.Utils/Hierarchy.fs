namespace VSharp

module public Hierarchy =

    let rec private getBaseTypeFromGenericParameter (t : System.Type) =
        let mergeHierarchy (l : System.Type) (r : System.Type) =
            if l.IsAssignableFrom(r) then r else l in
        let nakedTypes =
            t.GetGenericParameterConstraints() |>
            Seq.filter (fun nt -> nt.IsGenericParameter) |>
            Seq.map getBaseTypeFromGenericParameter |>
            List.ofSeq in
        t.BaseType :: nakedTypes |> Seq.reduce mergeHierarchy

    and private getInheritanceHierarchy (t : System.Type) =
        match t with
        | null -> List.empty
        | _ when t.IsGenericParameter -> t :: getInheritanceHierarchy (getBaseTypeFromGenericParameter t)
        | _ -> t :: getInheritanceHierarchy t.BaseType

    let private make t = getInheritanceHierarchy t

    type public Hierarchy(hierarchy : System.Type list) =
        member x.Inheritor = List.head hierarchy
        member x.Hierarchy = hierarchy
        member x.Name = x.Inheritor.ToString()
        member x.Is (r : Hierarchy) = Seq.contains r.Inheritor hierarchy
        member x.Equals (r : Hierarchy) = x.Inheritor = r.Inheritor
        member x.Equals (r : System.Type) = x.Inheritor = r
        new (typ : System.Type) = Hierarchy(make typ)
        override x.ToString() = x.Name
        override x.GetHashCode() =
            Microsoft.FSharp.Core.LanguagePrimitives.PhysicalHash(x.Hierarchy)
        override x.Equals(o : obj) =
            match o with
            | :? Hierarchy as other -> x.GetHashCode() = other.GetHashCode()
            | _ -> false
