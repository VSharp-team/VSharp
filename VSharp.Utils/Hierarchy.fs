namespace VSharp

module public Hierarchy =

    let rec private getBaseTypeFromGenericParameter (t : System.Type) =
        let mergeHierarchy (l : System.Type) (r : System.Type) =
            if l.IsAssignableFrom(r) then r else l in
        let nakedTypes =
            t.GetGenericParameterConstraints() |>
            Seq.filter (fun nt -> nt.IsGenericParameter) |>
            Seq.map getBaseTypeFromGenericParameter in
        let baseType = Seq.singleton t.BaseType in
        Seq.append baseType nakedTypes |> Seq.reduce mergeHierarchy

    and private getInheritanceHierarchy (t : System.Type) =
        match t with
        | null -> Seq.empty
        | _ when t.IsGenericParameter -> Seq.append (Seq.singleton t) (getInheritanceHierarchy <| getBaseTypeFromGenericParameter t)
        | _ -> Seq.append (Seq.singleton t) (getInheritanceHierarchy t.BaseType)

    let private make t = getInheritanceHierarchy t |> List.ofSeq

    type public Hierarchy(hierarchy : System.Type list) =
        member this.Inheritor = List.head hierarchy
        member this.Hierarchy = hierarchy
        member this.Name = this.Inheritor.ToString()
        member this.Is (r : Hierarchy) = Seq.contains r.Inheritor hierarchy
        member this.Equals (r : Hierarchy) = this.Inheritor = r.Inheritor
        member this.Equals (r : System.Type) = this.Inheritor = r
        new (typ : System.Type) = Hierarchy(make typ)
        override this.ToString() = this.Name
