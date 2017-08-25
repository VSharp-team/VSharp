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

    let public make t = getInheritanceHierarchy t |> List.ofSeq

    let public is (l : System.Type list) (r : System.Type list) =
        Seq.contains (Seq.head r) l

    let public name (h : System.Type list) = (List.head h).FullName

    let public equals (l : System.Type list) (r : System.Type list) = Seq.head l = Seq.head r

    let public inheritor h = List.head h
