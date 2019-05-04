namespace VSharp

open System.Collections.Generic

type hierarchy(h: System.Type list) =
    member x.Inheritor = List.head h
    member x.Hierarchy = h
    member x.Name = x.Inheritor.ToString()
    member x.IsGround =
        (not x.Inheritor.IsGenericType && not x.Inheritor.IsGenericParameter) || (x.Inheritor.IsConstructedGenericType)
    member x.Is (r : hierarchy) = r.Inheritor.IsAssignableFrom x.Inheritor
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
    let defaultOf (t : System.Type) =
        if t.IsValueType && System.Nullable.GetUnderlyingType t = null && not (t.ContainsGenericParameters)
            then System.Activator.CreateInstance t
            else null

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

    let private isInt = (=) typeof<int32>
    let private isUInt = (=) typeof<uint32>
    let private isLong = (=) typeof<int64>
    let private isULong = (=) typeof<uint64>

    let failDeduceBinaryTargetType op x y =
        internalfailf "%s (%O x, %O y); is not a binary arithmetical operator" op x y

    let inline deduceComparisonTargetType _ _ = typeof<bool>

    let private deduceBinaryGenericTargetType op x y =
        let areSameButSignedAndUnsigned =
            isInt x && isUInt y
            || isUInt x && isInt y
            || isLong x && isULong y
            || isULong x && isLong y
        if isReal x || isReal y || areSameButSignedAndUnsigned then failDeduceBinaryTargetType op x y
        elif isLong x || isULong x then x // DO NOT REORDER THESE elif's!
        elif isLong y || isULong y then y
        elif isInt x || isUInt x then x
        elif isInt y || isUInt y then y
        else typeof<int32>

    let deduceSimpleArithmeticOperationTargetType x y =
        if x = y && isReal x then x else deduceBinaryGenericTargetType "{*, +, -, /, %}" x y

    let deduceLogicalArithmeticOperationTargetType x y = deduceBinaryGenericTargetType "{&, |, ^}" x y

    let deduceShiftTargetType x y =
        let fail() = failDeduceBinaryTargetType "{<<, >>}" x y

        if not <| isInt y then fail()                               // DO NOT REORDER THESE elif's!
        elif isInt x || isUInt x || isLong x || isULong x then x
        elif isIntegral x then typeof<int32>
        else fail()
