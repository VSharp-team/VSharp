namespace VSharp

open System
open System.Collections.Generic
open System.Reflection

module TypeUtils =
    let isGround (x : Type) =
        (not x.IsGenericType && not x.IsGenericParameter) || (x.IsConstructedGenericType)

    let defaultOf (t : Type) =
        if t.IsValueType && Nullable.GetUnderlyingType t = null && not (t.ContainsGenericParameters)
            then Activator.CreateInstance t
            else null

    let private integralTypes =
        new HashSet<Type>(
                          [typedefof<byte>; typedefof<sbyte>;
                           typedefof<int16>; typedefof<uint16>;
                           typedefof<int32>; typedefof<uint32>;
                           typedefof<int64>; typedefof<uint64>;
                           typedefof<char>])

    let private unsignedTypes =
        new HashSet<Type>(
                          [typedefof<byte>; typedefof<uint16>;
                           typedefof<uint32>; typedefof<uint64>;])

    let private realTypes =
        new HashSet<Type>([typedefof<single>; typedefof<double>; typedefof<decimal>])

    let private numericTypes = new HashSet<Type>(Seq.append integralTypes realTypes)

    let private primitiveTypes = new HashSet<Type>(Seq.append numericTypes [typedefof<bool>])

    let isNumeric = numericTypes.Contains
    let isIntegral = integralTypes.Contains
    let isReal = realTypes.Contains
    let isUnsigned = unsignedTypes.Contains

    // returns true, if at least one constraint on type parameter "t" implies that "t" is reference type (for example, "t : class" case)
    // returns false, if "t" is value type or if we have no information about "t" type from constraints
    let rec isReferenceTypeParameter (t : System.Type) =
        let checkAttribute (t : System.Type) =
            t.GenericParameterAttributes &&& GenericParameterAttributes.ReferenceTypeConstraint <> GenericParameterAttributes.None
        let isSimpleReferenceConstraint (t : System.Type) = t.IsClass && t <> typeof<System.ValueType>
        let isReferenceConstraint (c : System.Type) = if c.IsGenericParameter then isReferenceTypeParameter c else isSimpleReferenceConstraint c
        checkAttribute t || t.GetGenericParameterConstraints() |> Array.exists isReferenceConstraint

    // returns true, if at least one constraint on type parameter "t" implies that "t" is value type (for example, "t : struct" case)
    // returns false, if "t" is reference type or if we have no information about "t" type from constraints
    let isValueTypeParameter (t : System.Type) = t.IsValueType

    let private isInt = (=) typeof<int32>
    let private isUInt = (=) typeof<uint32>
    let private isLong = (=) typeof<int64>
    let private isULong = (=) typeof<uint64>

    let uncheckedChangeType (value : obj) t =
        let bytes =
            match value with
            | :? bool    as b  -> (if b then 1L else 0L) |> BitConverter.GetBytes
            | :? byte    as n  -> n |> int64 |> BitConverter.GetBytes
            | :? sbyte   as n  -> n |> int64 |> BitConverter.GetBytes
            | :? int16   as n  -> n |> int64 |> BitConverter.GetBytes
            | :? uint16  as n  -> n |> int64 |> BitConverter.GetBytes
            | :? int     as n  -> n |> int64 |> BitConverter.GetBytes
            | :? uint32  as n  -> n |> uint64 |> BitConverter.GetBytes
            | :? int64   as n  -> n |> BitConverter.GetBytes
            | :? uint64  as n  -> n |> BitConverter.GetBytes
            | _ -> __notImplemented__()
        match t with
        | _ when t = typedefof<Boolean>   ->  BitConverter.ToBoolean(bytes, 0) :> obj
        | _ when t = typedefof<Byte>      ->  bytes.[0]                        :> obj
        | _ when t = typedefof<SByte>     ->  bytes.[0] |> sbyte               :> obj
        | _ when t = typedefof<Char>      ->  BitConverter.ToChar(bytes, 0)    :> obj
        | _ when t = typedefof<Int16>     ->  BitConverter.ToInt16(bytes, 0)   :> obj
        | _ when t = typedefof<UInt16>    ->  BitConverter.ToUInt16(bytes, 0)  :> obj
        | _ when t = typedefof<Int32>     ->  BitConverter.ToInt32(bytes, 0)   :> obj
        | _ when t = typedefof<UInt32>    ->  BitConverter.ToUInt32(bytes, 0)  :> obj
        | _ when t = typedefof<Int64>     ->  BitConverter.ToInt64(bytes, 0)   :> obj
        | _ when t = typedefof<UInt64>    ->  BitConverter.ToUInt64(bytes, 0)  :> obj
        | _ -> __notImplemented__()

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
