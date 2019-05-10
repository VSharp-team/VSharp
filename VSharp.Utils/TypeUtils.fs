namespace VSharp

open System.Collections.Generic

module public Hierarchy =
    let isGround (x : System.Type) =
        (not x.IsGenericType && not x.IsGenericParameter) || (x.IsConstructedGenericType)

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
