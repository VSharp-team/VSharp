namespace VSharp

open System
open System.Collections.Generic
open System.Reflection
open System.Reflection.Emit
open FSharpx.Collections

module TypeUtils =

    // ---------------------------------- Basic type groups ----------------------------------

    let private integralTypes =
        HashSet<Type>([typedefof<byte>; typedefof<sbyte>;
                       typedefof<int16>; typedefof<uint16>;
                       typedefof<int32>; typedefof<uint32>;
                       typedefof<int64>; typedefof<uint64>;
                       typedefof<char>])

    let private unsignedTypes =
        HashSet<Type>([typedefof<byte>; typedefof<uint16>;
                       typedefof<uint32>; typedefof<uint64>;])

    let private realTypes = HashSet<Type>([typedefof<single>; typedefof<double>; typedefof<decimal>])

    let private numericTypes = HashSet<Type>(Seq.append integralTypes realTypes)

    let private primitiveTypes = HashSet<Type>(Seq.append numericTypes [typedefof<bool>])

    // ---------------------------------- Basic type predicates ----------------------------------

    let isGround (x : Type) =
        (not x.IsGenericType && not x.IsGenericParameter) || (x.IsConstructedGenericType)

    let isNumeric x = numericTypes.Contains x || x.IsEnum
    let isIntegral = integralTypes.Contains
    let isReal = realTypes.Contains
    let isUnsigned = unsignedTypes.Contains
    let isPrimitive = primitiveTypes.Contains

    // returns true, if at least one constraint on type parameter "t" implies that "t" is reference type (for example, "t : class" case)
    // returns false, if "t" is value type or if we have no information about "t" type from constraints
    let rec isReferenceTypeParameter (t : Type) =
        let checkAttribute (t : Type) =
            t.GenericParameterAttributes &&& GenericParameterAttributes.ReferenceTypeConstraint <> GenericParameterAttributes.None
        let isSimpleReferenceConstraint (t : Type) = t.IsClass && t <> typeof<ValueType>
        let isReferenceConstraint (c : Type) = if c.IsGenericParameter then isReferenceTypeParameter c else isSimpleReferenceConstraint c
        checkAttribute t || t.GetGenericParameterConstraints() |> Array.exists isReferenceConstraint

    // returns true, if at least one constraint on type parameter "t" implies that "t" is value type (for example, "t : struct" case)
    // returns false, if "t" is reference type or if we have no information about "t" type from constraints
    let isValueTypeParameter (t : Type) = t.IsValueType

    let private isInt8 = (=) typeof<int8>
    let private isUInt8 = (=) typeof<uint8>
    let private isInt16 = (=) typeof<int16>
    let private isUInt16 = (=) typeof<uint16>
    let private isInt = (=) typeof<int32>
    let private isUInt = (=) typeof<uint32>
    let private isLong = (=) typeof<int64>
    let private isULong = (=) typeof<uint64>

    let private isWiderForNumericTypesMap =
        let widerThan8  = [|typeof<int32>; typeof<uint32>; typeof<int64>; typeof<uint64>; typeof<int16>; typeof<uint16>; typeof<char>; typeof<float32>; typeof<float>|]
        let widerThan16 = [|typeof<int32>; typeof<uint32>; typeof<int64>; typeof<float32>; typeof<uint64>; typeof<float>|]
        let widerThan32 = [|typeof<int64>; typeof<uint64>; typeof<float>|]
        let widerThan64 = [||]
        PersistentDict.ofSeq [
            (typeof<int8>,    widerThan8)
            (typeof<uint8>,   widerThan8)
            (typeof<int16>,   widerThan16)
            (typeof<uint16>,  widerThan16)
            (typeof<char>,    widerThan16)
            (typeof<int32>,   widerThan32)
            (typeof<uint32>,  widerThan32)
            (typeof<float32>, widerThan32)
            (typeof<int64>,   widerThan64)
            (typeof<uint64>,  widerThan64)
            (typeof<float>,   widerThan64) ]

    let isLessForNumericTypes (t1 : Type) (t2 : Type) =
        let t1 = if t1.IsEnum then t1.GetEnumUnderlyingType() else t1
        let t2 = if t2.IsEnum then t2.GetEnumUnderlyingType() else t2
        assert(isNumeric t1 && isNumeric t2)
        Array.contains t2 isWiderForNumericTypesMap.[t1]

    // ---------------------------------- Basic type operations ----------------------------------

    let getTypeOfConcrete value =
        if box value = null then null
        else value.GetType()

    let inline isNullable (t : Type) = Nullable.GetUnderlyingType(t) <> null

    let defaultOf (t : Type) =
        if t.IsValueType && not (isNullable t) && not t.ContainsGenericParameters
            then Activator.CreateInstance t
            else null

    let internalSizeOf (typ: Type) : uint32 = // Reflection hacks, don't touch! Marshal.SizeOf lies!
        let meth = DynamicMethod("GetManagedSizeImpl", typeof<uint32>, null);
        let gen = meth.GetILGenerator()
        gen.Emit(OpCodes.Sizeof, typ)
        gen.Emit(OpCodes.Ret)
        meth.CreateDelegate(typeof<Func<uint32>>).DynamicInvoke() |> unbox

    let numericSizeOf (typ: Type) : uint32 =
        let typ = if typ.IsEnum then typ.GetEnumUnderlyingType() else typ
        assert(isNumeric typ)
        match typ with
        | _ when typ = typeof<int8> -> 8u
        | _ when typ = typeof<uint8> -> 8u
        | _ when typ = typeof<int16> -> 16u
        | _ when typ = typeof<uint16> -> 16u
        | _ when typ = typeof<char> -> 16u
        | _ when typ = typeof<int32> -> 32u
        | _ when typ = typeof<uint32> -> 32u
        | _ when typ = typeof<float32> -> 32u
        | _ when typ = typeof<int64> -> 64u
        | _ when typ = typeof<uint64> -> 64u
        | _ when typ = typeof<float> -> 64u
        | _ -> __unreachable__()

    let isSubtypeOrEqual (t1 : Type) (t2 : Type) = t1 = t2 || t1.IsSubclassOf(t2)
    let isPointer (t : Type) = t.IsPointer || t = typeof<IntPtr> || t = typeof<UIntPtr>

    // --------------------------------------- Conversions ---------------------------------------

    let canConvert leftType rightType = isPrimitive leftType && isPrimitive rightType

    type private convType = delegate of obj -> obj

    let private convs = Dictionary<Type * Type, convType>()

    let private emitConv t =
        match t with
        | _ when t = typeof<SByte>      -> OpCodes.Conv_I1
        | _ when t = typeof<Int16>      -> OpCodes.Conv_I2
        | _ when t = typeof<Int32>      -> OpCodes.Conv_I4
        | _ when t = typeof<Int64>      -> OpCodes.Conv_I8
        | _ when t = typeof<Byte>       -> OpCodes.Conv_U1
        | _ when t = typeof<Char>       -> OpCodes.Conv_U2
        | _ when t = typeof<UInt16>     -> OpCodes.Conv_U2
        | _ when t = typeof<UInt32>     -> OpCodes.Conv_U4
        | _ when t = typeof<UInt64>     -> OpCodes.Conv_U8
        | _ when t = typeof<float32>    -> OpCodes.Conv_R4
        | _ when t = typeof<float>      -> OpCodes.Conv_R8
        | _ when t = typeof<Boolean>    -> __unreachable__()
        | _ when t = typeof<nativeint>  -> __unreachable__()
        | _ when t = typeof<unativeint> -> __unreachable__()
        | _                             -> __unreachable__()

    let private createNumericConv (fromType : Type) (toType : Type) =
        assert(isNumeric fromType && isNumeric toType)
        let args = [| typeof<obj> |]
        let conv = DynamicMethod("Conv", typeof<obj>, args)
        let il = conv.GetILGenerator(256)
        il.Emit(OpCodes.Ldarg_0)
        il.Emit(OpCodes.Unbox_Any, fromType)
        il.Emit(emitConv toType)
        il.Emit(OpCodes.Box, toType)
        il.Emit(OpCodes.Ret)
        conv.CreateDelegate(typeof<convType>) :?> convType

    let private getConv fromType toType = // TODO: initialize once all cases
        let result : convType ref = ref null
        if convs.TryGetValue((fromType, toType), result) then result.Value
        else
            let conv = createNumericConv fromType toType
            convs.Add((fromType, toType), conv)
            conv

    let private convNumeric value toType =
        let fromType = getTypeOfConcrete value
        let conv = getConv fromType toType
        conv.Invoke(value)

    let convert (value : obj) t =
        match t with
        // TODO: throws an exception when value = char, implement using Emit
        | _ when t = typeof<Boolean> || value.GetType() = typeof<Boolean> -> Convert.ChangeType(value, t)
        | _ when t.IsEnum -> Enum.ToObject(t, value)
        | _ -> convNumeric value t

    // --------------------------------------- Operation target type ---------------------------------------

    let failDeduceBinaryTargetType op x y =
        internalfailf "%s (%O x, %O y); is not a binary arithmetical operator" op x y

    let inline deduceComparisonTargetType _ _ = typeof<bool>

    let private deduceBinaryGenericTargetType op x y =
        let areSameButSignedAndUnsigned =
            isInt x && isUInt y
            || isUInt x && isInt y
            || isLong x && isULong y
            || isULong x && isLong y
        if isReal x || isReal y (* || areSameButSignedAndUnsigned *) then failDeduceBinaryTargetType op x y
        elif isLong x then x // DO NOT REORDER THESE elif's!
        elif isLong y then y
        elif isULong x then x
        elif isULong y then y
        elif isInt x then x
        elif isInt y then y
        elif isUInt x then x
        elif isUInt y then y
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
