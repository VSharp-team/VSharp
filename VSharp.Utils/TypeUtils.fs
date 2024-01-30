namespace VSharp

open EnumUtils
open System
open System.Collections.Generic
open System.Reflection
open System.Reflection.Emit
open FSharpx.Collections

[<StructuralEquality;StructuralComparison>]
type arrayDimensionType =
    | Vector
    | ConcreteDimension of int
    | SymbolicDimension

module TypeUtils =

    let private nativeSize = IntPtr.Size

    // ---------------------------------- Basic type groups ----------------------------------

    let private integralTypes =
        HashSet<Type>(
            [
                typeof<byte>; typeof<sbyte>; typeof<int16>; typeof<uint16>
                typeof<int32>; typeof<uint32>; typeof<int64>; typeof<uint64>;
                typeof<char>; typeof<IntPtr>; typeof<UIntPtr>
            ]
        )

    let private longTypes = HashSet<Type>([typeof<int64>; typeof<uint64>])

    let private unsignedTypes =
        HashSet<Type>(
            [typeof<byte>; typeof<uint16>; typeof<char>; typeof<uint32>; typeof<uint64>; typeof<UIntPtr>]
        )

    let private realTypes = HashSet<Type>([typeof<single>; typeof<double>])

    let private numericTypes = HashSet<Type>(Seq.append integralTypes realTypes)

    let private primitiveTypes = HashSet<Type>(Seq.append numericTypes [typeof<bool>])

    type AddressTypeAgent = struct end

    let indexType = typeof<int>
    let lengthType = typeof<int>
    let addressType = typeof<AddressTypeAgent>
    let systemRuntimeType = typeof<Object>.GetType()

    let szArrayHelper = lazy Type.GetType("System.SZArrayHelper")

    // ---------------------------------- Basic type predicates ----------------------------------

    let rec isPublic (x : Type) =
        assert(x <> null)
        x.IsPublic || x.IsNestedPublic && isPublic x.DeclaringType

    let isGround (x : Type) =
        (not x.IsGenericType && not x.IsGenericParameter) || x.IsConstructedGenericType

    let isNumeric x = numericTypes.Contains x || x.IsEnum
    let isIntegral x = integralTypes.Contains x || x.IsEnum
    let isLongType x = longTypes.Contains x
    let isReal x = realTypes.Contains x
    let isUnsigned x = unsignedTypes.Contains x
    let isSigned x = unsignedTypes.Contains x |> not
    let isPrimitive x = primitiveTypes.Contains x
    let isNative x = x = typeof<IntPtr> || x = typeof<UIntPtr>

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
        let widerThan8 =
            [|
                typeof<int32>; typeof<uint32>; typeof<int64>; typeof<uint64>
                typeof<int16>; typeof<uint16>; typeof<char>; typeof<float32>; typeof<float>
                typeof<IntPtr>; typeof<UIntPtr>
            |]
        let widerThan16 =
            [|
                typeof<int32>; typeof<uint32>; typeof<int64>
                typeof<float32>; typeof<uint64>; typeof<float>
                typeof<IntPtr>; typeof<UIntPtr>
            |]
        let widerThan32 =
            if nativeSize > sizeof<int> then
                [|typeof<int64>; typeof<uint64>; typeof<float>; typeof<IntPtr>; typeof<UIntPtr>|]
            else [|typeof<int64>; typeof<uint64>; typeof<float>|]
        let widerThan64 = [||]
        let widerThanNative =
            if nativeSize > sizeof<int> then widerThan64
            else widerThan32
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
            (typeof<float>,   widerThan64)
            (typeof<IntPtr>,  widerThanNative)
            (typeof<UIntPtr>, widerThanNative)
        ]

    let isLessForNumericTypes (t1 : Type) (t2 : Type) =
        let t1 = if t1.IsEnum then getEnumUnderlyingTypeChecked t1 else t1
        let t2 = if t2.IsEnum then getEnumUnderlyingTypeChecked t2 else t2
        assert(isNumeric t1 && isNumeric t2)
        Array.contains t2 isWiderForNumericTypesMap.[t1]

    let isDelegate typ = typeof<Delegate>.IsAssignableFrom typ

    let isImplementationDetails (t : Type) =
        t.FullName = "<PrivateImplementationDetails>"

    // ---------------------------------- Basic type operations ----------------------------------

    let inline getTypeOfConcrete value =
        if box value = null then null
        else value.GetType()

    type private sizeOfType = Func<uint32>

    let private sizeOfs = Dictionary<Type, sizeOfType>()

    let private createSizeOf (typ : Type) =
        assert(not typ.ContainsGenericParameters && typ <> typeof<Void>)
        let m = DynamicMethod("GetManagedSizeImpl", typeof<uint32>, null);
        let gen = m.GetILGenerator()
        gen.Emit(OpCodes.Sizeof, typ)
        gen.Emit(OpCodes.Ret)
        m.CreateDelegate(typeof<sizeOfType>) :?> sizeOfType

    let getSizeOf typ =
        let result : sizeOfType ref = ref null
        if sizeOfs.TryGetValue(typ, result) then result.Value
        else
            let sizeOf = createSizeOf typ
            sizeOfs.Add(typ, sizeOf)
            sizeOf

    let numericSizeOf (typ : Type) : uint32 =
        let typ = if typ.IsEnum then getEnumUnderlyingTypeChecked typ else typ
        assert(isNumeric typ)
        match typ with
        | _ when typ = typeof<int8> -> uint sizeof<int8>
        | _ when typ = typeof<uint8> -> uint sizeof<uint8>
        | _ when typ = typeof<int16> -> uint sizeof<int16>
        | _ when typ = typeof<uint16> -> uint sizeof<uint16>
        | _ when typ = typeof<char> -> uint sizeof<char>
        | _ when typ = typeof<int32> -> uint sizeof<int32>
        | _ when typ = typeof<uint32> -> uint sizeof<uint32>
        | _ when typ = typeof<float32> -> uint sizeof<float32>
        | _ when typ = typeof<int64> -> uint sizeof<int64>
        | _ when typ = typeof<uint64> -> uint sizeof<uint64>
        | _ when typ = typeof<float> -> uint sizeof<float>
        | _ when typ = typeof<IntPtr> -> uint nativeSize
        | _ when typ = typeof<UIntPtr> -> uint nativeSize
        | _ -> __unreachable__()

    let internalSizeOf (typ : Type) : int32 =
        if isNative typ || typ.IsByRef || not typ.IsValueType then nativeSize
        elif isNumeric typ then numericSizeOf typ |> int
        elif typ.ContainsGenericParameters then
            __insufficientInformation__ $"SizeOf: cannot calculate size of generic type {typ}"
        else
            let sizeOf = getSizeOf(typ)
            sizeOf.Invoke() |> int

    let numericBitSizeOf (typ : Type) : uint32 =
        numericSizeOf typ * 8u

    let bitSizeOfType t (resultingType : Type) =
        let size = internalSizeOf(t) * 8
        Convert.ChangeType(size, resultingType)

    let isSubtypeOrEqual (t1 : Type) (t2 : Type) = t2.IsAssignableFrom(t1)
    let isPointer (t : Type) = t.IsPointer

    let isValueType = function
        | (t : Type) when t.IsGenericParameter ->
            if isValueTypeParameter t then true
            elif isReferenceTypeParameter t then false
            else __insufficientInformation__ "Can't determine if %O is a value type or not!" t
        | t -> t.IsValueType && t <> addressType && t <> typeof<Void>

    let isNullable = function
        | (t : Type) when t.IsGenericParameter ->
            if isReferenceTypeParameter t then false
            else __insufficientInformation__ "Can't determine if %O is a nullable type or not!" t
        | t -> Nullable.GetUnderlyingType(t) <> null

    let isBoxedType t =
        t = typeof<obj> || t.IsInterface || isValueType t || t = typeof<Enum>

    let getGenericArgs (t : Type) =
        if t.IsGenericType then t.GetGenericArguments() else [||]

    let rec getSupertypes (t : Type) =
        if t = null then []
        else t :: getSupertypes t.BaseType

    let isGenericType (t : Type) = t.IsGenericType

    let isGenericParameter (t : Type) = t.IsGenericParameter

    let containsGenericParameters (t : Type) = t.ContainsGenericParameters

    let getTypeDef (t : Type) =
        if t.IsGenericType then t.GetGenericTypeDefinition() else t

    let (|Numeric|_|) = function
        | t when isNumeric t -> Some(t)
        | _ -> None

    let (|Void|_|) = function
        | t when t = typeof<Void> -> Some()
        | _ -> None

    let (|Bool|_|) = function
        | t when t = typeof<bool> -> Some()
        | _ -> None

    let (|Char|_|) = function
        | t when t = typeof<char> -> Some()
        | _ -> None

    let (|AddressType|_|) = function
        | t when t = addressType -> Some()
        | _ -> None

    let (|StringType|_|) = function
        | typ when typ = typeof<string> -> Some()
        | _ -> None

    let (|Native|_|) = function
        | typ when isNative typ -> Some()
        | _ -> None

    let private getGenericDefinition (dotNetType : Type) =
        if dotNetType.IsGenericType then
            dotNetType.GetGenericTypeDefinition()
        else dotNetType

    let private getGenericArguments (t : Type) =
        if t.IsGenericType then t.GetGenericArguments()
        else [||]

    let (|StructType|_|) = function
        | (t : Type) when t.IsValueType && not (isPrimitive t) && not t.IsEnum && not t.IsGenericParameter ->
            Some(getGenericDefinition t, getGenericArguments t)
        | _ -> None

    let (|ClassType|_|) = function
        | (t : Type) when t.IsClass && not t.IsByRef && not t.IsPointer && not t.IsArray && t <> typeof<Array> ->
            Some(getGenericDefinition t, getGenericArguments t)
        | _ -> None

    let (|InterfaceType|_|) = function
        | (t : Type) when t.IsInterface -> Some(getGenericDefinition t, getGenericArguments t)
        | _ -> None

    let (|TypeVariable|_|) = function
        | (t : Type) when t.IsGenericParameter -> Some(t)
        | _ -> None

    let (|ArrayType|_|) = function
        | (a : Type) when a = typeof<Array> -> Some(typeof<obj>, SymbolicDimension)
        | a when a.IsArray ->
            let dim = if a = a.GetElementType().MakeArrayType() then Vector else ConcreteDimension <| a.GetArrayRank()
            Some(ArrayType(a.GetElementType(), dim))
        | _ -> None

    let (|ValueType|_|) = function
        | StructType(t, [||]) when t = addressType -> None
        | StructType _
        | Numeric _
        | Bool -> Some(ValueType)
        | TypeVariable t when isValueTypeParameter t -> Some(ValueType)
        | _ -> None

    let (|Pointer|_|) (t : Type) =
        match t with
        | _ when t.IsPointer -> Some(t.GetElementType())
        | _ -> None

    let (|ByRef|_|) = function
        | (t : Type) when t.IsByRef -> Some(t.GetElementType())
        | _ -> None

    let (|ReferenceType|_|) = function
        | ClassType _
        | InterfaceType _
        | ArrayType _ -> Some(ReferenceType)
        | TypeVariable t when isReferenceTypeParameter t -> Some(ReferenceType)
        | _ -> None

    let (|ComplexType|_|) = function
        | ValueType
        | ReferenceType
        | TypeVariable _ -> Some(ComplexType)
        | _ -> None

    let (|Covariant|Invariant|Contravariant|) (parameter : Type) =
        assert parameter.IsGenericParameter
        if parameter.GenericParameterAttributes &&& GenericParameterAttributes.Contravariant = GenericParameterAttributes.Contravariant then Contravariant
        elif parameter.GenericParameterAttributes &&& GenericParameterAttributes.Covariant = GenericParameterAttributes.Covariant then Covariant
        else Invariant


    let elementType = function
        | ArrayType(t, _) -> t
        | t -> internalfailf "expected array type, but got %O" t

    let isArrayType = function
        | ArrayType _ -> true
        | _ -> false

    let isStruct = function
        | StructType _ -> true
        | _ -> false

    let rec isOpenType = function
        | TypeVariable _ -> true
        | StructType(_, parameters) when Array.exists isOpenType parameters -> true
        | ClassType(_, parameters) when Array.exists isOpenType parameters -> true
        | InterfaceType(_, parameters) when Array.exists isOpenType parameters -> true
        | ArrayType(t, _) when isOpenType t -> true
        | _ -> false

    let rankOf = function
        | ArrayType(_, dim) ->
            match dim with
            | Vector -> 1
            | ConcreteDimension d -> d
            | SymbolicDimension -> __insufficientInformation__ "Can't get precise array rank of System.Array object!"
        | t -> internalfail $"Getting rank of an array: expected array type, but got {t}"

    // Performs syntactical unification of types, returns the infimum of x and y (where x < y iff x is more generic).
    let rec structuralInfimum (x : Type) (y : Type) =
        if x = y then Some x
        else
            if x.IsGenericParameter then Some y
            elif y.IsGenericParameter then Some x
            elif not x.IsGenericType || not y.IsGenericType then None
            elif x.GetGenericTypeDefinition() <> y.GetGenericTypeDefinition() then None
            else
                let xargs = x.GetGenericArguments()
                let yargs = y.GetGenericArguments()
                assert(xargs.Length = yargs.Length)
                let infs = Array.map2 structuralInfimum xargs yargs
                if Array.forall Option.isSome infs then Some (x.GetGenericTypeDefinition().MakeGenericType(Array.map Option.get infs))
                else None

    let rec getAllInterfaces (t : Type) =
        seq {
            let bases = t.GetInterfaces()
            yield! bases
            for b in bases do
                yield! getAllInterfaces b
        } |> Array.ofSeq

    let typeImplementsInterface (t : Type) (targetInterface : Type) =
        assert(targetInterface.IsInterface)
        let matches (i : Type) =
            i = targetInterface
            || i.IsGenericType && targetInterface.IsGenericType
            && i.GetGenericTypeDefinition() = targetInterface.GetGenericTypeDefinition()
        getAllInterfaces t |> Array.exists matches

    // [NOTE] there is no enums, because pushing to evaluation stack causes cast
    let signedToUnsigned = function
        | t when t = typeof<int32> || t = typeof<uint32> -> typeof<uint32>
        | t when t = typeof<int8> || t = typeof<uint8> -> typeof<uint8>
        | t when t = typeof<int16> || t = typeof<uint16> -> typeof<uint16>
        | t when t = typeof<int64> || t = typeof<uint64> -> typeof<uint64>
        | t when t = typeof<IntPtr> || t = typeof<UIntPtr> -> typeof<UIntPtr>
        | t -> internalfail $"signedToUnsigned: unexpected type {t}"

    let unsignedToSigned = function
        | t when t = typeof<uint32> || t = typeof<int32> -> typeof<int32>
        | t when t = typeof<uint8> || t = typeof<int8> -> typeof<int8>
        | t when t = typeof<uint16> || t = typeof<int16> || t = typeof<char> -> typeof<int16>
        | t when t = typeof<uint64> || t = typeof<int64> -> typeof<int64>
        | t when t = typeof<UIntPtr> || t = typeof<IntPtr> -> typeof<IntPtr>
        | t -> internalfail $"signedToUnsigned: unexpected type {t}"

    let numericSameSign t1 t2 =
        isUnsigned t1 && isUnsigned t2 || isSigned t1 && isSigned t2

    let private solidTypes = [
        typeof<Type>
        systemRuntimeType
        typeof<System.Threading.Thread>
        typeof<System.Diagnostics.Tracing.EventSource>
        typeof<FieldInfo>
        typeof<System.Reflection.Pointer>
    ]

    let isSolidType (typ : Type) =
        List.exists typ.IsAssignableTo solidTypes

    // --------------------------------------- Conversions ---------------------------------------

    let canConvert leftType rightType =
        let lPrimitive = isPrimitive leftType
        let rPrimitive = isPrimitive rightType
        let enumCase() =
            (rPrimitive && leftType.IsEnum || lPrimitive && rightType.IsEnum)
            && internalSizeOf leftType = internalSizeOf rightType
        lPrimitive && rPrimitive || enumCase()

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
        | _ when t = typeof<nativeint>  -> OpCodes.Conv_I
        | _ when t = typeof<unativeint> -> OpCodes.Conv_U
        | _ when t = typeof<Boolean>    -> __unreachable__()
        | _                             -> __unreachable__()

    let private createNumericConv (fromType : Type) (toType : Type) =
        assert(isNumeric fromType && isNumeric toType)
        let args = [| typeof<obj> |]
        let conv = DynamicMethod($"Conv {fromType} {toType}", typeof<obj>, args)
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

    let private convNumeric value actualType toType =
        let conv = getConv actualType toType
        conv.Invoke(value)

    let convert (value : obj) t =
        if value = null then value
        else
            let actualType = value.GetType()
            match t with
            // TODO: throws an exception when value = char, implement using Emit
            | _ when actualType = t -> value
            | _ when t = typeof<Boolean> || actualType = typeof<Boolean> -> Convert.ChangeType(value, t)
            | _ when t.IsEnum -> Enum.ToObject(t, value)
            | _ when actualType.IsEnum -> Convert.ChangeType(value, t)
            | _ -> convNumeric value actualType t

    // --------------------------------------- Subtyping ---------------------------------------

    // [NOTE] All heuristics of subtyping are here
    let rec private commonConcreteCanCast canCast leftType rightType certainK uncertainK =
        match leftType, rightType with
        | _ when leftType = rightType -> certainK true
        | ArrayType _, ClassType(obj, _) -> obj = typeof<obj> |> certainK
        | Numeric t1, Numeric t2 -> canCast t1 t2 |> certainK
        // NOTE: Managed pointers (refs), unmanaged pointers (ptr) are specific kinds of numbers
        // NOTE: Numeric zero may may be treated as ref or ptr
        | Numeric t, Pointer _
        | Pointer _, Numeric t
        | Numeric t, ByRef _
        | ByRef _, Numeric t when t = typeof<IntPtr> || t = typeof<UIntPtr> -> certainK true
        | Pointer _, Pointer _ -> certainK true
        | ByRef t1, ByRef t2 -> commonConcreteCanCast canCast t1 t2 certainK uncertainK
        // NOTE: *void cannot be used for read, so we can store refs there
        | ByRef _, Pointer Void -> certainK true
        // NOTE: need subtype relation between 't1' and 't2', because while read by this pointer we should read type 't2'
        | ByRef t1, Pointer t2 -> commonConcreteCanCast canCast t1 t2 certainK uncertainK
        // NOTE: void* can be stored in pinned references (fixed managed pointers)
        | Pointer Void, ByRef _ -> certainK true
        // NOTE: pointers can be stored in pinned references (fixed managed pointers)
        | Pointer t1, ByRef t2 -> commonConcreteCanCast canCast t1 t2 certainK uncertainK
        | ArrayType _, ArrayType(_, SymbolicDimension) -> certainK true
        | ArrayType(t1, ConcreteDimension d1), ArrayType(t2, ConcreteDimension d2) ->
            // TODO: check 'is' for int[] and long[] (it must be false) #do
            if d1 = d2 then commonConcreteCanCast canCast t1 t2 certainK uncertainK else certainK false
        | _ when leftType.IsByRefLike -> certainK false
        | _ when rightType.IsByRefLike -> certainK false
        | ComplexType, ComplexType ->
            if canCast leftType rightType then certainK true
            elif isGround leftType && isGround rightType then certainK false
            else uncertainK leftType rightType
        | _ -> certainK false

    let isConcreteSubtype leftType rightType =
        let canCast lType (rType : Type) = rType.IsAssignableFrom(lType)
        commonConcreteCanCast canCast leftType rightType

    // Works like isVerifierAssignable in .NET specification
    let isAssignable leftType rightType =
        isConcreteSubtype leftType rightType id (fun _ _ -> false)

    let canCastImplicitly leftType rightType =
        let canCast lType (rType : Type) = rType.IsAssignableFrom(lType) || canConvert lType rType
        commonConcreteCanCast canCast leftType rightType id (fun _ _ -> false)

    /// If 'leftType' is assignable to 'rightType' and 'rightType' is assignable to 'leftType',
    /// 'mostConcreteType' will return 'leftType'
    /// Example: mostConcreteType typeof<uint array> typeof<int array> == typeof<uint array>
    let inline mostConcreteType (leftType : Type) (rightType : Type) =
        let isStringCase() =
            rightType = typeof<string> && leftType = typeof<char[]>
            || leftType = typeof<string> && rightType = typeof<char[]>
        if leftType = null then rightType
        elif rightType = null then leftType
        elif rightType.IsAssignableFrom(leftType) then leftType
        elif isStringCase() then typeof<string>
        else
            assert leftType.IsAssignableFrom(rightType)
            rightType
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
        elif isNative x then x
        elif isNative y then y
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
        if not <| isInt y then fail() // DO NOT REORDER THESE elif's!
        elif isInt x || isUInt x || isLong x || isULong x then x
        elif isIntegral x then typeof<int32>
        else fail()
