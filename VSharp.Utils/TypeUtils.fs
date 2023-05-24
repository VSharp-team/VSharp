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

    // ---------------------------------- Basic type groups ----------------------------------

    let private integralTypes =
        HashSet<Type>([typedefof<byte>; typedefof<sbyte>;
                       typedefof<int16>; typedefof<uint16>;
                       typedefof<int32>; typedefof<uint32>;
                       typedefof<int64>; typedefof<uint64>;
                       typedefof<char>])

    let private unsignedTypes =
        HashSet<Type>([typedefof<byte>; typedefof<uint16>;
                       typedefof<uint32>; typedefof<uint64>
                       typeof<UIntPtr>])

    let private realTypes = HashSet<Type>([typedefof<single>; typedefof<double>])

    let private numericTypes = HashSet<Type>(Seq.append integralTypes realTypes)

    let private primitiveTypes = HashSet<Type>(Seq.append numericTypes [typedefof<bool>])

    type AddressTypeAgent = struct end

    let indexType = typeof<int>
    let lengthType = typeof<int>
    let addressType = typeof<AddressTypeAgent>

    let szArrayHelper = lazy Type.GetType("System.SZArrayHelper")

    // ---------------------------------- Basic type predicates ----------------------------------

    let rec isPublic (x : Type) =
        x.IsPublic || x.IsNestedPublic && isPublic x.DeclaringType

    let isGround (x : Type) =
        (not x.IsGenericType && not x.IsGenericParameter) || x.IsConstructedGenericType

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
        let t1 = if t1.IsEnum then getEnumUnderlyingTypeChecked t1 else t1
        let t2 = if t2.IsEnum then getEnumUnderlyingTypeChecked t2 else t2
        assert(isNumeric t1 && isNumeric t2)
        Array.contains t2 isWiderForNumericTypesMap.[t1]

    let isDelegate typ = typeof<Delegate>.IsAssignableFrom typ

    // ---------------------------------- Basic type operations ----------------------------------

    let inline getTypeOfConcrete value =
        if box value = null then null
        else value.GetType()

    // TODO: wrap Type, cache size there
    let internalSizeOf (typ: Type) : int32 = // Reflection hacks, don't touch! Marshal.SizeOf lies!
        assert(not typ.ContainsGenericParameters)
        let meth = DynamicMethod("GetManagedSizeImpl", typeof<uint32>, null);
        let gen = meth.GetILGenerator()
        gen.Emit(OpCodes.Sizeof, typ)
        gen.Emit(OpCodes.Ret)
        let size : uint32 = meth.CreateDelegate(typeof<Func<uint32>>).DynamicInvoke() |> unbox
        int size

    let numericSizeOf (typ: Type) : uint32 =
        let typ = if typ.IsEnum then getEnumUnderlyingTypeChecked typ else typ
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

    let isSubtypeOrEqual (t1 : Type) (t2 : Type) = t2.IsAssignableFrom(t1)
    let isPointer (t : Type) = t.IsPointer || t = typeof<IntPtr> || t = typeof<UIntPtr>

    let isValueType = function
        | (t : Type) when t.IsGenericParameter ->
            if isValueTypeParameter t then true
            elif isReferenceTypeParameter t then false
            else __insufficientInformation__ "Can't determine if %O is a value type or not!" t
        | t -> t.IsValueType && t <> typeof<AddressTypeAgent>

    let isNullable = function
        | (t : Type) when t.IsGenericParameter ->
            if isReferenceTypeParameter t then false
            else __insufficientInformation__ "Can't determine if %O is a nullable type or not!" t
        | t -> Nullable.GetUnderlyingType(t) <> null

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
        | (t : Type) when t.IsClass && not t.IsByRef && not t.IsArray && t <> typeof<Array> ->
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
        | StructType(t, [||]) when t = typeof<AddressTypeAgent> -> None
        | StructType _
        | Numeric _
        | Bool -> Some(ValueType)
        | TypeVariable t when isValueTypeParameter t -> Some(ValueType)
        | _ -> None

    let (|Pointer|_|) = function
        | t when t = typeof<IntPtr> || t = typeof<UIntPtr> -> Some(typeof<Void>)
        | p when p.IsPointer -> Some(p.GetElementType())
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

    let bitSizeOfType t (resultingType : Type) = Convert.ChangeType(internalSizeOf(t) * 8, resultingType)

    let rankOf = function
        | ArrayType(_, dim) ->
            match dim with
            | Vector -> 1
            | ConcreteDimension d -> d
            | SymbolicDimension -> __insufficientInformation__ "Can't get precise array rank of System.Array object!"
        | t -> internalfailf "Getting rank of an array: expected array type, but got %O" t

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

    let typeImplementsInterface (t : Type) (targetInterface : Type) =
        assert(targetInterface.IsInterface)
        let matches (i : Type) =
            i = targetInterface ||
            i.IsGenericType && targetInterface.IsGenericType &&
                i.GetGenericTypeDefinition() = targetInterface.GetGenericTypeDefinition()
        t.GetInterfaces() |> Seq.exists matches

    let rec getBaseInterfaces (t : Type) =
        seq {
            let bases = t.GetInterfaces()
            yield! bases
            for b in bases do
                yield! getBaseInterfaces b
        }

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

    // --------------------------------------- Subtyping ---------------------------------------

    // [NOTE] All heuristics of subtyping are here
    let rec private commonConcreteCanCast canCast leftType rightType certainK uncertainK =
        match leftType, rightType with
        | _ when leftType = rightType -> certainK true
        | ArrayType _, ClassType(obj, _) -> obj = typedefof<obj> |> certainK
        | Numeric t, Numeric enum
        | Numeric enum, Numeric t when enum.IsEnum && numericSizeOf enum = numericSizeOf t -> certainK true
        // NOTE: Managed pointers (refs), unmanaged pointers (ptr) are specific kinds of numbers
        // NOTE: Numeric zero may may be treated as ref or ptr
        | Numeric _, Pointer _
        | Pointer _, Numeric _
        | Pointer _, Pointer _
        | Numeric _, ByRef _
        | ByRef _, Numeric _ -> certainK true
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
        if leftType = null then rightType
        elif rightType = null then leftType
        elif rightType.IsAssignableFrom(leftType) then leftType
        else
            assert(leftType.IsAssignableFrom(rightType))
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
