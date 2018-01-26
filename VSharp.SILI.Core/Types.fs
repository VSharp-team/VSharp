namespace VSharp.Core

open VSharp
open global.System
open System.Collections.Generic
open System.Reflection

type variance =
    | Contravariant
    | Covariant
    | Invarinat

[<StructuralEquality;NoComparison>]
type arrayDimensionType =
    | Vector
    | ConcreteDimension of int
    | SymbolicDimension of string

[<StructuralEquality;NoComparison>]
type termType =
    | Void
    | Bottom
    | Null
    | Bool
    | Numeric of System.Type
    | String
    | StructType of hierarchy * termTypeRef list * termType list        // Value type with generic argument and interfaces
    | ClassType of hierarchy * termTypeRef list * termType list         // Reference type with generic argument and interfaces
    | InterfaceType of hierarchy * termTypeRef list * termType list     // Interface type with generic argument and interfaces
    | TypeVariable of typeId
    | ArrayType of termType * arrayDimensionType
    | Func of termType list * termType
    | Reference of termType
    | Pointer of termType // C-style pointers like int*

    override x.ToString() =
        match x with
        | Void -> "void"
        | Bottom -> "exception"
        | Null -> "<nullType>"
        | Bool -> "bool"
        | Numeric t -> t.Name.ToLower()
        | String -> "string"
        | Func(domain, range) -> String.Join(" -> ", List.append domain [range])
        | StructType(t, _, _)
        | ClassType(t, _, _)
        | InterfaceType(t, _, _)
        | TypeVariable(Implicit t) -> toString t
        | TypeVariable(Explicit (name, t)) -> sprintf "%s{%O}" name t
        | ArrayType(t, Vector) -> t.ToString() + "[]"
        | ArrayType(t, ConcreteDimension 1) -> t.ToString() + "[*]"
        | ArrayType(t, ConcreteDimension rank) -> t.ToString() + "[" + new string(',', rank - 1) + "]"
        | ArrayType(_, SymbolicDimension name) -> name
        | Reference t -> sprintf "<Reference to %O>" t
        | Pointer t -> sprintf "<Pointer to %O>" t

and [<CustomEquality;NoComparison>]
    termTypeRef =
        | TermTypeRef of termType ref
        override x.GetHashCode() =
            Microsoft.FSharp.Core.LanguagePrimitives.PhysicalHash(x)
        override x.Equals(o : obj) =
            match o with
            | :? termTypeRef as other -> x.GetHashCode() = other.GetHashCode()
            | _ -> false

and [<StructuralEquality;NoComparison>]
    typeId =
        | Implicit of hierarchy
        | Explicit of string * termType

module internal Types =
    let (|StructType|_|) = function
        | StructType(t, g ,i) -> Some(StructType(t.Inheritor, g, i))
        | _ -> None

    let (|ClassType|_|) = function
        | ClassType(t, g ,i) -> Some(ClassType(t.Inheritor, g, i))
        | _ -> None

    let (|InterfaceType|_|) = function
        | InterfaceType(t, g ,i) -> Some(InterfaceType(t.Inheritor, g, i))
        | _ -> None

    let pointerType = Numeric typedefof<int>

    let private integerTypes =
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

    let private numericTypes = new HashSet<System.Type>(Seq.append integerTypes realTypes)

    let private primitiveTypes = new HashSet<Type>(Seq.append numericTypes [typedefof<bool>])

    let isNumeric = function
        | Numeric _ -> true
        | _ -> false

    let isBool = function
        | Bool -> true
        | _ -> false

    let isString = function
        | String -> true
        | _ -> false

    let isFunction = function
        | Func _ -> true
        | _ -> false

    let isClass = function
        | ClassType _ -> true
        | _ -> false

    let isStruct = function
        | StructType _ -> true
        | _ -> false

    let isArray = function
        | ArrayType _ -> true
        | _ -> false

    let isObject = function
        | ClassType(t, _, _) when t = typedefof<obj> -> true
        | _ -> false

    let isVoid = function
        | Void -> true
        | _ -> false

    let isBottom = function
        | Bottom -> true
        | _ -> false

    let isNull = function
        | Null -> true
        | _ -> false

    let isReference = function
        | Reference _ -> true
        | _ -> false

    let isPointer = function
        | Pointer _ -> true
        | _ -> false

    let domainOf = function
        | Func(domain, _) -> domain
        | _ -> []

    let rangeOf = function
        | Func(_, range) -> range
        | t -> t

    let elementType = function
        | ArrayType(t, _) -> t
        | t -> internalfailf "expected array type, but got %O" t

    let rec isReferenceType = function
        | String
        | ClassType _
        | InterfaceType _
        | ArrayType _
        | Func _ -> true
        | TypeVariable(Implicit t) when not t.Inheritor.IsValueType-> true
        | TypeVariable(Explicit(_, t)) -> isReferenceType t
        | _ -> false

    let isValueType = not << isReferenceType

    let wrapReferenceType = function
        | t when isReferenceType t -> Reference t
        | t -> t

    let rec toDotNetType t =
        match t with
        | Null -> null
        | Bool -> typedefof<bool>
        | String -> typedefof<string>
        | Numeric t
        | StructType(t, _, _)
        | InterfaceType(t, _, _)
        | ClassType(t, _, _) -> t
        | TypeVariable(Implicit t) -> t.Inheritor
        | ArrayType(_, SymbolicDimension _) -> typedefof<System.Array>
        | ArrayType(t, Vector) -> (toDotNetType t).MakeArrayType()
        | ArrayType(t, ConcreteDimension rank) -> (toDotNetType t).MakeArrayType(rank)
        | Reference t -> toDotNetType t
        | Pointer t -> (toDotNetType t).MakePointerType()
        | _ -> typedefof<obj>

    let sizeOf typ = // Reflection hacks, don't touch! Marshal.SizeOf lies!
        let internalSizeOf (typ: Type) : uint32 =
            let meth = new Reflection.Emit.DynamicMethod("GetManagedSizeImpl", typeof<uint32>, null);

            let gen = meth.GetILGenerator()
            gen.Emit(Reflection.Emit.OpCodes.Sizeof, typ)
            gen.Emit(Reflection.Emit.OpCodes.Ret)

            meth.CreateDelegate(typeof<Func<uint32>>).DynamicInvoke()
            |> unbox
        typ |> toDotNetType |> internalSizeOf |> int


    let bitSizeOfType t (resultingType : System.Type) = System.Convert.ChangeType(sizeOf(t) * 8, resultingType)


    module internal Constructor =
        let private StructType (t : Type) g i = StructType(hierarchy t, g, i)
        let private ClassType (t : Type) g i = ClassType(hierarchy t, g, i)
        let private InterfaceType (t : Type) g i = InterfaceType(hierarchy t, g, i)
        let private Implicit (t : Type) = Implicit(hierarchy t)

        module private TypesCache =

            let private types = new Dictionary<System.Type, termType ref>()

            let contains t = types.ContainsKey t
            let prepare t = types.Add (t, ref Null)
            let find t = types.[t]

            let embody t value =
                types.[t] := value
                types.[t]

        let getVariance (genericParameterAttributes : GenericParameterAttributes) =
            let (==>) (left : GenericParameterAttributes) (right : GenericParameterAttributes) =
                left &&& right = right
            let variance = genericParameterAttributes &&& GenericParameterAttributes.VarianceMask
            match variance with
            | _ when variance ==> GenericParameterAttributes.Contravariant -> Contravariant
            | _ when variance ==> GenericParameterAttributes.Covariant -> Covariant
            | _ -> Invarinat

        let rec private getGenericArguments (dotNetType : Type) =
            if dotNetType.IsGenericType then
                Seq.map (TermTypeRef << fromDotNetTypeRef) (dotNetType.GetGenericArguments()) |>
                List.ofSeq
            else []

        and private makeInterfaceType (interfaceType : Type) =
            let genericArguments = getGenericArguments interfaceType
            InterfaceType interfaceType genericArguments []

        and private getInterfaces (dotNetType : Type) = dotNetType.GetInterfaces() |> Seq.map makeInterfaceType |> List.ofSeq

        and private fromCommonDotNetType (dotNetType : Type) =
            match dotNetType with
            | null -> Null
            | p when p.IsPointer -> p.GetElementType() |> fromCommonDotNetType |> Pointer
            | v when v.FullName = "System.Void" -> Void
            | a when a.FullName = "System.Array" -> ArrayType(fromCommonDotNetType typedefof<obj>, SymbolicDimension "System.Array")
            | b when b.Equals(typedefof<bool>) -> Bool
            | n when numericTypes.Contains(n) -> Numeric n
            | s when s.Equals(typedefof<string>) -> String
            | e when e.IsEnum -> Numeric e
            | a when a.IsArray ->
                ArrayType(
                    fromCommonDotNetType (a.GetElementType()) |> wrapReferenceType,
                    if a = a.GetElementType().MakeArrayType() then Vector else ConcreteDimension <| a.GetArrayRank())
            | s when s.IsValueType && not s.IsGenericParameter-> StructType s (getGenericArguments s) (getInterfaces s)
            | f when f.IsSubclassOf(typedefof<System.Delegate>) ->
                let methodInfo = f.GetMethod("Invoke")
                let returnType = methodInfo.ReturnType |> fromCommonDotNetType
                let parameters = methodInfo.GetParameters() |>
                                    Seq.map (fun (p : System.Reflection.ParameterInfo) ->
                                    fromCommonDotNetType p.ParameterType)
                Func(List.ofSeq parameters, returnType)
            | p when p.IsGenericParameter -> fromDotNetGenericParameter p
            | c when c.IsClass -> ClassType c (getGenericArguments c) (getInterfaces c)
            | i when i.IsInterface -> makeInterfaceType i
            | _ -> __notImplemented__()

        and private fromDotNetGenericParameterConstraint (dotNetType : Type) =
            match dotNetType with
            | g when g.IsGenericParameter ->
                fromDotNetGenericParameter g :: fromDotNetGenericParameterConstraints (g.GetGenericParameterConstraints())
            | i when i.IsInterface -> makeInterfaceType i |> List.singleton
            | _ -> List.Empty

        and private fromDotNetGenericParameterConstraints (constraints : Type[]) =
            constraints |> Seq.collect fromDotNetGenericParameterConstraint |> Seq.distinct |> List.ofSeq

        and private fromDotNetGenericParameter (genericParameter : Type) : termType =
            TypeVariable(Implicit genericParameter)

        and private fromDotNetTypeRef dotNetType =
            let key = dotNetType
            let res =
                if TypesCache.contains key then TypesCache.find key
                else
                    TypesCache.prepare key
                    let termType = fromCommonDotNetType dotNetType
                    TypesCache.embody key termType
            res

        let fromDotNetType (dotNetType : System.Type) =  if dotNetType = null then Null else !(fromDotNetTypeRef dotNetType)

        let (|StructureType|_|) = function
            | termType.StructType(t, genArg, interfaces) -> Some(StructureType(t, genArg, interfaces))
            | Numeric t -> Some(StructureType(hierarchy t, [], getInterfaces t))
            | Bool -> Some(StructureType(hierarchy typedefof<bool>, [], getInterfaces typedefof<bool>))
            | TypeVariable(Implicit t) when t.Inheritor.IsValueType -> Some(StructureType(t, [], getInterfaces t.Inheritor))
            | _ -> None

        let (|ReferenceType|_|) = function
            | String -> Some(ReferenceType(hierarchy typedefof<string>, [], getInterfaces typedefof<string>))
            | termType.ClassType(t, genArg, interfaces) -> Some(ReferenceType(t, genArg, interfaces))
            | termType.InterfaceType(t, genArg, interfaces) -> Some(ReferenceType(t, genArg, interfaces))
            | termType.ArrayType _ as arr ->
                let t = toDotNetType arr
                Some(ReferenceType(hierarchy t, [], getInterfaces t))
            | _ -> None

        let (|ComplexType|_|) = function
            | StructureType(t, genArg, interfaces)
            | ReferenceType(t, genArg, interfaces) -> Some(ComplexType(t, genArg, interfaces))
            | TypeVariable(Implicit t)-> Some(ComplexType(t, [], getInterfaces t.Inheritor))
            | _ -> None

        let (|ConcreteType|_|) = function
            | ComplexType(t, _, _) -> Some(ConcreteType t)
            | _ -> None

    open Constructor

    module public Variable =
        let private typeVariabeName = "TypeVariable"
        let create termType () = TypeVariable(Explicit(IdGenerator.startingWith typeVariabeName, termType))

        let fromTermType termType =
            let updateDimension = function
                | SymbolicDimension _ -> SymbolicDimension (IdGenerator.startingWith "ArrayTypeVariable")
                | d -> d
            let rec getNewType = function
                | ArrayType(elemType, dim) ->
                    let newElemType = getNewType elemType
                    ArrayType(newElemType, updateDimension dim)
                | ConcreteType t as termType when t.Inheritor.IsSealed && not t.Inheritor.IsGenericParameter -> termType
                | termType -> create termType ()
            getNewType termType

        let fromDotNetType dotnetType =
            let termType = fromDotNetType dotnetType
            fromTermType termType

    let isPrimitive t =
        let dotNetType = toDotNetType t
        primitiveTypes.Contains dotNetType || dotNetType.IsEnum

    let isInteger = toDotNetType >> integerTypes.Contains

    let isReal = toDotNetType >> realTypes.Contains

    let isUnsigned = unsignedTypes.Contains

    let rec isAssignableToGenericType (givenType : Type) (genericType : Type) =
        let areInterfacesFound =
            givenType.GetInterfaces() |>
            Seq.exists (fun it -> it.IsGenericType && it.GetGenericTypeDefinition() = genericType)
        areInterfacesFound ||
            let baseType = givenType.BaseType
            baseType <> null &&
            if baseType.IsGenericType then baseType.GetGenericTypeDefinition() = genericType
            else isAssignableToGenericType baseType genericType

    let private updateConstraints constraints = function
        | termType.ClassType(t, g, _) -> ClassType(t, g, constraints)
        | termType.StructType(t, g, _) -> StructType(t, g, constraints)
        | _ -> __unreachable__()

    let rec fieldsOf (t : System.Type) isStatic =
        let staticFlag = if isStatic then BindingFlags.Static else BindingFlags.Instance
        let flags = BindingFlags.Public ||| BindingFlags.NonPublic ||| staticFlag
        let fields = t.GetFields(flags)
        let extractFieldInfo (field : FieldInfo) =
            // Events may appear at this point. Filtering them out...
            if field.FieldType.IsSubclassOf(typeof<MulticastDelegate>) then None
            else
                let fieldName = sprintf "%s.%s" ((safeGenericTypeDefinition field.DeclaringType).FullName) field.Name
                Some (fieldName, fromDotNetType field.FieldType)
        let ourFields = fields |> FSharp.Collections.Array.choose extractFieldInfo
        if isStatic || t.BaseType = null then ourFields
        else Array.append (fieldsOf t.BaseType false) ourFields
