namespace VSharp

open global.System
open System.Collections.Generic
open System.Reflection
open JetBrains.Metadata.Reader.API
    
[<StructuralEquality;NoComparison>]
type public TermType =
    | Void
    | Bottom
    | Null
    | Bool
    | Numeric of System.Type
    | String
    | StructType of System.Type * TermTypeRef list * TermType list // some value type with generic argument and interfaces
    | ClassType of System.Type * TermTypeRef list * TermType list // some reference type with generic argument and interfaces
    | SubType of System.Type * TermTypeRef list * TermType list * string //some symbolic type with generic argument, interfaces and constraint
    | ArrayType of TermType * int
    | Func of TermType list * TermType
    | PointerType of TermType

    override this.ToString() =
        match this with
        | Void -> "void"
        | Bottom -> "exception"
        | Null -> "<nullType>"
        | Bool -> "bool"
        | Numeric t -> t.Name.ToLower()
        | String -> "string"
        | Func(domain, range) -> String.Join(" -> ", List.append domain [range])
        | StructType(t, _, _)
        | ClassType (t, _, _) -> toString t
        | SubType(t, _, _, name) -> sprintf "<Subtype of %O>" t
        | ArrayType(t, rank) -> t.ToString() + "[" + new string(',', rank) + "]"
        | PointerType t -> sprintf "<Pointer to %O>" t

and [<CustomEquality;NoComparison>]
    TermTypeRef =
        | TermTypeRef of TermType ref
        override this.GetHashCode() =
            Microsoft.FSharp.Core.LanguagePrimitives.PhysicalHash(this)
        override this.Equals(o : obj) =
            match o with
            | :? TermTypeRef as other -> this.GetHashCode() = other.GetHashCode()
            | _ -> false

module public Types =
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

    let private primitiveTypes = new HashSet<Type>(Seq.append numericTypes [typedefof<bool>; typedefof<string>])
        
    let rec public IsAssignableToGenericType (givenType : Type) (genericType : Type) =
        let isFindInterfaces =
            givenType.GetInterfaces() |> 
            Array.filter (fun it -> it.IsGenericType) |> 
            Array.map (fun it -> it.GetGenericTypeDefinition()) |>
            Array.exists (fun it -> it = genericType)
        isFindInterfaces ||
            let baseType = givenType.BaseType in
            baseType <> null &&
            if baseType.IsGenericType then baseType.GetGenericTypeDefinition() = genericType
            else IsAssignableToGenericType baseType genericType

    let public IsNumeric = function
        | Numeric _ -> true
        | _ -> false

    let public IsBool = function
        | Bool -> true
        | _ -> false

    let public IsString = function
        | String -> true
        | _ -> false

    let public IsFunction = function
        | Func _ -> true
        | _ -> false

    let public IsClass = function
        | ClassType _ -> true
        | _ -> false

    let public IsStruct = function
        | StructType _ -> true
        | _ -> false

    let public IsArray = function
        | ArrayType _ -> true
        | _ -> false

    let public IsObject = function
        | SubType(t, _, _, _) when t = typedefof<obj> -> true
        | _ -> false

    let public IsVoid = function
        | Void -> true
        | _ -> false

    let public IsBottom = function
        | Bottom -> true
        | _ -> false

    let public IsPointer = function
        | PointerType _ -> true
        | _ -> false

    let public DomainOf = function
        | Func(domain, _) -> domain
        | _ -> []

    let public RangeOf = function
        | Func(_, range) -> range
        | t -> t

    let public elementType = function
        | ArrayType(t, _) -> t
        | t -> internalfail ("expected array type, but got " + toString t)

    let public IsRelation = RangeOf >> IsBool

    let public IsReferenceType = function
        | String
        | ClassType _
        | ArrayType _
        | Func _
        | SubType _ -> true
        | _ -> false

    let public IsValueType = not << IsReferenceType

    let (|StructureType|_|) = function
        | StructType(t, genArg, interfaces) -> Some(StructureType(t, genArg, interfaces))
        | Numeric t -> Some(StructureType(t, [], []))
        | Bool -> Some(StructureType(typedefof<bool>, [], []))
        | _ -> None

    let (|ReferenceType|_|) = function
        | String -> Some(ReferenceType(typedefof<string>, [], []))
        | ClassType(t, genArg, interfaces) -> Some(ReferenceType(t, genArg, interfaces))
        | _ -> None

    let (|ComplexType|_|) = function
        | StructureType(t, genArg, interfaces)
        | ReferenceType(t, genArg, interfaces) -> Some(ComplexType(t, genArg, interfaces))
        | SubType(t, genArg, interfaces, _) -> Some(ComplexType(t, genArg, interfaces))
        | _ -> None

    let internal PointerFromReferenceType = function
        | t when IsReferenceType t -> PointerType t
        | t -> t

    let rec public ToDotNetType t =
        match t with
        | Null -> null
        | Bool -> typedefof<bool>
        | String -> typedefof<string>
        | Numeric t
        | StructType(t, _, _)
        | ClassType(t, _, _)
        | SubType(t, _, _, _) -> t
        | ArrayType(t, 0) -> typedefof<System.Array>
        | ArrayType(t, rank) -> (ToDotNetType t).MakeArrayType(rank)
        | PointerType t -> ToDotNetType t
        | _ -> typedefof<obj>

    let SizeOfNumeric x =
        System.Runtime.InteropServices.Marshal.SizeOf(ToDotNetType x)

    let BitSizeOf a typeOfA (t : System.Type) = System.Convert.ChangeType(SizeOfNumeric(typeOfA) * 8, t)

    module public Constructor =
        type private TypeKind =
            | Concrete
            | Unique
            | Global

        let private (|Symbolic|Concrete|) = function
            | Unique
            | Global -> Symbolic
            | Concrete -> Concrete

        let rec private getNameFromDotNetType termTypeParameter (dotNetType : Type) =
            match dotNetType with
            | null -> ""
            | g when g.IsGenericParameter ->
                match termTypeParameter with
                | Unique -> IdGenerator.startingWith g.Name
                | Concrete
                | Global -> g.Name
            | _ ->
                match termTypeParameter with
                | Unique -> IdGenerator.startingWith dotNetType.FullName
                | Concrete
                | Global -> dotNetType.FullName

        let private genericParameterFromMetadata (arg : IMetadataGenericArgument) =
            match arg with
            | _ when arg.TypeOwner <> null -> Type.GetType(arg.TypeOwner.AssemblyQualifiedName, true).GetGenericArguments().[int arg.Index]
            | _ when arg.MethodOwner <> null ->
                let metadataMethod = arg.MethodOwner in
                let lengthGenericArguments = metadataMethod.GenericArguments.Length in
                let parameters = metadataMethod.Parameters in
                let declaringType = metadataMethod.DeclaringType in
                let method =
                    Type.GetType(metadataMethod.DeclaringType.AssemblyQualifiedName, true).GetMethods() |>
                    Seq.filter (fun (m : MethodInfo) -> m.Name = metadataMethod.Name) |>
                    Seq.map (fun (m : MethodInfo) -> m, m.GetParameters(), m.GetGenericArguments().Length) |>
                    Seq.filter (fun (m, (p : ParameterInfo[]), gLen) -> (gLen = lengthGenericArguments) && (p.Length = parameters.Length)) |>
                    Seq.map (fun (m, p, _) -> (m, p)) |>
                    Seq.filter (fun (m, (p : ParameterInfo[])) ->
                        Seq.forall2 (fun (l : ParameterInfo) (r : IMetadataParameter) ->
                            l.ParameterType.FullName |? l.ParameterType.Name = r.Type.FullName) p parameters) |>
                    Seq.map fst |>
                    Array.ofSeq
                in
                method.[0].GetGenericArguments().[int arg.Index]
            | _ -> __notImplemented__()
    
        let rec public MetadataToDotNetType (arg : IMetadataType) =
            match arg with
            | null -> null
            | _ when arg.AssemblyQualifiedName = "__Null" -> null
            | _ when arg.FullName = "System.Void" -> typedefof<System.Void>
            | :? IMetadataGenericArgumentReferenceType as g -> genericParameterFromMetadata g.Argument
            | :? IMetadataArrayType as a -> (a.ElementType |> MetadataToDotNetType).MakeArrayType(int(a.Rank))
            | :? IMetadataClassType as c ->
                let originType = Type.GetType(c.Type.AssemblyQualifiedName, true) in
                if not originType.IsGenericType || Array.isEmpty c.Arguments then originType
                else originType.MakeGenericType(c.Arguments |> Array.map MetadataToDotNetType)
            | _ -> Type.GetType(arg.AssemblyQualifiedName, true)

        module private TypesCache =
            type private KeyType =
                | ConcreteKind
                | SymbolicKind

            let private fromTypeKind typeKind =
                match typeKind with
                | Concrete -> ConcreteKind
                | Symbolic -> SymbolicKind

            let private types = new Dictionary<System.Type * KeyType, TermType ref>()

            let public Contains (t, tk) = types.ContainsKey((t, fromTypeKind tk))

            let public Prepare (t, tk) = types.Add ((t, fromTypeKind tk), ref Null)

            let public Find (t, tk) = types.[(t, fromTypeKind tk)]

            let public Embody (t, tk) value =
                types.[(t, fromTypeKind tk)] := value
                types.[(t, fromTypeKind tk)]

        let rec private getGenericArguments typeKind (dotNetType : Type) =
            dotNetType.GetGenericArguments() |>
            Seq.map (fun t -> TermTypeRef <| fromDotNetTypeRef typeKind t) |>
            List.ofSeq

        and private fromDotNetInterface typeKind (interfaceType : Type) =
            let interfaces =
                interfaceType.GetInterfaces() |>
                Seq.map (fun t -> fromDotNetInterface typeKind t ) |>
                List.ofSeq
            in
            let genericArguments = getGenericArguments typeKind interfaceType in
            SubType(interfaceType, genericArguments, interfaces, interfaceType.FullName)

        and private getInterfaces typeKind (dotNetType : Type) = dotNetType.GetInterfaces() |> Array.map (fromDotNetInterface typeKind) |> Array.toList

        and private fromCommonDotNetType (dotNetType : Type) k =
            match dotNetType with
            | null -> Null
            | v when v.FullName = "System.Void" -> Void
            | b when b.Equals(typedefof<bool>) -> Bool
            | n when numericTypes.Contains(n) -> Numeric n
            | s when s.Equals(typedefof<string>) -> String
            | e when e.IsEnum -> Numeric e
            | a when a.IsArray -> ArrayType(fromCommonDotNetType (a.GetElementType()) k |> PointerFromReferenceType, a.GetArrayRank())
            | s when s.IsValueType && not s.IsGenericParameter-> StructType(s, getGenericArguments Concrete s, getInterfaces Concrete s)
            | f when f.IsSubclassOf(typedefof<System.Delegate>) ->
                let methodInfo = f.GetMethod("Invoke") in
                let returnType = methodInfo.ReturnType |> (fun t -> fromCommonDotNetType t k) in
                let parameters = methodInfo.GetParameters() |>
                                    Array.map (fun (p : System.Reflection.ParameterInfo) ->
                                    fromCommonDotNetType p.ParameterType k) in
                Func(List.ofArray parameters, returnType)
            | _ -> k dotNetType

        and private fromDotNetGenericParameter typeKind (genericParameter : Type) =
            let constraints = genericParameter.GetGenericParameterConstraints() in
            let fromGenericParameterOrInterface (dotNetType : Type) =
                match dotNetType with
                | g when g.IsGenericParameter -> fromDotNetGenericParameter typeKind g
                | i when i.IsInterface -> fromDotNetInterface typeKind i
            in
            let listTypeConstraint =
                constraints |>
                Array.filter (fun t -> t.IsInterface || t.IsGenericParameter) |>
                Array.map fromGenericParameterOrInterface |>
                Array.toList
            match genericParameter with
                | s when s.IsValueType -> StructType(genericParameter, [], listTypeConstraint)
                | _ -> match typeKind with
                        | Concrete -> ClassType(genericParameter, [], listTypeConstraint)
                        | _ -> SubType(genericParameter, [], listTypeConstraint, genericParameter.Name)

        and private fromDotNetTypeToConcrete dotNetType =
            fromCommonDotNetType dotNetType (fun concrete ->
                match concrete with
                | p when p.IsGenericParameter -> fromDotNetGenericParameter Concrete p
                // Actually interface is not nessesary a reference type, but if the implementation is unknown we consider it to be class (to check non-null).
                | c when c.IsClass -> ClassType(c, getGenericArguments Concrete c, getInterfaces Concrete c)
                | i when i.IsInterface -> fromDotNetInterface Concrete i
                | _ -> __notImplemented__())

        and private fromDotNetTypeToSymbolic typeKind dotNetType =
            fromCommonDotNetType dotNetType (fun symbolic ->
            match symbolic with
            | p when p.IsGenericParameter -> fromDotNetGenericParameter typeKind p
            | a when a.FullName = "System.Array" -> ArrayType(SubType(symbolic, [], [], a.FullName), int(0))
            | c when c.IsClass ->
                let interfaces = getInterfaces typeKind c
                let genericArguments = getGenericArguments typeKind c
                match c with
                | _ when c.IsSealed -> ClassType(c, genericArguments, interfaces)
                | _ -> SubType(c, genericArguments, interfaces, c.FullName)
            | i when i.IsInterface -> fromDotNetInterface typeKind i
            | _ -> __notImplemented__())

        and private fromDotNetType typeKind dotNetType =
            match typeKind with
            | Concrete -> fromDotNetTypeToConcrete dotNetType
            | Symbolic -> fromDotNetTypeToSymbolic typeKind dotNetType

        and private fromDotNetTypeRef (typeKind : TypeKind) dotNetType =
            let key = dotNetType, typeKind in
            let name = getNameFromDotNetType typeKind dotNetType in
            let res =
                if TypesCache.Contains key then (TypesCache.Find key)
                else
                    TypesCache.Prepare key
                    let termType = (fromDotNetType typeKind dotNetType) in
                    TypesCache.Embody key termType
            match !res with
            | SubType(t, a, p, n) -> ref <|SubType(t, a, p, name)
            | _ -> res

        let private FromDotNetType termTypeParameter dotNetType = ! (fromDotNetTypeRef termTypeParameter dotNetType)

        let rec private FromMetadataType typeKind (t : IMetadataType) =
            match t with
            | null -> SubType(typedefof<obj>, [], [], "unknown")
            | _ when t.AssemblyQualifiedName = "__Null" -> Null
            | _ when t.FullName = "System.Void" -> Void
            | :? IMetadataGenericArgumentReferenceType as g ->
                let arg = MetadataToDotNetType g in
                FromDotNetType typeKind arg
            | :? IMetadataArrayType as a ->
                let elementType = FromMetadataType typeKind a.ElementType |> PointerFromReferenceType in
                ArrayType(elementType, int(a.Rank))
            | :? IMetadataClassType as ct ->
                let dotnetType = MetadataToDotNetType ct in
                FromDotNetType typeKind dotnetType
            | _ -> Type.GetType(t.AssemblyQualifiedName, true) |> FromDotNetType typeKind

        let public FromConcreteDotNetType t = FromDotNetType Concrete t

        let public FromUniqueSymbolicDotNetType (t : Type) = FromDotNetType Unique t

        let public FromGlobalSymbolicDotNetType t = FromDotNetType Global t

        let public FromConcreteMetadataType t = FromMetadataType Concrete t

        let public FromUniqueSymbolicMetadataType (t : IMetadataType) = FromMetadataType Unique t

        let public FromGlobalSymbolicMetadataType t = FromMetadataType Global t

    open Constructor

    let public IsPrimitive t =
        let dotNetType = ToDotNetType t in
        primitiveTypes.Contains dotNetType || dotNetType.IsEnum

    let public IsInteger = ToDotNetType >> integerTypes.Contains

    let public IsReal = ToDotNetType >> realTypes.Contains

    let public IsUnsigned = unsignedTypes.Contains

    let public SystemGenericTypeDefinition (t : System.Type) =
        if t.IsGenericType && not <| t.IsGenericTypeDefinition then t.GetGenericTypeDefinition() else t

    let public FromDecompiledSignature (signature : JetBrains.Decompiler.Ast.IFunctionSignature) (returnMetadataType : IMetadataType) =
        let returnType = FromUniqueSymbolicMetadataType returnMetadataType in
        let paramToType (param : JetBrains.Decompiler.Ast.IMethodParameter) =
            param.Type |> FromGlobalSymbolicMetadataType in
        let args = Seq.map paramToType signature.Parameters |> List.ofSeq in
        Func(args, returnType)

    let public FromMetadataMethodSignature (m : IMetadataMethod) =
        let returnType = FromUniqueSymbolicMetadataType m.ReturnValue.Type in
        let paramToType (param : IMetadataParameter) =
            param.Type |> FromGlobalSymbolicMetadataType
        let args = Seq.map paramToType m.Parameters |> List.ofSeq in
        Func(args, returnType)

    let public GetMetadataTypeOfNode (node : JetBrains.Decompiler.Ast.INode) =
        DecompilerServices.getTypeOfNode node

    let public GetSystemTypeOfNode (node : JetBrains.Decompiler.Ast.INode) =
        let mt = GetMetadataTypeOfNode node in
        if mt = null then typedefof<obj>
        else MetadataToDotNetType mt

    let public GetFieldsOf (t : System.Type) isStatic =
        let staticFlag = if isStatic then BindingFlags.Static else BindingFlags.Instance in
        let flags = BindingFlags.Instance ||| BindingFlags.Public ||| BindingFlags.NonPublic ||| staticFlag in
        let fields = t.GetFields(flags) in
        let extractFieldInfo (field : FieldInfo) =
            let fieldName = sprintf "%s.%s" ((SystemGenericTypeDefinition field.DeclaringType).FullName) field.Name in
            (fieldName, FromConcreteDotNetType field.FieldType)
        fields |> Array.map extractFieldInfo |> Map.ofArray
