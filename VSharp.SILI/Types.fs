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
    | Object of string
    | Bool
    | Numeric of System.Type
    | String
    | StructType of System.Type // some value type
    | ClassType of System.Type // some reference type
    | SubType of System.Type * string
    | ArrayType of TermType * int
    | Func of TermType list * TermType
    | PointerType

    override this.ToString() =
        match this with
        | Void -> "void"
        | Bottom -> "exception"
        | Null -> "<nullType>"
        | Object name -> sprintf "<%s object>" name
        | Bool -> "bool"
        | Numeric t -> t.Name.ToLower()
        | String -> "string"
        | Func(domain, range) -> String.Join(" -> ", List.append domain [range])
        | StructType t -> t.ToString()
        | ClassType t -> t.ToString()
        | SubType(t, name) -> sprintf "<Subtype of %s>" (toString t)
        | ArrayType(t, rank) -> t.ToString() + "[" + new string(',', rank) + "]"
        | PointerType -> "<Pointer>"

module public Types =
    let private integerTypes =
        new HashSet<System.Type>(
                          [typedefof<byte>; typedefof<sbyte>;
                           typedefof<int16>; typedefof<uint16>;
                           typedefof<int32>; typedefof<uint32>;
                           typedefof<int64>; typedefof<uint64>;
                           typedefof<char>])

    let private realTypes =
        new HashSet<System.Type>([typedefof<single>; typedefof<double>; typedefof<decimal>])

    let private numericTypes = new HashSet<System.Type>(Seq.append integerTypes realTypes)

    let private primitiveTypes = new HashSet<Type>(Seq.append numericTypes [typedefof<bool>; typedefof<string>])

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
        | Object _ -> true
        | _ -> false

    let public IsVoid = function
        | Void -> true
        | _ -> false

    let public IsBottom = function
        | Bottom -> true
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
        | Object _
        | ClassType _
        | ArrayType _
        | Func _
        | SubType _ -> true
        | _ -> false

    let public IsValueType = not << IsReferenceType

    let (|StructureType|_|) = function
        | StructType t
        | Numeric t -> Some(StructureType(t))
        | Bool -> Some(StructureType(typedefof<bool>))
        | _ -> None

    let (|ReferenceType|_|) = function
        | String -> Some(ReferenceType(typedefof<string>))
        | ClassType t -> Some(ReferenceType(t))
        | _ -> None

    let (|ComplexType|_|) = function
        | StructureType t
        | ReferenceType t
        | SubType(t, _) -> Some(ComplexType(t))
        | _ -> None

    let rec public ToDotNetType t =
        match t with
        | Null -> null
        | Object _ -> typedefof<obj>
        | Bool -> typedefof<bool>
        | Numeric res -> res
        | String -> typedefof<string>
        | StructType t -> t
        | ClassType t -> t
        | ArrayType(t, 0) -> typedefof<System.Array>
        | ArrayType(t, rank) -> (ToDotNetType t).MakeArrayType(rank)
        | PointerType -> __notImplemented__()
        | SubType(t, _) ->  t
        | _ -> typedefof<obj>

    let rec FromCommonDotNetType dotNetType k =
        match dotNetType with
        | null -> Null
        | b when b.Equals(typedefof<bool>) -> Bool
        | n when numericTypes.Contains(n) -> Numeric n
        | s when s.Equals(typedefof<string>) -> String
        | e when e.IsEnum -> Numeric e
        | a when a.IsArray -> ArrayType(FromCommonDotNetType(a.GetElementType()) k, a.GetArrayRank())
        | s when s.IsValueType -> StructType s
        | f when f.IsSubclassOf(typedefof<System.Delegate>) ->
             let methodInfo = f.GetMethod("Invoke") in
             let returnType = methodInfo.ReturnType |> (fun t -> FromCommonDotNetType t k) in
             let parameters = methodInfo.GetParameters() |> Array.map (fun (p : System.Reflection.ParameterInfo) -> FromCommonDotNetType p.ParameterType k) in
             Func(List.ofArray parameters, returnType)
        | _ -> k dotNetType

    let rec public FromDotNetType dotNetType =
        FromCommonDotNetType dotNetType (fun concrete ->
        match concrete with
            // Actually interface is not nessesary a reference type, but if the implementation is unknown we consider it to be class (to check non-null).
            | c when c.IsClass || c.IsInterface -> ClassType c
            | _ -> __notImplemented__())

    let public IsPrimitive t =
        let dotNetType = ToDotNetType t in
        primitiveTypes.Contains dotNetType || dotNetType.IsEnum

    let public IsInteger = ToDotNetType >> integerTypes.Contains

    let public IsReal = ToDotNetType >> realTypes.Contains

    let public FromQualifiedTypeName = System.Type.GetType >> FromDotNetType

    let rec public FromConcreteMetadataType (t : IMetadataType) =
        match t with
        | null -> Object "unknown"
        | _ when t.AssemblyQualifiedName = "__Null" -> Null
        | _ when t.FullName = "System.Object" -> ClassType typedefof<obj>
        | _ when t.FullName = "System.Void" -> Void
        | :? IMetadataGenericArgumentReferenceType as g ->
            let constraints = g.Argument.TypeConstraints in
            if not(Array.isEmpty constraints) then
                __notImplemented__()
            ClassType typedefof<obj>
        | :? IMetadataArrayType as a ->
            let elementType = FromConcreteMetadataType a.ElementType in
            ArrayType(elementType, int(a.Rank))
        | :? IMetadataClassType as c ->
            Type.GetType(c.Type.AssemblyQualifiedName, true) |> FromDotNetType
        | _ -> Type.GetType(t.AssemblyQualifiedName, true) |> FromDotNetType

    let rec public FromSymbolicMetadataType (t : IMetadataType) (isUnique : bool) =
        match t with
        | null -> Object "unknown"
        | _ when t.AssemblyQualifiedName = "__Null" -> Null
        | _ when t.FullName = "System.Object" -> if isUnique then Object (typedefof<obj>.FullName) else Object (IdGenerator.startingWith typedefof<obj>.FullName)
        | _ when t.FullName = "System.Void" -> Void
        | :? IMetadataGenericArgumentReferenceType as g ->
            let constraints = g.Argument.TypeConstraints in
            if not(Array.isEmpty constraints) then
                __notImplemented__()
            Object "generic"
        | :? IMetadataArrayType as a ->
            let elementType = FromSymbolicMetadataType a.ElementType false in
            ArrayType(elementType, int(a.Rank))
        | :? IMetadataClassType as ct ->
                let metadataType =  Type.GetType(ct.Type.AssemblyQualifiedName, true) in
                FromCommonDotNetType metadataType (fun symbolic ->
                match symbolic with
                | a when a.FullName = "System.Array" -> ArrayType(Object "Array", int(0))
                | c when (c.IsClass && c.IsSealed) -> ClassType c
                | c when (c.IsClass || c.IsInterface) && not(c.IsSubclassOf(typedefof<System.Delegate>)) ->
                    if isUnique then SubType(c, c.FullName) else SubType(c, IdGenerator.startingWith c.FullName)
                | _ -> __notImplemented__())
        | _ -> Type.GetType(t.AssemblyQualifiedName, true) |> FromDotNetType

    let public MetadataToDotNetType (t : IMetadataType) = t |> FromConcreteMetadataType |> ToDotNetType

    let public FromDecompiledSignature (signature : JetBrains.Decompiler.Ast.IFunctionSignature) (returnMetadataType : IMetadataType) =
        let returnType = FromSymbolicMetadataType returnMetadataType true in
        let paramToType (param : JetBrains.Decompiler.Ast.IMethodParameter) =
            param.Type |> (fun t -> FromSymbolicMetadataType t false) in
        let args = Seq.map paramToType signature.Parameters |> List.ofSeq in
        Func(args, returnType)

    let public FromMetadataMethodSignature (m : IMetadataMethod) =
        let returnType = FromSymbolicMetadataType m.ReturnValue.Type true in
        let paramToType (param : IMetadataParameter) =
            param.Type |> (fun t -> FromSymbolicMetadataType t false)
        let args = Seq.map paramToType m.Parameters |> List.ofSeq in
        Func(args, returnType)

    let public GetMetadataTypeOfNode (node : JetBrains.Decompiler.Ast.INode) =
        DecompilerServices.getTypeOfNode node

    let public GetSystemTypeOfNode (node : JetBrains.Decompiler.Ast.INode) =
        let mt = GetMetadataTypeOfNode node in
        if mt = null then typedefof<obj>
        else ToDotNetType (FromConcreteMetadataType mt)

    let public GetFieldsOf (t : System.Type) isStatic =
        let staticFlag = if isStatic then BindingFlags.Static else BindingFlags.Instance in
        let flags = BindingFlags.Instance ||| BindingFlags.Public ||| BindingFlags.NonPublic ||| staticFlag in
        let fields = t.GetFields(flags) in
        let extractFieldInfo (field : FieldInfo) =
            let fieldName = sprintf "%s.%s" (DecompilerServices.removeGenericParameters (field.DeclaringType.FullName)) field.Name in
            (fieldName, FromDotNetType field.FieldType)
        fields |> Array.map extractFieldInfo |> Map.ofArray
