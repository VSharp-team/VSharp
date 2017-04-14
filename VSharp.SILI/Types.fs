namespace VSharp

open System
open System.Collections.Generic
open System.Reflection

[<StructuralEquality;NoComparison>]
type public TermType =
    | Void
    | Bottom
    | Object
    | Bool
    | Numeric of System.Type
    | String
    | StructType of System.Type
    | ClassType of System.Type
    | Func of TermType list * TermType

    override this.ToString() =
        match this with
        | Void -> "void"
        | Bottom -> "exception"
        | Object -> "object"
        | Bool -> "bool"
        | Numeric t -> t.Name.ToLower()
        | String -> "string"
        | Func(domain, range) -> String.Join(" -> ", List.append domain [range])
        | StructType t -> t.ToString()
        | ClassType t -> t.ToString()

module public Types =
    let private integerTypes =
        new HashSet<System.Type>(
                          [typedefof<byte>; typedefof<sbyte>;
                           typedefof<int16>; typedefof<uint16>;
                           typedefof<int32>; typedefof<uint32>;
                           typedefof<int64>; typedefof<uint64>])

    let private realTypes =
        new HashSet<System.Type>([typedefof<single>; typedefof<double>; typedefof<decimal>])

    let private numericTypes = new HashSet<System.Type>(Seq.append integerTypes realTypes)

    let private primitiveTypes = new HashSet<Type>(Seq.append numericTypes [typedefof<bool>; typedefof<string>])

    let public ToDotNetType t =
        match t with
        | Object -> typedefof<obj>
        | Bool -> typedefof<bool>
        | Numeric res -> res
        | String -> typedefof<string>
        | StructType t -> t
        | ClassType t -> t
        | _ -> typedefof<obj>

    let rec public FromDotNetType = function
        | b when b.Equals(typedefof<bool>) -> Bool
        | n when numericTypes.Contains(n) -> Numeric n
        | s when s.Equals(typedefof<string>) -> String
        | f when f.IsSubclassOf(typedefof<System.Delegate>) ->
            let methodInfo = f.GetMethod("Invoke") in
            let returnType = methodInfo.ReturnType |> FromDotNetType in
            let parameters = methodInfo.GetParameters() |> Array.map (fun (p : System.Reflection.ParameterInfo) -> FromDotNetType p.ParameterType) in
            Func(List.ofArray parameters, returnType)
        | s when s.IsValueType -> StructType s
        // Actually interface is not nessesary reference type, but if the implementation is unknown we consider it to be class (to check non-null).
        | c when c.IsClass || c.IsInterface -> ClassType c
        | _ -> __notImplemented__()

    let public FromQualifiedTypeName = System.Type.GetType >> FromDotNetType

    let private QualifiedNameOfMetadataType (t : JetBrains.Metadata.Reader.API.IMetadataType) =
        match t with
        | :? JetBrains.Metadata.Reader.API.IMetadataClassType as c -> c.Type.AssemblyQualifiedName
        | t -> t.AssemblyQualifiedName

    let public FromMetadataType (t : JetBrains.Metadata.Reader.API.IMetadataType) =
        if t = null then Object
        else
            match QualifiedNameOfMetadataType t with
            | "__Null" -> Object
            | _ as qtn ->
                let dotNetType = Type.GetType(qtn, true) in
                if dotNetType = null then __notImplemented__()
                else FromDotNetType dotNetType

    let public MetadataToDotNetType (t : JetBrains.Metadata.Reader.API.IMetadataType) = t |> FromMetadataType |> ToDotNetType

    let public FromDecompiledSignature (signature : JetBrains.Decompiler.Ast.IFunctionSignature) (returnMetadataType : JetBrains.Metadata.Reader.API.IMetadataType) =
        let returnType = FromMetadataType returnMetadataType in
        let paramToType (param : JetBrains.Decompiler.Ast.IMethodParameter) =
            param.Type |> FromMetadataType
        let args = Seq.map paramToType signature.Parameters |> List.ofSeq in
        Func(args, returnType)

    let public FromMetadataMethodSignature (m : JetBrains.Metadata.Reader.API.IMetadataMethod) =
        let returnType = FromMetadataType m.ReturnValue.Type in
        let paramToType (param : JetBrains.Metadata.Reader.API.IMetadataParameter) =
            param.Type |> FromMetadataType
        let args = Seq.map paramToType m.Parameters |> List.ofSeq in
        Func(args, returnType)

    let public IsInteger = ToDotNetType >> integerTypes.Contains

    let public IsReal = ToDotNetType >> realTypes.Contains

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

    let public IsObject = function
        | Object _ -> true
        | _ -> false

    let public IsVoid = function
        | Void -> true
        | _ -> false

    let public IsBottom = function
        | Bottom -> true
        | _ -> false

    let public IsReference t = IsClass t || IsObject t || IsFunction t

    let public IsPrimitive = ToDotNetType >> primitiveTypes.Contains

    let public DomainOf = function
        | Func(domain, _) -> domain
        | _ -> []

    let public RangeOf = function
        | Func(_, range) -> range
        | t -> t

    let public IsRelation = RangeOf >> IsBool

    let public GetMetadataTypeOfNode (node : JetBrains.Decompiler.Ast.INode) =
        DecompilerServices.getTypeOfNode node

    let public GetSystemTypeOfNode (node : JetBrains.Decompiler.Ast.INode) =
        let mt = GetMetadataTypeOfNode node in
        if mt = null then typedefof<obj>
        else ToDotNetType (FromMetadataType mt)

    let public GetFieldsOf (t : System.Type) isStatic =
        let staticFlag = if isStatic then BindingFlags.Static else BindingFlags.Instance in
        let flags = BindingFlags.Instance ||| BindingFlags.Public ||| BindingFlags.NonPublic ||| staticFlag in
        let fields = t.GetFields(flags) in
        let extractFieldInfo (field : FieldInfo) =
            (sprintf "%s.%s" field.DeclaringType.FullName field.Name, FromDotNetType field.FieldType)
        fields |> Array.map extractFieldInfo |> Map.ofArray
