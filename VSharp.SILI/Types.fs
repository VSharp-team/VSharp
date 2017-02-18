namespace VSharp

open System
open System.Collections.Generic

[<StructuralEquality;NoComparison>]
type public TermType =
    | Void
    | Object
    | Bool
    | Numeric of System.Type
    | String
    | Product of TermType list
    | Resolved of System.Type
    | Unresolved of JetBrains.Metadata.Reader.API.IMetadataType
    | Func of TermType list * TermType
    | Unknown

    override this.ToString() =
        match this with
        | Void -> "void"
        | Object -> "object"
        | Bool -> "bool"
        | Numeric t -> t.Name.ToLower()
        | String -> "string"
        | Product ts -> ts |> List.map toString |> join ", " |> box |> format1 "({0})"
        | Func(domain, range) -> String.Join(" -> ", List.append domain [range])
        | Resolved t -> t.ToString()
        | Unresolved t -> t.AssemblyQualifiedName
        | Unknown -> "<unknown or dynamic type>"

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

    let private primitiveSolvableTypes = new HashSet<Type>(Seq.append numericTypes [typedefof<bool>])

    let private primitiveTypes = new HashSet<Type>(Seq.append primitiveSolvableTypes [typedefof<string>])

    let public ToDotNetType t =
        match t with
        | Object -> typedefof<obj>
        | Bool -> typedefof<bool>
        | Numeric res -> res
        | String -> typedefof<string>
        | Resolved t -> t
        | _ -> typedefof<obj>

    let rec public FromDotNetType t =
        match t with
        | o when o.Equals(typedefof<obj>) -> Object
        | b when b.Equals(typedefof<bool>) -> Bool
        | n when numericTypes.Contains(n) -> Numeric n
        | s when s.Equals(typedefof<string>) -> String
        | f when f.IsSubclassOf(typedefof<System.Delegate>) ->
            let methodInfo = f.GetMethod("Invoke") in
            let returnType = methodInfo.ReturnType |> FromDotNetType in
            let parameters = methodInfo.GetParameters() |> Array.map (fun (p : System.Reflection.ParameterInfo) -> FromDotNetType p.ParameterType) in
            Func(List.ofArray parameters, returnType)
        | _ -> Resolved t

    let public FromMetadataType (t : JetBrains.Metadata.Reader.API.IMetadataType) =
        if t = null then Unknown
        else
            match t.AssemblyQualifiedName with
            | "__Null" -> Object
            | _ as qtn ->
                let dotNetType = Type.GetType(qtn) in
                if dotNetType = null then Unresolved t
                else FromDotNetType dotNetType

    let public MetadataToDotNetType (t : JetBrains.Metadata.Reader.API.IMetadataType) = t |> FromMetadataType |> ToDotNetType

    let public FromFunctionSignature (signature : JetBrains.Decompiler.Ast.IFunctionSignature) (returnMetadataType : JetBrains.Metadata.Reader.API.IMetadataType) =
        let returnType = FromMetadataType returnMetadataType in
        let paramToType (param : JetBrains.Decompiler.Ast.IMethodParameter) =
            param.Type |> FromMetadataType
        let args = Seq.map paramToType signature.Parameters |> List.ofSeq in
        Func(args, returnType)

    let public IsBool (t : TermType) =
        match t with
        | Bool -> true
        | _ -> false

    let public IsInteger = ToDotNetType >> integerTypes.Contains

    let public IsReal = ToDotNetType >> realTypes.Contains

    let public IsNumeric = function
        | Numeric _ -> true
        | _ -> false

    let public IsString = function
        | String -> true
        | _ -> false

    let public IsFunction = function
        | Func _ -> true
        | _ -> false

    let public IsObject = function
        | Object _ -> true
        | _ -> false

    let public IsPrimitive = ToDotNetType >> primitiveTypes.Contains
    let public IsPrimitiveSolvable = ToDotNetType >> primitiveSolvableTypes.Contains

    let rec public IsSolvable = function
        | Product ts -> ts |> Seq.forall IsSolvable
        | Func(domain, range) -> List.forall IsSolvable domain && IsSolvable range
        | t -> IsPrimitiveSolvable t

    let public DomainOf = function
        | Func(domain, _) -> domain
        | _ -> []

    let public RangeOf = function
        | Func(_, range) -> range
        | t -> t

    let public IsRelation = RangeOf >> IsBool

    let public GetMetadataTypeOfNode (node : JetBrains.Decompiler.Ast.INode) =
        DecompilerServices.getPropertyOfNode node "Type" null :?> JetBrains.Metadata.Reader.API.IMetadataType

    let public GetSystemTypeOfNode (node : JetBrains.Decompiler.Ast.INode) =
        let mt = GetMetadataTypeOfNode node in
        if mt = null then typedefof<obj>
        else ToDotNetType (FromMetadataType mt)
