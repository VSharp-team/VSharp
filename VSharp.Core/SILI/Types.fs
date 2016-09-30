namespace VSharp.Core.Symbolic

open System
open System.Collections.Generic
open VSharp.Core.Utils

type public TermType =
    | Void
    | Bool
    | Numeric of System.Type
    | String
    | Product of TermType list
    | Func of TermType * TermType

    override this.ToString() =
        match this with
        | Void -> "void"
        | Bool -> "bool"
        | Numeric t -> t.Name.ToLower()
        | String -> "string"
        | Product ts -> ts |> List.map Wrappers.toString |> Wrappers.join ", " |> box |> Wrappers.format1 "({0})"
        | Func(domain, range) -> String.Format("{0} -> {1}", domain, range)

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
        | Bool -> typedefof<bool>
        | Numeric res -> res
        | String -> typedefof<string>
        | _ -> typedefof<obj>

    let public FromPrimitiveDotNetType t =
        match t with
        | b when b.Equals(typedefof<bool>) -> Bool
        | n when numericTypes.Contains(n) -> Numeric n
        | b when b.Equals(typedefof<string>) -> String
        | _ -> Void

    let public IsBool (t : TermType) =
        match t with
        | Bool -> true
        | _ -> false

    let public IsInteger = ToDotNetType >> integerTypes.Contains

    let public IsReal = ToDotNetType >> realTypes.Contains

    let public IsNumeric t =
        match t with
        | Numeric _ -> true
        | _ -> false

    let public IsString t =
        match t with
        | String -> true
        | _ -> false

    let public IsPrimitive = ToDotNetType >> primitiveTypes.Contains
    let public IsPrimitiveSolvable = ToDotNetType >> primitiveSolvableTypes.Contains

    let rec public IsSolvable t =
        match t with
        | Product ts -> ts |> Seq.forall IsSolvable
        | Func(domain, range) -> IsSolvable domain && IsSolvable range
        | _ -> IsPrimitiveSolvable t

    let public (-->) domain range = Func(domain, range)

    let public DomainOf t =
        match t with
        | Func(domain, _) -> domain
        | _ -> Void

    let public RangeOf t =
        match t with
        | Func(_, range) -> range
        | _ -> t

    let public IsRelation = RangeOf >> IsBool

    let public GetTypeOfNode (node : JetBrains.Decompiler.Ast.INode) =
        // node.Data.TryGetValue is poorly implemented (it checks reference equality of keys), so searching manually...
        let typeKey = "Type"
        let typeOption = node.Data |> Seq.tryPick (fun keyValue -> if (keyValue.Key.ToString() = typeKey) then Some(keyValue.Value) else None)

        match typeOption with
        | Some t -> Type.GetType((t :?> JetBrains.Metadata.Reader.API.IMetadataType).AssemblyQualifiedName)
        | None -> typedefof<obj>
