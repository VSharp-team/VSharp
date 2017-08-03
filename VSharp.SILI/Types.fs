namespace VSharp

open global.System
open System.Collections.Generic
open System.Reflection
open JetBrains.Metadata.Reader.API

[<StructuralEquality;NoComparison>]
type public Constraint<'T> =
    | DefaultConctructor
    | ReferenceType
    | ValueType
    | Interface of 'T

    override this.ToString() =
        match this with
        | DefaultConctructor -> "has default constructor"
        | ReferenceType -> "has reference type"
        | Interface t -> t.ToString()

type public TermType =
    | Void
    | Bottom
    | Null
    | Object of string * Constraint<TermType> list // symbolic object with constraint
    | Bool
    | Numeric of System.Type
    | String
    | StructType of System.Type * TermType list // some value type with generic argument
    | ClassType of System.Type * TermType list // some reference type with generic argument
    | SubType of System.Type * TermType list * string * Constraint<TermType> list //some symbolic type with generic argument and constraint
    | ArrayType of TermType * int
    | Func of TermType list * TermType
    | PointerType of TermType

    override this.ToString() =
        match this with
        | Void -> "void"
        | Bottom -> "exception"
        | Null -> "<nullType>"
        | Object(name, []) -> sprintf "<%s object>" name
        | Object(name, typeConstraint) -> 
            sprintf "<%s object with constraint %s>" name (String.Join(", ", typeConstraint))
        | Bool -> "bool"
        | Numeric t -> t.Name.ToLower()
        | String -> "string"
        | Func(domain, range) -> String.Join(" -> ", List.append domain [range])
        | StructType(t, [])
        | ClassType (t, []) -> t.ToString()
        | StructType(t, genericArgument)
        | ClassType(t, genericArgument) -> sprintf "<%s%s>" (t.ToString()) (sprintf "<%s>" <| String.Join(", ", genericArgument)) 
        | SubType(t, [], name, []) -> sprintf "<Subtype of %s>" (toString t)
        | SubType(t, genericArgument, name, []) ->
            sprintf "<Subtype of %s%s>" (toString t) (sprintf "<%s>" <| String.Join(", ", genericArgument))
        | SubType(t, [], name, typeConstraint) ->
            sprintf "<Subtype of %s with constraint %s>" (toString t) (String.Join(", ", typeConstraint))
        | ArrayType(t, rank) -> t.ToString() + "[" + new string(',', rank) + "]"
        | PointerType t -> sprintf "<Reference to %s>" (toString t)

type public Constraint = Constraint<TermType>

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
    
    let public genericParameters = new Dictionary<System.Type, TermType>() 
    
    let rec public IsAssignableToGenericType (givenType : Type) (genericType : Type) =
        let isFindInterfaces = givenType.GetInterfaces() |> 
            Array.filter (fun it -> it.IsGenericType) |> 
            Array.map (fun it -> it.GetGenericTypeDefinition()) |>
            Array.exists (fun it -> it = genericType)
        if isFindInterfaces then true
        else
            let baseType = givenType.BaseType
            if baseType = null then false
            else if baseType.IsGenericType then baseType.GetGenericTypeDefinition() = genericType
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
        | StructType(t, genArg) -> Some(StructureType(t, genArg))
        | Numeric t -> Some(StructureType(t, []))
        | Bool -> Some(StructureType(typedefof<bool>, []))
        | _ -> None

    let (|ReferenceType|_|) = function
        | String -> Some(ReferenceType(typedefof<string>, []))
        | ClassType(t, genArg) -> Some(ReferenceType(t, genArg))
        | _ -> None

    let (|ComplexType|_|) = function
        | StructureType(t, genArg)
        | ReferenceType(t, genArg) -> Some(ComplexType(t, genArg, []))
        | SubType(t, genArg, _, typeConstraint) -> Some(ComplexType(t, genArg, typeConstraint))
        | Object(name, typeConstraint) -> Some(ComplexType(typedefof<obj>, [], typeConstraint))
        | _ -> None

    let public pointerFromReferenceType = function
        | t when IsReferenceType t -> (PointerType t)
        | t -> t

    let rec public ToDotNetType t =
        match t with
        | Null -> null
        | Object _ -> typedefof<obj>
        | Bool -> typedefof<bool>
        | String -> typedefof<string>
        | Numeric t
        | StructType(t, [])
        | ClassType(t, [])
        | SubType(t, [], _, _) ->  t
        | StructType(t, genArg)
        | ClassType(t, genArg)
        | SubType(t, genArg, _, _) -> t.MakeGenericType(genArg |> (List.map ToDotNetType) |> List.toArray)
        | ArrayType(t, 0) -> typedefof<System.Array>
        | ArrayType(t, rank) -> (ToDotNetType t).MakeArrayType(rank)
        | PointerType t -> ToDotNetType t
        | _ -> typedefof<obj>

    let rec FromCommonDotNetType (dotNetType : Type) k =
        match dotNetType with
        | null -> Null
        | v when v.FullName = "System.Void" -> Void
        | b when b.Equals(typedefof<bool>) -> Bool
        | n when numericTypes.Contains(n) -> Numeric n
        | s when s.Equals(typedefof<string>) -> String
        | e when e.IsEnum -> Numeric e
        | a when a.IsArray -> ArrayType(FromCommonDotNetType (a.GetElementType()) k |> pointerFromReferenceType, a.GetArrayRank())
        | s when s.IsValueType -> 
            if not s.IsGenericType then StructType(s, [])
            else StructType(s, s.GetGenericArguments() |> Array.map (fun t -> FromCommonDotNetType t k) |> Array.toList)
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
            | c when c.IsClass || c.IsInterface -> 
                if not c.IsGenericType then ClassType(c, [])
                else ClassType(c, c.GetGenericArguments() |> Array.map FromDotNetType |> Array.toList)
            | _ -> __notImplemented__())

    let public IsPrimitive t =
        let dotNetType = ToDotNetType t in
        primitiveTypes.Contains dotNetType || dotNetType.IsEnum

    let public IsInteger = ToDotNetType >> integerTypes.Contains

    let public IsReal = ToDotNetType >> realTypes.Contains

    let public FromQualifiedTypeName = System.Type.GetType >> FromDotNetType
    
    let public GenericParameterFromMetadata (g : IMetadataGenericArgumentReferenceType) =
        let arg = g.Argument
        match arg with
        | _ when arg.TypeOwner <> null -> Type.GetType(arg.TypeOwner.AssemblyQualifiedName, true).GetGenericArguments().[(int)arg.Index]
        | _ when arg.MethodOwner <> null ->
            let metadataMethod = arg.MethodOwner
            let lengthGenericArguments = metadataMethod.GenericArguments.Length
            let parameters = metadataMethod.Parameters
            let declaringType = metadataMethod.DeclaringType
            let method = Type.GetType(metadataMethod.DeclaringType.AssemblyQualifiedName, true).GetMethods() |>
                Array.filter (fun (m : MethodInfo) -> m.Name = metadataMethod.Name) |>
                Array.map (fun (m : MethodInfo) -> m, m.GetParameters(), m.GetGenericArguments().Length) |>
                Array.filter (fun (m, (p : ParameterInfo[]), gLen) -> (gLen = lengthGenericArguments) && (p.Length = parameters.Length)) |>
                Array.map (fun (m, p, _) -> (m, p)) |>
                Array.filter (fun (m, (p : ParameterInfo[])) -> 
                    Array.forall2 (fun (l : ParameterInfo) (r : IMetadataParameter) -> 
                        (if (l.ParameterType.FullName <> null) then l.ParameterType.FullName else l.ParameterType.Name) = r.Type.FullName) p parameters) |>
                Array.map (fun (m, _) -> m)
            method.[0].GetGenericArguments().[(int)arg.Index]
            
    let public FromMetadataGenericArgument fromMetadataType (arg : IMetadataGenericArgument) =
        let (===) (a : GenericArgumentAttributes) (b : GenericArgumentAttributes) = (a = (a &&& b)) in 
        let constraints = arg.TypeConstraints in
        let fromAttributesToConstraint attr =
            match attr with
            | _ when attr === GenericArgumentAttributes.DefaultConstructorConstraint -> [DefaultConctructor]
            | _ when attr === GenericArgumentAttributes.ReferenceTypeConstraint -> [ReferenceType]
            | _ when attr === GenericArgumentAttributes.ValueTypeConstraint -> [ValueType]
            | _ when attr === (GenericArgumentAttributes.DefaultConstructorConstraint &&&
                GenericArgumentAttributes.ReferenceTypeConstraint) -> [ReferenceType ; DefaultConctructor]
            | _ when attr === (GenericArgumentAttributes.DefaultConstructorConstraint &&&
                GenericArgumentAttributes.ValueTypeConstraint) -> [ValueType ; DefaultConctructor]
            | _  -> []
        in
        if not(Array.isEmpty constraints) then
            let listInterfacesConstraint, listInheritanceConstraint =
                constraints |> Array.toList |>
                List.map fromMetadataType |>              
                List.mappedPartition (fun termType ->
                    match termType with
                    | ComplexType(t, gp, c) when t.IsInterface -> Some(Interface termType)
                    | _ -> None)
            let (listSpecialConcstraint : Constraint list) = fromAttributesToConstraint arg.Attributes
            match listSpecialConcstraint.Head with
            | ValueType -> SubType(typedefof<ValueType>, [], arg.Name, listInterfacesConstraint @ listSpecialConcstraint.Tail)
            | Constraint.ReferenceType -> Object(arg.Name, listInterfacesConstraint @ listSpecialConcstraint)
            | _ -> if listInheritanceConstraint.IsEmpty then Object(arg.Name, listInterfacesConstraint @ listSpecialConcstraint)
                   else 
            __notImplemented__() //TODO
        else Object(arg.Name, [])
    
    let rec public FromConcreteMetadataType (t : IMetadataType) =
        match t with
        | null -> Object("unknown", [])
        | _ when t.AssemblyQualifiedName = "__Null" -> Null
        | _ when t.FullName = "System.Object" -> ClassType(typedefof<obj>, [])
        | _ when t.FullName = "System.Void" -> Void
        | :? IMetadataGenericArgumentReferenceType as g ->
//            if not <| genericParameters.ContainsKey(arg) then genericParameters.Add(arg, FromMetadataGenericArgument FromConcreteMetadataType arg)
            __notImplemented__()
        | :? IMetadataArrayType as a ->
            let elementType = FromConcreteMetadataType a.ElementType |> pointerFromReferenceType in
            ArrayType(elementType, int(a.Rank))
        | :? IMetadataClassType as ct ->
            let metadataType = Type.GetType(ct.Type.AssemblyQualifiedName, true) in
            FromCommonDotNetType metadataType (fun concrete ->
            match concrete with
            // Actually interface is not nessesary a reference type, but if the implementation is unknown we consider it to be class (to check non-null).
            | c when ((c.IsClass || c.IsInterface) && not(c.IsSubclassOf(typedefof<System.Delegate>))) -> 
                if not c.IsGenericType then ClassType(c, [])
                else
                   let gArg = ct.Arguments |> Array.toList |> List.map FromConcreteMetadataType
                   ClassType(c, gArg)
            | _ -> __notImplemented__())
        | _ -> Type.GetType(t.AssemblyQualifiedName, true) |> FromDotNetType

    let rec public FromSymbolicMetadataType (t : IMetadataType) (isUnique : bool) =
        match t with
        | null -> Object("unknown", [])
        | _ when t.AssemblyQualifiedName = "__Null" -> Null
        | _ when t.FullName = "System.Object" ->
            if isUnique then Object(typedefof<obj>.FullName, []) 
            else Object(IdGenerator.startingWith typedefof<obj>.FullName, [])
        | _ when t.FullName = "System.Void" -> Void
        | :? IMetadataGenericArgumentReferenceType as g ->
            let constraints = g.Argument.TypeConstraints in
            if not(Array.isEmpty constraints) then
                let listConstraint = constraints |> Array.map (fun t -> FromSymbolicMetadataType t isUnique) |> Array.toList
                __notImplemented__() //TODO
            else Object(g.FullName, [])
        | :? IMetadataArrayType as a ->
            let elementType = FromSymbolicMetadataType a.ElementType false |> pointerFromReferenceType in
            ArrayType(elementType, int(a.Rank))
        | :? IMetadataClassType as ct -> //TODO: make with generic
                let metadataType = Type.GetType(ct.Type.AssemblyQualifiedName, true) in
                FromCommonDotNetType metadataType (fun symbolic ->
                match symbolic with
                | a when a.FullName = "System.Array" -> ArrayType(Object("Array", []), int(0))
                | c when (c.IsClass || c.IsInterface) ->
                    let makeType agrs = 
                        match c with
                        | _ when c.IsSealed -> ClassType(c, agrs)
                        | _ when not(c.IsSubclassOf(typedefof<System.Delegate>)) -> 
                             if isUnique then SubType(c, agrs, c.FullName, []) else SubType(c, agrs, IdGenerator.startingWith c.FullName, [])
                    in
                    if not c.IsGenericType then makeType []
                    else
                        let gArgs = ct.Arguments |> Array.toList |> List.map (fun t -> FromSymbolicMetadataType t isUnique)
                        makeType gArgs
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
