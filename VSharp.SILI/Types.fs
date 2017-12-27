namespace VSharp

open global.System
open System.Collections.Generic
open System.Reflection
open JetBrains.Metadata.Reader.API
open Hierarchy

type public Variance =
    | Contravariant
    | Covariant
    | Invarinat

[<StructuralEquality;NoComparison>]
type public ArrayDimensionType =
    | Vector
    | ConcreteDimension of int
    | SymbolicDimension of string

[<StructuralEquality;NoComparison>]
type public TermType =
    | Void
    | Bottom
    | Null
    | Bool
    | Numeric of System.Type
    | String
    | StructType of Hierarchy * TermTypeRef list * TermType list // some value type with generic argument and interfaces
    | ClassType of Hierarchy * TermTypeRef list * TermType list // some reference type with generic argument and interfaces
    | InterfaceType of Hierarchy * TermTypeRef list * TermType list // some interface type with generic argument and interfaces
    | GeneralType of TypeId
    | ArrayType of TermType * ArrayDimensionType
    | Func of TermType list * TermType
    | Reference of TermType
    | Pointer of TermType // int* and other C style pointers

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
        | InterfaceType(t, _, _) -> toString t
        | GeneralType(GenericParameter t) -> t.ToString()
        | GeneralType(GeneralName (name, _)) -> name
        | ArrayType(t, Vector) -> t.ToString() + "[]"
        | ArrayType(t, ConcreteDimension 1) -> t.ToString() + "[*]"
        | ArrayType(t, ConcreteDimension rank) -> t.ToString() + "[" + new string(',', rank - 1) + "]"
        | ArrayType(t, SymbolicDimension name) -> name
        | Reference t -> sprintf "<Reference to %O>" t
        | Pointer t -> sprintf "<Pointer to %O>" t

and [<CustomEquality;NoComparison>]
    TermTypeRef =
        | TermTypeRef of TermType ref
        override x.GetHashCode() =
            Microsoft.FSharp.Core.LanguagePrimitives.PhysicalHash(x)
        override x.Equals(o : obj) =
            match o with
            | :? TermTypeRef as other -> x.GetHashCode() = other.GetHashCode()
            | _ -> false

and [<StructuralEquality;NoComparison>]
    TypeId =
        | GenericParameter of Hierarchy
        | GeneralName of string * TermType

module public Types =
    let (|StructType|_|) = function
        | StructType(t, g ,i) -> Some(StructType(t.Inheritor, g, i))
        | _ -> None

    let (|ClassType|_|) = function
        | ClassType(t, g ,i) -> Some(ClassType(t.Inheritor, g, i))
        | _ -> None

    let (|InterfaceType|_|) = function
        | InterfaceType(t, g ,i) -> Some(InterfaceType(t.Inheritor, g, i))
        | _ -> None

    let internal pointerType = Numeric typedefof<int>

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
        | ClassType(t, _, _) when t = typedefof<obj> -> true
        | _ -> false

    let public IsVoid = function
        | Void -> true
        | _ -> false

    let public IsBottom = function
        | Bottom -> true
        | _ -> false

    let public IsNull = function
        | Null -> true
        | _ -> false

    let public IsReference = function
        | Reference _ -> true
        | _ -> false

    let public IsPointer = function
        | Pointer _ -> true
        | _ -> false

    let public DomainOf = function
        | Func(domain, _) -> domain
        | _ -> []

    let public RangeOf = function
        | Func(_, range) -> range
        | t -> t

    let public elementType = function
        | ArrayType(t, _) -> t
        | t -> internalfailf "expected array type, but got %O" t

    let rec public IsReferenceType = function
        | String
        | ClassType _
        | InterfaceType _
        | ArrayType _
        | Func _ -> true
        | GeneralType(GenericParameter t) when not t.Inheritor.IsValueType-> true
        | GeneralType(GeneralName(_, t)) -> IsReferenceType t
        | _ -> false

    let public IsValueType = not << IsReferenceType

    let internal WrapReferenceType = function
        | t when IsReferenceType t -> Reference t
        | t -> t

    let rec public ToDotNetType t =
        match t with
        | Null -> null
        | Bool -> typedefof<bool>
        | String -> typedefof<string>
        | Numeric t
        | StructType(t, _, _)
        | InterfaceType(t, _, _)
        | ClassType(t, _, _) -> t
        | GeneralType(GenericParameter t) -> t.Inheritor
        | ArrayType(t, SymbolicDimension _) -> typedefof<System.Array>
        | ArrayType(t, Vector) -> (ToDotNetType t).MakeArrayType()
        | ArrayType(t, ConcreteDimension rank) -> (ToDotNetType t).MakeArrayType(rank)
        | Reference t -> ToDotNetType t
        | Pointer t -> (ToDotNetType t).MakePointerType()
        | _ -> typedefof<obj>

    let internal SizeOf typ = // Reflection hacks, don't touch! Marshal.SizeOf lies!
        let internalSizeOf (typ: Type) : uint32 =
            let method = new Reflection.Emit.DynamicMethod("GetManagedSizeImpl", typeof<uint32>, null);

            let gen = method.GetILGenerator()
            gen.Emit(Reflection.Emit.OpCodes.Sizeof, typ)
            gen.Emit(Reflection.Emit.OpCodes.Ret)

            method.CreateDelegate(typeof<Func<uint32>>).DynamicInvoke()
            |> unbox
        typ |> ToDotNetType |> internalSizeOf |> int


    let internal BitSizeOf a typeOfA (t : System.Type) = System.Convert.ChangeType(SizeOf(typeOfA) * 8, t)


    module public Constructor =
        let private genericParameterFromMetadata (arg : IMetadataGenericArgument) =
            match arg with
            | _ when arg.TypeOwner <> null -> Type.GetType(arg.TypeOwner.AssemblyQualifiedName, true).GetGenericArguments().[int arg.Index]
            | _ when arg.MethodOwner <> null ->
                let metadataMethod = arg.MethodOwner
                let lengthGenericArguments = metadataMethod.GenericArguments.Length
                let parameters = metadataMethod.Parameters
                let declaringType = metadataMethod.DeclaringType
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
                assert(method.Length = 1)
                method.[0].GetGenericArguments().[int arg.Index]
            | _ -> __notImplemented__()

        let rec public MetadataToDotNetType (arg : IMetadataType) =
            match arg with
            | null -> null
            | _ when arg.AssemblyQualifiedName = "__Null" -> null
            | _ when arg.FullName = "System.Void" -> typedefof<System.Void>
            | :? IMetadataGenericArgumentReferenceType as g -> genericParameterFromMetadata g.Argument
            | :? IMetadataArrayType as a ->
                if a.IsVector
                    then (a.ElementType |> MetadataToDotNetType).MakeArrayType()
                    else (a.ElementType |> MetadataToDotNetType).MakeArrayType(int(a.Rank))
            | :? IMetadataClassType as c ->
                let originType = Type.GetType(c.Type.AssemblyQualifiedName, true)
                if not originType.IsGenericType || Array.isEmpty c.Arguments then originType
                else originType.MakeGenericType(c.Arguments |> Array.map MetadataToDotNetType)
            | _ -> Type.GetType(arg.AssemblyQualifiedName, true)

        let private StructType (t : Type) g i = StructType(Hierarchy t, g, i)

        let private ClassType (t : Type) g i = ClassType(Hierarchy t, g, i)

        let private InterfaceType (t : Type) g i = InterfaceType(Hierarchy t, g, i)

        let private GenericParameter (t : Type) = GenericParameter(Hierarchy t)

        module private TypesCache =

            let private types = new Dictionary<System.Type, TermType ref>()

            let public Contains t = types.ContainsKey t

            let public Prepare t = types.Add (t, ref Null)

            let public Find t = types.[t]

            let public Embody t value =
                types.[t] := value
                types.[t]

        let public GetVariance (genericParameterAttributes : GenericParameterAttributes) =
            let (==>) (left : GenericParameterAttributes) (right : GenericParameterAttributes) =
                left &&& right = right
            let variance = genericParameterAttributes &&& GenericParameterAttributes.VarianceMask
            match variance with
            | _ when variance ==> GenericParameterAttributes.Contravariant -> Contravariant
            | _ when variance ==> GenericParameterAttributes.Covariant -> Covariant
            | _ -> Invarinat

        let rec private getGenericArguments (dotNetType : Type) =
            if dotNetType.IsGenericType then
                Seq.map (fun t -> TermTypeRef (fromDotNetTypeRef t)) (dotNetType.GetGenericArguments()) |>
                List.ofSeq
            else []

        and private getConstraintFromDotNetInterface (interfaceType : Type) =
            let genericArguments = getGenericArguments interfaceType
            InterfaceType interfaceType genericArguments []

        and private getInterfaces (dotNetType : Type) = dotNetType.GetInterfaces() |> Seq.map getConstraintFromDotNetInterface |> List.ofSeq

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
                    fromCommonDotNetType (a.GetElementType()) |> WrapReferenceType,
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
            // Actually interface is not nessesary a reference type, but if the implementation is unknown we consider it to be class (to check non-null).
            | c when c.IsClass -> ClassType c (getGenericArguments c) (getInterfaces c)
            | i when i.IsInterface -> getConstraintFromDotNetInterface i
            | _ -> __notImplemented__()

        and private fromDotNetGenericParameterConstraint (dotNetType : Type) =
            match dotNetType with
            | g when g.IsGenericParameter ->
                fromDotNetGenericParameter g :: fromDotNetGenericParameterConstraints (g.GetGenericParameterConstraints())
            | i when i.IsInterface -> getConstraintFromDotNetInterface i |> List.singleton
            | _ -> List.Empty

        and private fromDotNetGenericParameterConstraints (constraints : Type[]) =
            constraints |> Seq.collect fromDotNetGenericParameterConstraint |> Seq.distinct |> List.ofSeq

        and private fromDotNetGenericParameter (genericParameter : Type) : TermType =
            GeneralType(GenericParameter genericParameter)

        and private fromDotNetType dotNetType = fromCommonDotNetType dotNetType

        and private fromDotNetTypeRef dotNetType =
            let key = dotNetType
            let res =
                if TypesCache.Contains key then TypesCache.Find key
                else
                    TypesCache.Prepare key
                    let termType = fromDotNetType dotNetType
                    TypesCache.Embody key termType
            res

        let public FromDotNetType (dotNetType : System.Type) =  if dotNetType = null then Null else !(fromDotNetTypeRef dotNetType)

        let rec public FromMetadataType (t : IMetadataType) =
            match t with
            | null -> ClassType typedefof<obj> [] []
            | _ when t.AssemblyQualifiedName = "__Null" -> Null
            | _ when t.FullName = "System.Void" -> Void
            | :? IMetadataGenericArgumentReferenceType as g ->
                let arg = MetadataToDotNetType g
                FromDotNetType arg
            | :? IMetadataArrayType as a ->
                let elementType = FromMetadataType a.ElementType |> WrapReferenceType
                ArrayType(elementType, if a.IsVector then Vector else a.Rank |> int |> ConcreteDimension)
            | :? IMetadataClassType as ct ->
                let dotnetType = MetadataToDotNetType ct
                FromDotNetType dotnetType
            | :? IMetadataPointerType as pt ->
                let dotnetType = MetadataToDotNetType pt
                FromDotNetType dotnetType
            | _ -> Type.GetType(t.AssemblyQualifiedName, true) |> FromDotNetType

        let private generalTypeName = "GeneralType" 

        let public CreateGeneralType termType () = GeneralType(GeneralName(IdGenerator.startingWith generalTypeName, termType))

        let (|StructureType|_|) = function
            | TermType.StructType(t, genArg, interfaces) -> Some(StructureType(t, genArg, interfaces))
            | Numeric t -> Some(StructureType(Hierarchy t, [], getInterfaces t))
            | Bool -> Some(StructureType(Hierarchy typedefof<bool>, [], getInterfaces typedefof<bool>))
            | GeneralType(GenericParameter t) when t.Inheritor.IsValueType -> Some(StructureType(t, [], getInterfaces t.Inheritor))
            | _ -> None

        let (|ReferenceType|_|) = function
            | String -> Some(ReferenceType(Hierarchy typedefof<string>, [], getInterfaces typedefof<string>))
            | TermType.ClassType(t, genArg, interfaces) -> Some(ReferenceType(t, genArg, interfaces))
            | TermType.InterfaceType(t, genArg, interfaces) -> Some(ReferenceType(t, genArg, interfaces))
            | TermType.ArrayType _ as arr ->
                let t = ToDotNetType arr
                Some(ReferenceType(Hierarchy t, [], getInterfaces t))
            | _ -> None

        let (|ComplexType|_|) = function
            | StructureType(t, genArg, interfaces)
            | ReferenceType(t, genArg, interfaces) -> Some(ComplexType(t, genArg, interfaces))
            | GeneralType(GenericParameter t)-> Some(ComplexType(t, [], getInterfaces t.Inheritor))
            | _ -> None

        let (|ConcreteType|_|) = function
            | ComplexType(t, _, _) -> Some(ConcreteType t)
            | _ -> None

    open Constructor

    let public IsPrimitive t =
        let dotNetType = ToDotNetType t
        primitiveTypes.Contains dotNetType || dotNetType.IsEnum

    let public IsInteger = ToDotNetType >> integerTypes.Contains

    let public IsReal = ToDotNetType >> realTypes.Contains

    let public IsUnsigned = unsignedTypes.Contains

    let public SystemGenericTypeDefinition (t : System.Type) =
        if t.IsGenericType && not <| t.IsGenericTypeDefinition then t.GetGenericTypeDefinition() else t

    let rec public IsAssignableToGenericType (givenType : Type) (genericType : Type) =
        let areInterfacesFound =
            givenType.GetInterfaces() |>
            Seq.exists (fun it -> it.IsGenericType && it.GetGenericTypeDefinition() = genericType)
        areInterfacesFound ||
            let baseType = givenType.BaseType
            baseType <> null &&
            if baseType.IsGenericType then baseType.GetGenericTypeDefinition() = genericType
            else IsAssignableToGenericType baseType genericType

    let private updateConstraints constraints = function
        | TermType.ClassType(t, g, _) -> ClassType(t, g, constraints)
        | TermType.StructType(t, g, _) -> StructType(t, g, constraints)
        | _ -> __unreachable__()

    let internal GetMetadataTypeOfNode (node : JetBrains.Decompiler.Ast.INode) =
        DecompilerServices.getTypeOfNode node

    let internal GetSystemTypeOfNode (node : JetBrains.Decompiler.Ast.INode) =
        let mt = GetMetadataTypeOfNode node
        if mt = null then typedefof<obj>
        else MetadataToDotNetType mt

    let internal GetFieldsOf (t : System.Type) isStatic =
        let staticFlag = if isStatic then BindingFlags.Static else BindingFlags.Instance
        let flags = BindingFlags.Instance ||| BindingFlags.Public ||| BindingFlags.NonPublic ||| staticFlag
        let fields = t.GetFields(flags)
        let extractFieldInfo (field : FieldInfo) =
            let fieldName = sprintf "%s.%s" ((SystemGenericTypeDefinition field.DeclaringType).FullName) field.Name
            (fieldName, FromDotNetType field.FieldType)
        fields |> FSharp.Collections.Array.map extractFieldInfo |> Map.ofArray
