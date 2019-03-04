namespace VSharp.Core

open VSharp
open global.System
open System.Collections.Generic
open System.Reflection

type ISymbolicTypeSource =
    abstract TypeEquals : ISymbolicTypeSource -> bool

type variance =
    | Contravariant
    | Covariant
    | Invarinat

[<StructuralEquality;NoComparison>]
type arrayDimensionType =
    | Vector
    | ConcreteDimension of int
    | SymbolicDimension of string transparent

[<StructuralEquality;NoComparison>]
type termType =
    | Void
    | Bottom
    | Null
    | Bool
    | Numeric of System.Type
    | StructType of hierarchy * termType list        // Value type with generic argument
    | ClassType of hierarchy * termType list         // Reference type with generic argument
    | InterfaceType of hierarchy * termType list     // Interface type with generic argument
    | TypeVariable of typeId
    | ArrayType of termType * arrayDimensionType
    | Func of termType list * termType
    | Reference of termType
    | Pointer of termType // C-style pointers like int*

    override x.ToString() =
        match x with
        | Void -> "System.Void"
        | Bottom -> "exception"
        | Null -> "<nullType>"
        | Bool -> typedefof<bool>.FullName
        | Numeric t -> t.FullName
        | Func(domain, range) -> String.Join(" -> ", List.append domain [range])
        | StructType(t, g)
        | ClassType(t, g)
        | InterfaceType(t, g) ->
            if t.Inheritor.IsGenericType
                then
                    let args = String.Join(",", (Seq.map toString g))
                    sprintf "%s[%s]" (t.Inheritor.GetGenericTypeDefinition().FullName) args
                else toString t
        | TypeVariable(Explicit t) -> toString t
        | TypeVariable(Implicit (name, _, t)) -> sprintf "%s{%O}" name.v t
        | ArrayType(t, Vector) -> t.ToString() + "[]"
        | ArrayType(t, ConcreteDimension 1) -> t.ToString() + "[*]"
        | ArrayType(t, ConcreteDimension rank) -> t.ToString() + "[" + new string(',', rank - 1) + "]"
        | ArrayType(_, SymbolicDimension name) -> name.v
        | Reference t -> sprintf "<Reference to %O>" t
        | Pointer t -> sprintf "<Pointer to %O>" t

and [<CustomEquality;CustomComparison>]
    typeId =
        | Explicit of hierarchy
        | Implicit of (string transparent) * ISymbolicTypeSource * termType
        override x.GetHashCode() =
            match x with
            | Explicit h -> hash h
            | Implicit(_, ts, t) -> (hash t) * 3571 ^^^ (hash ts)
        override x.Equals(o : obj) =
            match o with
            | :? typeId as other ->
                match x, other with
                | Implicit(_, s1, t1), Implicit(_, s2, t2) -> s1.TypeEquals s2 && t1 = t2
                | Explicit h1, Explicit h2 -> h1 = h2
                | _ -> false
            | _ -> false
        interface IComparable with
            override x.CompareTo(other) =
                match other with
                | :? typeId as other ->
                    match x, other with
                    | Explicit h1, Explicit h2 -> compare (hash h1.Inheritor) (hash h2.Inheritor) //TODO: change hash to MetadataToken when mono/mono#10127 is fixed.
                    | Explicit _, Implicit _ -> -1
                    | Implicit _, Explicit _ -> 1
                    | Implicit(_, c1, _), Implicit(_, c2, _) ->
                        compare (c1.GetHashCode()) (c2.GetHashCode())
                | _ -> -1

module internal Types =
    let (|StructType|_|) = function
        | StructType(t, g) -> Some(StructType(t.Inheritor, g))
        | _ -> None

    let (|ClassType|_|) = function
        | ClassType(t, g) -> Some(ClassType(t.Inheritor, g))
        | _ -> None

    let (|InterfaceType|_|) = function
        | InterfaceType(t, g) -> Some(InterfaceType(t.Inheritor, g))
        | _ -> None

    let (|Char|_|) = function
        | Numeric t when t = typeof<char> -> Some()
        | _ -> None

    let StructType t g = StructType(t, g)
    let ClassType t g = ClassType(t, g)
    let InterfaceType t g = InterfaceType(t, g)

    let pointerType = Numeric typedefof<int>
    let indexType = Numeric typedefof<int>

    let isNumeric = function
        | Numeric _ -> true
        | _ -> false

    let isBool = function
        | Bool -> true
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
        | ClassType(t, _) when t = typedefof<obj> -> true
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
        | ClassType _
        | InterfaceType _
        | ArrayType _
        | Func _ -> true
        | TypeVariable(Explicit t) when not t.Inheritor.IsValueType-> true
        | TypeVariable(Implicit(_, _, t)) -> isReferenceType t
        | _ -> false

    let isValueType = not << isReferenceType

    let wrapReferenceType = function
        | t when isReferenceType t -> Reference t
        | t -> t

    let rec toDotNetType t =
        match t with
        | Null -> null
        | Bool -> typedefof<bool>
        | Numeric t -> t
        | StructType(t, args)
        | InterfaceType(t, args)
        | ClassType(t, args) ->
            if t.IsGenericType
                then t.GetGenericTypeDefinition().MakeGenericType(Seq.map toDotNetType args |> Seq.toArray)
                else t
        | TypeVariable(Explicit t) -> t.Inheritor
        | TypeVariable(Implicit(_, _, t)) -> toDotNetType t
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
        let private StructType (t : Type) g = StructType (hierarchy t) g
        let private ClassType (t : Type) g = ClassType (hierarchy t) g
        let private InterfaceType (t : Type) g = InterfaceType (hierarchy t) g
        let private Explicit (t : Type) = Explicit(hierarchy t)

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
                Seq.map fromDotNetType (dotNetType.GetGenericArguments()) |> List.ofSeq
            else []

        and private makeInterfaceType (interfaceType : Type) =
            let genericArguments = getGenericArguments interfaceType
            InterfaceType interfaceType genericArguments

        and private getInterfaces (dotNetType : Type) = dotNetType.GetInterfaces() |> Seq.map makeInterfaceType |> List.ofSeq

        and private fromCommonDotNetType (dotNetType : Type) =
            match dotNetType with
            | null -> Null
            | p when p.IsPointer -> p.GetElementType() |> fromCommonDotNetType |> Pointer
            | v when v.FullName = "System.Void" -> Void
            | a when a.FullName = "System.Array" -> ArrayType(fromCommonDotNetType typedefof<obj> |> Reference, SymbolicDimension {v="System.Array"})
            | b when b.Equals(typedefof<bool>) -> Bool
            | n when TypeUtils.isNumeric n -> Numeric n
            | e when e.IsEnum -> Numeric e
            | a when a.IsArray ->
                ArrayType(
                    fromCommonDotNetType (a.GetElementType()) |> wrapReferenceType,
                    if a = a.GetElementType().MakeArrayType() then Vector else ConcreteDimension <| a.GetArrayRank())
            | s when s.IsValueType && not s.IsGenericParameter-> StructType s (getGenericArguments s)
            | f when f.IsSubclassOf(typedefof<System.Delegate>) ->
                let methodInfo = f.GetMethod("Invoke")
                let returnType = methodInfo.ReturnType |> fromCommonDotNetType
                let parameters = methodInfo.GetParameters() |>
                                    Seq.map (fun (p : System.Reflection.ParameterInfo) ->
                                    fromCommonDotNetType p.ParameterType)
                Func(List.ofSeq parameters, returnType)
            | p when p.IsGenericParameter -> fromDotNetGenericParameter p
            | c when c.IsClass -> ClassType c (getGenericArguments c)
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
            TypeVariable(Explicit genericParameter)

        and fromDotNetType (dotNetType : System.Type) =  if dotNetType = null then Null else fromCommonDotNetType dotNetType

        let (|StructureType|_|) = function
            | termType.StructType(t, genArg) -> Some(StructureType(t, genArg))
            | Numeric t -> Some(StructureType(hierarchy t, []))
            | Bool -> Some(StructureType(hierarchy typedefof<bool>, []))
            | TypeVariable(Explicit t) when t.Inheritor.IsValueType -> Some(StructureType(t, []))
            | _ -> None

        let (|ReferenceType|_|) = function
            | termType.ClassType(t, genArg) -> Some(ReferenceType(t, genArg))
            | termType.InterfaceType(t, genArg) -> Some(ReferenceType(t, genArg))
            | termType.ArrayType _ as arr ->
                let t = toDotNetType arr
                Some(ReferenceType(hierarchy t, []))
            | _ -> None

        let (|ComplexType|_|) = function
            | StructureType(t, genArg)
            | ReferenceType(t, genArg) -> Some(ComplexType(t, genArg))
            | TypeVariable(Explicit t)-> Some(ComplexType(t, []))
            | _ -> None

        let (|ConcreteType|_|) = function
            | ComplexType(t, _) -> Some(ConcreteType t)
            | _ -> None

    open Constructor

    module public Variable =
        let create name source termType = TypeVariable(Implicit({v=sprintf "TypeVariable{%s}" name}, source, termType))

        let fromTermType name source termType =
            let updateDimension = function
                | SymbolicDimension _ -> SymbolicDimension {v=sprintf "ArrayTypeVariable{%s}" name}
                | d -> d
            let rec getNewType = function
                | ArrayType(elemType, dim) ->
                    let newElemType = getNewType elemType
                    ArrayType(newElemType, updateDimension dim)
                | ConcreteType t as termType when t.Inheritor.IsSealed && not t.Inheritor.IsGenericParameter -> termType
                | Pointer termType -> Pointer (create name source termType)
                | termType -> create name source termType
            getNewType termType

        let fromDotNetType name source dotnetType =
            let termType = fromDotNetType dotnetType
            fromTermType name source termType

    let public Char = Numeric typedefof<char>

    let public String = fromDotNetType typedefof<string>

    let (|StringType|_|) = function
        | typ when typ = String -> Some()
        | _ -> None

    let isString = (=) String

    let isPrimitive = toDotNetType >> TypeUtils.isPrimitive

    let isInteger = toDotNetType >> TypeUtils.isIntegral

    let isReal = toDotNetType >> TypeUtils.isReal

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

    let rec specifyType = function
        | TypeVariable(Implicit(_, _, typ)) -> specifyType typ
        | typ -> typ

    let unwrapReferenceType = function
        | Reference t -> t
        | t -> t
