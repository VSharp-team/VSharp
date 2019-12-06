namespace VSharp.Core

open VSharp
open VSharp.CSharpUtils
open global.System

type ISymbolicTypeSource =
    abstract TypeEquals : ISymbolicTypeSource -> bool

[<StructuralEquality;NoComparison>]
type arrayDimensionType =
    | Vector
    | ConcreteDimension of int
    | SymbolicDimension

[<StructuralEquality;NoComparison>]
type termType =
    | Void
    | Bottom
    | Null // TODO: delete Null from termType
    | Bool
    | Numeric of typeId
    | StructType of typeId * termType list        // Value type with generic argument
    | ClassType of typeId * termType list         // Reference type with generic argument
    | InterfaceType of typeId * termType list     // Interface type with generic argument
    | TypeVariable of typeId
    | ArrayType of termType * arrayDimensionType
    | Pointer of termType // C-style pointers like int*

    override x.ToString() =
        match x with
        | Void -> "System.Void"
        | Bottom -> "exception"
        | Null -> "<nullType>"
        | Bool -> typedefof<bool>.FullName
        | Numeric(Id t) -> t.FullName
        | StructType(Id t, g)
        | ClassType(Id t, g)
        | InterfaceType(Id t, g) ->
            if t.IsGenericType
                then
                    let args = String.Join(",", (Seq.map toString g))
                    sprintf "%s[%s]" (t.GetGenericTypeDefinition().FullName) args
                else toString t
        | TypeVariable(Id t) -> toString t
        | ArrayType(t, Vector) -> t.ToString() + "[]"
        | ArrayType(t, ConcreteDimension 1) -> t.ToString() + "[*]"
        | ArrayType(t, ConcreteDimension rank) -> t.ToString() + "[" + new string(',', rank - 1) + "]"
        | ArrayType(_, SymbolicDimension) -> "System.Array"
        | Pointer t -> sprintf "<Pointer to %O>" t

and [<CustomEquality;CustomComparison>]
    typeId =
        | Id of System.Type
        override x.GetHashCode() =
            match x with
            | Id h -> h.GetDeterministicHashCode()
        override x.Equals(o : obj) =
            match o with
            | :? typeId as other ->
                match x, other with
                | Id h1, Id h2 -> h1 = h2
            | _ -> false
        interface IComparable with
            override x.CompareTo(other) =
                match other with
                | :? typeId as other ->
                    match x, other with
                    | Id h1, Id h2 -> compare (hash h1) (hash h2) //TODO: change hash to MetadataToken when mono/mono#10127 is fixed.
                | _ -> -1

module internal Types =

    let Numeric t = Numeric (Id t)

    let (|Char|_|) = function
        | Numeric(Id t) when t = typeof<char> -> Some()
        | _ -> None

    let (|StructureType|_|) = function
        | StructType _
        | Numeric _
        | Bool -> Some(StructureType)
        | TypeVariable(Id t) when TypeUtils.isValueTypeParameter t -> Some(StructureType)
        | _ -> None

    let (|ReferenceType|_|) = function
        | ClassType _
        | InterfaceType _
        | ArrayType _ -> Some(ReferenceType)
        | TypeVariable(Id t) when TypeUtils.isReferenceTypeParameter t -> Some(ReferenceType)
        | _ -> None

    let (|ComplexType|_|) = function
        | StructureType
        | ReferenceType
        | TypeVariable _ -> Some(ComplexType)
        | _ -> None


    let StructType t g = StructType(t, g)
    let ClassType t g = ClassType(t, g)
    let InterfaceType t g = InterfaceType(t, g)

    let pointerType = Numeric typedefof<int>
    let indexType = Numeric typedefof<int>
    let lengthType = Numeric typedefof<int>

    let isNumeric = function
        | Numeric _ -> true
        | _ -> false

    let isBool = function
        | Bool -> true
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
        | ClassType(Id t, _) when t = typedefof<obj> -> true
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

    let concreteIsReferenceType = function
        | ReferenceType -> true
        | _ -> false

    let isPointer = function
        | Pointer _ -> true
        | _ -> false

    let elementType = function
        | ArrayType(t, _) -> t
        | t -> internalfailf "expected array type, but got %O" t

    let rec toDotNetType t =
        match t with
        | Void -> typedefof<Void>
        | Bool -> typedefof<bool>
        | Numeric(Id t) -> t
        | StructType(Id t, args)
        | InterfaceType(Id t, args)
        | ClassType(Id t, args) ->
            if t.IsGenericType
                then t.GetGenericTypeDefinition().MakeGenericType(Seq.map toDotNetType args |> Seq.toArray)
                else t
        | TypeVariable(Id t) -> t
        | ArrayType(_, SymbolicDimension) -> typedefof<System.Array>
        | ArrayType(t, Vector) -> (toDotNetType t).MakeArrayType()
        | ArrayType(t, ConcreteDimension rank) -> (toDotNetType t).MakeArrayType(rank)
        | Pointer t -> (toDotNetType t).MakePointerType()
        | Null -> __unreachable__()
        | _ -> __notImplemented__()

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
        let private StructType (t : Type) g = StructType (Id t) g
        let private ClassType (t : Type) g = ClassType (Id t) g
        let private InterfaceType (t : Type) g = InterfaceType (Id t) g

        let rec private getGenericArguments (dotNetType : Type) =
            if dotNetType.IsGenericType then
                Seq.map fromDotNetType (dotNetType.GetGenericArguments()) |> List.ofSeq
            else []

        and private makeInterfaceType (interfaceType : Type) =
            let genericArguments = getGenericArguments interfaceType
            InterfaceType interfaceType genericArguments

        and private getInterfaces (dotNetType : Type) = dotNetType.GetInterfaces() |> Seq.map makeInterfaceType |> List.ofSeq

        and fromDotNetType (dotNetType : Type) =
            match dotNetType with
            | null -> Null
            | p when p.IsPointer -> p.GetElementType() |> fromDotNetType |> Pointer
            | v when v.FullName = "System.Void" -> Void
            | a when a.FullName = "System.Array" -> ArrayType(fromDotNetType typedefof<obj>, SymbolicDimension)
            | b when b.Equals(typedefof<bool>) -> Bool
            | n when TypeUtils.isNumeric n -> Numeric n
            | e when e.IsEnum -> Numeric e
            | a when a.IsArray ->
                ArrayType(
                    fromDotNetType (a.GetElementType()),
                    if a = a.GetElementType().MakeArrayType() then Vector else ConcreteDimension <| a.GetArrayRank())
            | s when s.IsValueType && not s.IsGenericParameter -> StructType s (getGenericArguments s)
            | p when p.IsGenericParameter -> TypeVariable(Id p)
            | c when c.IsClass -> ClassType c (getGenericArguments c)
            | i when i.IsInterface -> makeInterfaceType i
            | _ -> __notImplemented__()

    open Constructor

    let public Char = Numeric typedefof<char>

    let public String = fromDotNetType typedefof<string>

    let (|StringType|_|) = function
        | typ when typ = String -> Some()
        | _ -> None

    let isString = (=) String

    let isInteger = toDotNetType >> TypeUtils.isIntegral

    let isReal = toDotNetType >> TypeUtils.isReal
