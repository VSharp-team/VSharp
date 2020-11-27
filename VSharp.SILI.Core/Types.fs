namespace VSharp.Core

#nowarn "69"

open VSharp
open VSharp.CSharpUtils
open global.System

[<StructuralEquality;StructuralComparison>]
type arrayDimensionType =
    | Vector
    | ConcreteDimension of int
    | SymbolicDimension

[<StructuralEquality;StructuralComparison>]
type symbolicType =
    | Void   // TODO: delete Void from termType
    | Null   // TODO: delete Null from termType
    | Bool
    | Numeric of typeId
    | AddressType
    | StructType of typeId * symbolicType list        // Value type with generic argument
    | ClassType of typeId * symbolicType list         // Reference type with generic argument
    | InterfaceType of typeId * symbolicType list     // Interface type with generic argument
    | TypeVariable of typeId
    | ArrayType of symbolicType * arrayDimensionType
    | Pointer of symbolicType // C-style pointers like int*

    override x.ToString() =
        match x with
        | Void -> "System.Void"
        | Null -> "<nullType>"
        | Bool -> typedefof<bool>.FullName
        | Numeric(Id t) -> t.FullName
        | AddressType -> "<heapAddressType>"
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
    interface IAtomicRegion<symbolicType>

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

    type AddressTypeAgent = class end

    let Numeric t =
        match t with
        | _ when t = typeof<AddressTypeAgent> -> AddressType
        | _ -> Numeric (Id t)

    let (|Char|_|) = function
        | Numeric(Id t) when t = typeof<char> -> Some()
        | _ -> None

    let (|ValueType|_|) = function
        | StructType _
        | Numeric _
        | Bool -> Some(ValueType)
        | TypeVariable(Id t) when TypeUtils.isValueTypeParameter t -> Some(ValueType)
        | _ -> None

    let (|ReferenceType|_|) = function
        | ClassType _
        | InterfaceType _
        | ArrayType _ -> Some(ReferenceType)
        | TypeVariable(Id t) when TypeUtils.isReferenceTypeParameter t -> Some(ReferenceType)
        | _ -> None

    let (|ComplexType|_|) = function
        | ValueType
        | ReferenceType
        | TypeVariable _ -> Some(ComplexType)
        | _ -> None


    let StructType t g = StructType(t, g)
    let ClassType t g = ClassType(t, g)
    let InterfaceType t g = InterfaceType(t, g)

    let indexType = Numeric typeof<int>
    let lengthType = Numeric typeof<int>
    let objectType = ClassType (Id typeof<obj>) []

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

    let rec isOpenType = function
        | TypeVariable _ -> true
        | StructType(_, parameters) when List.exists isOpenType parameters -> true
        | ClassType(_, parameters) when List.exists isOpenType parameters -> true
        | InterfaceType(_, parameters) when List.exists isOpenType parameters -> true
        | ArrayType(t, _) when isOpenType t -> true
        | _ -> false

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
        | AddressType -> typeof<AddressTypeAgent>
        | Null -> __unreachable__()

    let sizeOf typ = // Reflection hacks, don't touch! Marshal.SizeOf lies!
        let internalSizeOf (typ: Type) : uint32 =
            let meth = Reflection.Emit.DynamicMethod("GetManagedSizeImpl", typeof<uint32>, null);

            let gen = meth.GetILGenerator()
            gen.Emit(Reflection.Emit.OpCodes.Sizeof, typ)
            gen.Emit(Reflection.Emit.OpCodes.Ret)

            meth.CreateDelegate(typeof<Func<uint32>>).DynamicInvoke()
            |> unbox
        typ |> toDotNetType |> internalSizeOf |> int


    let bitSizeOfType t (resultingType : System.Type) = System.Convert.ChangeType(sizeOf(t) * 8, resultingType)

    let rankOf = function
        | ArrayType(_, dim) ->
            match dim with
            | Vector -> 1
            | ConcreteDimension d -> d
            | SymbolicDimension -> __insufficientInformation__ "Can't get precise array rank of System.Array object!"
        | t -> internalfailf "Getting rank of an array: expected array type, but got %O" t

    // Performs syntactical unification of types, returns the infimum of x and y (where x < y iff x is more generic).
    let rec structuralInfimum (x : System.Type) (y : System.Type) =
        if x = y then Some x
        else
            if x.IsGenericParameter then Some y
            elif y.IsGenericParameter then Some x
            elif not x.IsGenericType || not y.IsGenericType then None
            elif x.GetGenericTypeDefinition() <> y.GetGenericTypeDefinition() then None
            else
                let xargs = x.GetGenericArguments()
                let yargs = y.GetGenericArguments()
                assert(xargs.Length = yargs.Length)
                let infs = Array.map2 structuralInfimum xargs yargs
                if Array.forall Option.isSome infs then Some (x.GetGenericTypeDefinition().MakeGenericType(Array.map Option.get infs))
                else None

    module internal Constructor =
        let private StructType (t : Type) g = StructType (Id t) g
        let private ClassType (t : Type) g = ClassType (Id t) g
        let private InterfaceType (t : Type) g = InterfaceType (Id t) g

        let private getGenericDefinition (dotNetType : Type) =
            if dotNetType.IsGenericType then
                dotNetType.GetGenericTypeDefinition()
            else dotNetType

        let rec private getGenericArguments (dotNetType : Type) =
            if dotNetType.IsGenericType then
                Seq.map fromDotNetType (dotNetType.GetGenericArguments()) |> List.ofSeq
            else []

        and fromDotNetType (dotNetType : Type) =
            match dotNetType with
            | null -> Null
            | t when t.IsByRef -> internalfail "byref type is not implemented!" // TODO: care about byref type
            | p when p.IsPointer -> p.GetElementType() |> fromDotNetType |> Pointer
            | v when v.FullName = "System.Void" -> Void
            | a when a.FullName = "System.Array" -> ArrayType(fromDotNetType typedefof<obj>, SymbolicDimension)
            | b when b.Equals(typedefof<bool>) -> Bool
            | a when a.Equals(typedefof<AddressTypeAgent>) -> AddressType
            | n when TypeUtils.isNumeric n -> Numeric n
            | e when e.IsEnum -> Numeric e
            | a when a.IsArray ->
                ArrayType(
                    fromDotNetType (a.GetElementType()),
                    if a = a.GetElementType().MakeArrayType() then Vector else ConcreteDimension <| a.GetArrayRank())
            | s when s.IsValueType && not s.IsGenericParameter -> StructType (getGenericDefinition s) (getGenericArguments s)
            | p when p.IsGenericParameter -> TypeVariable(Id p)
            | c when c.IsClass -> ClassType (getGenericDefinition c) (getGenericArguments c)
            | i when i.IsInterface -> InterfaceType (getGenericDefinition i) (getGenericArguments i)
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

    let isValueType = function
        | TypeVariable(Id t) when TypeUtils.isValueTypeParameter t -> true
        | TypeVariable(Id t) when TypeUtils.isReferenceTypeParameter t -> false
        | TypeVariable _ as t -> __insufficientInformation__ "Can't determine if %O is a value type or not!" t
        | Null -> __unreachable__()
        | t -> (toDotNetType t).IsValueType

    let isConcreteSubtype t1 t2 =
        (toDotNetType t2).IsAssignableFrom (toDotNetType t1)

type symbolicType with
    interface IAtomicRegion<symbolicType> with
        override x.Intersect y =
            let x = Types.toDotNetType x
            let y = Types.toDotNetType y
            Types.structuralInfimum x y |> Option.map Types.Constructor.fromDotNetType
