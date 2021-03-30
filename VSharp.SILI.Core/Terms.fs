namespace VSharp.Core

open VSharp
open VSharp.CSharpUtils
open System
open System.Collections.Generic
open Types.Constructor

[<CustomEquality;CustomComparison>]
type stackKey =
    | ThisKey of Reflection.MethodBase
    | ParameterKey of Reflection.ParameterInfo
    | LocalVariableKey of Reflection.LocalVariableInfo * Reflection.MethodBase
    | TemporaryLocalVariableKey of Type
    override x.ToString() =
        match x with
        | ThisKey _ -> "this"
        | ParameterKey pi -> pi.Name
        | LocalVariableKey (lvi,_) -> "__loc__" + lvi.LocalIndex.ToString()
        | TemporaryLocalVariableKey typ -> sprintf "__tmp__%s" (Reflection.getFullTypeName typ)
    override x.GetHashCode() =
        let fullname =
            match x with
            | ThisKey m -> sprintf "%s##this" (Reflection.getFullMethodName m)
            | ParameterKey pi -> sprintf "%O##%O" pi.Member pi
            | LocalVariableKey (lvi, m) -> sprintf "%O##%s" (Reflection.getFullMethodName m) (lvi.ToString())
            | TemporaryLocalVariableKey typ -> sprintf "temporary##%s" (Reflection.getFullTypeName typ)
        fullname.GetDeterministicHashCode()
    interface IComparable with
        override x.CompareTo(other: obj) =
            match other with
            | :? stackKey as other ->
                match x, other with
                | ThisKey _, ThisKey _
                | ParameterKey _, ParameterKey _
                | LocalVariableKey _, LocalVariableKey _
                | TemporaryLocalVariableKey _, TemporaryLocalVariableKey _ -> x.GetHashCode().CompareTo(other.GetHashCode())
                | ThisKey _, _ -> -1
                | _, ThisKey _ -> 1
                | LocalVariableKey _, _ -> -1
                | _, LocalVariableKey _ -> 1
                | TemporaryLocalVariableKey _, _ -> -1
                | _, TemporaryLocalVariableKey _ -> 1
            | _ -> -1
    override x.Equals(other) = (x :> IComparable).CompareTo(other) = 0
    member x.TypeOfLocation =
        match x with
        | ThisKey m -> m.DeclaringType
        | ParameterKey p -> p.ParameterType
        | LocalVariableKey(l, _) -> l.LocalType
        | TemporaryLocalVariableKey typ -> typ
        |> fromDotNetType
    member x.Map typeSubst =
        match x with
        | ThisKey m -> ThisKey (Reflection.concretizeMethodBase m typeSubst)
        | ParameterKey p -> ParameterKey (Reflection.concretizeParameter p typeSubst)
        | LocalVariableKey(l, m) -> LocalVariableKey (Reflection.concretizeLocalVariable l m typeSubst)
        | TemporaryLocalVariableKey typ -> TemporaryLocalVariableKey (Reflection.concretizeType typeSubst typ)

type concreteHeapAddress = vectorTime
type arrayType = symbolicType * int * bool // Element type * dimension * is vector

type IFunctionIdentifier =
    abstract ReturnType : Type
    abstract IsConstructor : bool
    abstract Method : System.Reflection.MethodBase

type EmptyIdentifier() =
    interface IFunctionIdentifier with
        override x.ReturnType = typeof<Void>
        override x.IsConstructor = false
        override x.Method = null

[<StructuralEquality;NoComparison>]
type operation =
    | Operator of OperationType
    | Application of StandardFunction
    | Cast of symbolicType * symbolicType
    member x.priority =
        match x with
        | Operator op -> Operations.operationPriority op
        | Application _ -> Operations.maxPriority
        | Cast _ -> Operations.maxPriority - 1

// TODO: symbolic type -> primitive type
// TODO: get rid of Nop!
[<StructuralEquality;NoComparison>]
type termNode =
    | Nop
    | Concrete of obj * symbolicType
    | Constant of string transparent * ISymbolicConstantSource * symbolicType
    | Expression of operation * term list * symbolicType
    | Struct of pdict<fieldId, term> * symbolicType
    | HeapRef of heapAddress * symbolicType
    | Ref of address
    | Ptr of address option * symbolicType * term option // base address (none = null) * type sight * offset
    | Union of (term * term) list

    override x.ToString() =
        let getTerm (term : term) = term.term

        let checkExpression priority parentPriority =
            if priority < parentPriority then sprintf "(%s)" else id

        let formatIfNotEmpty format value =
            match value with
            | _ when String.IsNullOrEmpty value -> value
            | _ -> format value

        let formatWithIndent indent = sprintf "\n%s%s" indent

        let extendIndent = (+) "\t"

        let rec toStr parentPriority indent term =
            match term with
            | Nop -> "<VOID>"
            | Constant(name, _, _) -> name.v
            | Concrete(_, (ClassType(Id t, _) as typ)) when t.IsSubclassOf(typedefof<System.Delegate>) ->
                sprintf "<Lambda Expression %O>" typ
            | Concrete(_, Null) -> "null"
            | Concrete(obj, AddressType) when (obj :?> uint32 list) = [0u] -> "null"
            | Concrete(c, Numeric (Id t)) when t = typedefof<char> && c :?> char = '\000' -> "'\\000'"
            | Concrete(c, Numeric (Id t)) when t = typedefof<char> -> sprintf "'%O'" c
            | Concrete(:? concreteHeapAddress as k, AddressType) -> VectorTime.print k
            | Concrete(value, _) -> value.ToString()
            | Expression(operation, operands, _) ->
                match operation with
                | Operator operator when Operations.operationArity operator = 1 ->
                    assert (List.length operands = 1)
                    let operand = List.head operands
                    let opStr = Operations.operationToString operator |> checkExpression operation.priority parentPriority
                    let printedOperand = toStr operation.priority indent operand.term
                    sprintf (Printf.StringFormat<string->string>(opStr)) printedOperand
                | Operator operator ->
                    assert (List.length operands >= 2)
                    let printedOperands = operands |> List.map (getTerm >> toStr operation.priority indent)
                    let sortedOperands = if Operations.isCommutative operator then List.sort printedOperands else printedOperands
                    sortedOperands
                        |> String.concat (Operations.operationToString operator)
                        |> checkExpression operation.priority parentPriority
                | Cast(_, dest) ->
                    assert (List.length operands = 1)
                    sprintf "(%O %s)" dest (toStr operation.priority indent (List.head operands).term) |>
                        checkExpression operation.priority parentPriority
                | Application f -> operands |> List.map (getTerm >> toStr -1 indent) |> join ", " |> sprintf "%O(%s)" f
            | Struct(fields, t) ->
                fieldsToString indent fields |> sprintf "%O STRUCT [%s]" t
            | Union(guardedTerms) ->
                let guardedToString (guard, term) =
                    let guardString = toStringWithParentIndent indent guard
                    let termString = toStringWithParentIndent indent term
                    sprintf "| %s ~> %s" guardString termString
                let printed = guardedTerms |> Seq.map guardedToString |> Seq.sort |> join ("\n" + indent)
                formatIfNotEmpty (formatWithIndent indent) printed |> sprintf "UNION[%s]"
            | HeapRef({term = Concrete(obj, AddressType)}, Null) when (obj :?> uint32 list) = [0u] -> "NullRef"
            | HeapRef(address, baseType) -> sprintf "(HeapRef %O to %O)" address baseType
            | Ref address -> sprintf "(%sRef %O)" (address.Zone()) address
            | Ptr(address, typ, shift) ->
                let offset =
                    match shift with
                    | None -> ""
                    | Some shift -> ", offset = " + shift.ToString()
                match address with
                | None -> sprintf "(nullptr as %O%s)" typ offset
                | Some address -> sprintf "(%sPtr %O as %O%s)" (address.Zone()) address typ offset

        and fieldsToString indent fields =
            let stringResult = PersistentDict.toString "| %O ~> %O" ("\n" + indent) toString toString toString fields
            formatIfNotEmpty (formatWithIndent indent) stringResult

        and toStringWithIndent indent term = toStr -1 indent term.term

        and toStringWithParentIndent parentIndent = toStringWithIndent <| extendIndent parentIndent

        toStr -1 "\t" x

and heapAddress = term // only Concrete(:? concreteHeapAddress) or Constant of type HeapAddressType!

and address =
    | PrimitiveStackLocation of stackKey
    | StructField of address * fieldId
    | StackBufferIndex of stackKey * term
    | BoxedLocation of concreteHeapAddress * symbolicType // TODO: delete type from boxed location?
    | ClassField of heapAddress * fieldId
    | ArrayIndex of heapAddress * term list * arrayType
    | ArrayLowerBound of heapAddress * term * arrayType
    | ArrayLength of heapAddress * term * arrayType
    | StaticField of symbolicType * fieldId
    override x.ToString() =
        match x with
        | PrimitiveStackLocation key -> toString key
        | ClassField(addr, field) -> sprintf "%O.%O" addr field
        | ArrayIndex(addr, idcs, _) -> sprintf "%O[%s]" addr (List.map toString idcs |> join ", ")
        | StaticField(typ, field) -> sprintf "%O.%O" typ field
        | StructField(addr, field) -> sprintf "%O.%O" addr field
        | ArrayLength(addr, dim, _) -> sprintf "Length(%O, %O)" addr dim
        | BoxedLocation(addr, typ) -> sprintf "%O^%s" typ (addr |> List.map toString |> join ".")
        | StackBufferIndex(key, idx) -> sprintf "%O[%O]" key idx
        | ArrayLowerBound(addr, dim, _) -> sprintf "LowerBound(%O, %O)" addr dim
    member x.Zone() =
        match x with
        | PrimitiveStackLocation _
        | StackBufferIndex _ -> "Stack"
        | ClassField _
        | ArrayIndex _
        | ArrayLength _
        | BoxedLocation _
        | ArrayLowerBound  _ -> "Heap"
        | StaticField _ -> "Statics"
        | StructField(addr, _) -> addr.Zone()

and
    [<CustomEquality;NoComparison>]
    term =
        {term : termNode; hc : int}
        override x.ToString() = x.term.ToString()
        override x.GetHashCode() = x.hc
        override x.Equals(o : obj) =
            match o with
            | :? term as other -> Microsoft.FSharp.Core.LanguagePrimitives.PhysicalEquality x.term other.term
            | _ -> false

and
    ISymbolicConstantSource =
        abstract SubTerms : term seq
        abstract Time : vectorTime

type INonComposableSymbolicConstantSource =
    inherit ISymbolicConstantSource

module HashMap =
    let hashMap = weakdict<termNode, term>()
    let addTerm node =
        let result = ref { term = node; hc = 0 }
        if hashMap.TryGetValue(node, result)
            then !result
            else
                let hc = hash node
                let term = { term = node; hc = hc }
                hashMap.Add(node, term)
                term

[<AutoOpen>]
module internal Terms =

    let term (term : term) = term.term

// --------------------------------------- Primitives ---------------------------------------

    let Nop = HashMap.addTerm Nop
    let Concrete obj typ = HashMap.addTerm (Concrete(obj, typ))
    let Constant name source typ = HashMap.addTerm (Constant({v=name}, source, typ))
    let Expression op args typ = HashMap.addTerm (Expression(op, args, typ))
    let Struct fields typ = HashMap.addTerm (Struct(fields, typ))
    let HeapRef address baseType =
        match address.term with
        | Constant(name, _, _) when name.v = "this.head" -> ()
        | _ -> ()
        HashMap.addTerm (HeapRef(address, baseType))
    let Ref address =
        match address with
        | ArrayIndex(_, indices, (_, dim, _)) -> assert(List.length indices = dim)
        | _ -> ()
        HashMap.addTerm (Ref address)
    let Ptr baseAddress typ offset = HashMap.addTerm (Ptr(baseAddress, typ, offset))
    let ConcreteHeapAddress addr = Concrete addr AddressType
    let Union gvs =
        if List.length gvs < 2 then internalfail "Empty and one-element unions are forbidden!"
        HashMap.addTerm (Union gvs)

    let castReferenceToPointer targetType = term >> function
        | Ref address -> Ptr (Some address) targetType None
        | Ptr(address, _, None) -> Ptr address targetType None
        | Ptr(address, _, (Some _ as indent)) -> Ptr address targetType indent
        | t -> internalfailf "Expected reference or pointer, got %O" t

    let isVoid = term >> function
        | Nop -> true
        | _ -> false

    let isConcrete = term >> function
        | Concrete _ -> true
        | _ -> false

    let isUnion = term >> function
        | Union _ -> true
        | _ -> false

    let isTrue = term >> function
        | Concrete(b, t) when Types.isBool t && (b :?> bool) -> true
        | _ -> false

    let isFalse = term >> function
        | Concrete(b, t) when Types.isBool t && not (b :?> bool) -> true
        | _ -> false

    let operationOf = term >> function
        | Expression(op, _, _) -> op
        | term -> internalfailf "expression expected, %O received" term

    let argumentsOf = term >> function
        | Expression(_, args, _) -> args
        | term -> internalfailf "expression expected, %O received" term

    let fieldsOf = term >> function
        | Struct(fields, _) -> fields
        | term -> internalfailf "struct or class expected, %O received" term

    let private typeOfUnion getType gvs =
        let chooseTypes (_, v) =
            let typ = getType v
            if Types.isVoid typ || Types.isNull typ then None else Some typ
        let nonEmptyTypes = List.choose chooseTypes gvs
        match nonEmptyTypes with
        | [] -> Null
        | t::ts ->
            let allSame =
                List.forall ((=) t) ts
            if allSame then t
            else internalfailf "evaluating type of unexpected union %O!" gvs

    let commonTypeOf getType term =
        match term.term with
        | Nop -> symbolicType.Void
        | Union gvs -> typeOfUnion getType gvs
        | _ -> getType term

    let typeOfAddress = function
        | ClassField(_, field)
        | StructField(_, field)
        | StaticField(_, field) -> field.typ |> fromDotNetType
        | ArrayIndex(_, _, (elementType, _, _)) -> elementType
        | BoxedLocation(_, typ) -> typ
        | ArrayLength _
        | ArrayLowerBound  _ -> Types.lengthType
        | StackBufferIndex _ -> Types.Numeric typeof<int8>
        | PrimitiveStackLocation loc -> loc.TypeOfLocation

    let typeOfRef =
        let getTypeOfRef = term >> function
            | Ref addr -> typeOfAddress addr
            | Ptr(addr, _, _) ->
                match addr with
                | Some addr -> typeOfAddress addr
                | None -> __unreachable__()
            | term -> internalfailf "expected reference, but got %O" term
        commonTypeOf getTypeOfRef

    let sightTypeOfPtr =
        let getTypeOfPtr = term >> function
            | Ptr(_, typ, _) -> typ
            | term -> internalfailf "expected pointer, but got %O" term
        commonTypeOf getTypeOfPtr

    let baseTypeOfPtr = typeOfRef

    let typeOf =
        let getType term =
            match term.term with
            | Concrete(_, t)
            | Constant(_, _, t)
            | Expression(_, _, t)
            | HeapRef(_, t)
            | Struct(_, t) -> t
            | Ref _ -> typeOfRef term
            | Ptr _ -> sightTypeOfPtr term |> Pointer
            | _ -> __unreachable__()
        commonTypeOf getType

    let symbolicTypeToArrayType = function
        | ArrayType(elementType, dim) ->
            match dim with
            | Vector -> (elementType, 1, true)
            | ConcreteDimension d -> (elementType, d, false)
            | SymbolicDimension -> __insufficientInformation__ "Cannot process array of unknown dimension!"
        | typ -> internalfailf "Expected array type, but got %O" typ

    let sizeOf = typeOf >> Types.sizeOf
    let bitSizeOf term resultingType = Types.bitSizeOfType (typeOf term) resultingType

    let isBool t =    typeOf t |> Types.isBool
    let isNumeric t = typeOf t |> Types.isNumeric

    let rec isStruct term =
        match term.term with
        | Struct _ -> true
        | Union gvs -> List.forall (snd >> isStruct) gvs
        | _ -> false

    let rec isReference term =
        match term.term with
        | HeapRef _ -> true
        | Ref _ -> true
        | Union gvs -> List.forall (snd >> isReference) gvs
        | _ -> false

    // Only for concretes: there will never be null type
    let canCastConcrete (concrete : obj) targetType =
        assert(not <| Types.isNull targetType)
        let targetType = Types.toDotNetType targetType
        let actualType = TypeUtils.getTypeOfConcrete concrete
        actualType = targetType || targetType.IsAssignableFrom(actualType)

    let castConcrete (concrete : obj) (t : Type) =
        let actualType = TypeUtils.getTypeOfConcrete concrete
        let functionIsCastedToMethodPointer () =
            typedefof<System.Reflection.MethodBase>.IsAssignableFrom(actualType) && typedefof<IntPtr>.IsAssignableFrom(t)
        if actualType = t then
            Concrete concrete (fromDotNetType t)
        elif t.IsEnum && t.GetEnumUnderlyingType().IsAssignableFrom(actualType) || actualType.IsEnum && actualType.GetEnumUnderlyingType().IsAssignableFrom(t) then
            Concrete concrete (fromDotNetType t)
        elif TypeUtils.canConvert actualType t then
            Concrete (TypeUtils.convert concrete t) (fromDotNetType t)
        elif t.IsAssignableFrom(actualType) then
            Concrete concrete (fromDotNetType t)
        elif functionIsCastedToMethodPointer() then
            Concrete concrete (fromDotNetType actualType)
        else raise(InvalidCastException(sprintf "Cannot cast %s to %s!" (Reflection.getFullTypeName actualType) (Reflection.getFullTypeName t)))

    let True =
        Concrete (box true) Bool

    let False =
        Concrete (box false) Bool

    let makeBool predicate =
        if predicate then True else False

    let makeIndex (i : int) =
        Concrete i Types.indexType

    let zeroAddress =
        Concrete VectorTime.zero AddressType

    let nullRef =
        HeapRef zeroAddress Null

    let makeNullPtr typ =
        Ptr None typ None

    let makeNumber n =
        Concrete n (Numeric(Id(n.GetType())))

    let makeBinary operation x y t =
        assert(Operations.isBinary operation)
        Expression (Operator operation) [x; y] t

    let makeNAry operation x t =
        match x with
        | [] -> raise(ArgumentException("List of args should be not empty"))
        | [x] -> x
        | _ -> Expression (Operator operation) x t

    let makeUnary operation x t =
        assert(Operations.isUnary operation)
        Expression (Operator operation) [x] t

    let (|CastExpr|_|) = term >> function
        | Expression(Cast(srcType, dstType), [x], t) ->
            assert(dstType = t)
            Some(CastExpr(x, srcType, dstType))
        | _ -> None

    let rec private makeCast term fromType toType =
        match term, toType with
        | _ when fromType = toType -> term
        | CastExpr(x, xType, Numeric(Id t)), Numeric(Id dstType) when not <| TypeUtils.isLessForNumericTypes t dstType ->
            makeCast x xType toType
        | CastExpr(x, (Numeric(Id srcType) as xType), Numeric(Id t)), Numeric(Id dstType)
            when not <| TypeUtils.isLessForNumericTypes t srcType && not <| TypeUtils.isLessForNumericTypes dstType t ->
            makeCast x xType toType
        | _ -> Expression (Cast(fromType, toType)) [term] toType

    let rec primitiveCast term targetType =
        match term.term, targetType with // TODO: make better #do
        | _ when typeOf term = targetType -> term
        | _, Pointer typ when typeOf term |> Types.isNumeric -> Ptr None typ (Some term)
        | Concrete(value, _), _ -> castConcrete value (Types.toDotNetType targetType)
        // TODO: make cast to Bool like function Transform2BooleanTerm
        | Constant(_, _, t), _
        | Expression(_, _, t), _ -> makeCast term t targetType
        | Ref _, ByRef _ -> term
        | Union gvs, _ -> gvs |> List.map (fun (g, v) -> (g, primitiveCast v targetType)) |> Union
        | _ -> __unreachable__()

    let negate term =
        assert(isBool term)
        makeUnary OperationType.LogicalNeg term Bool

    let (|True|_|) term = if isTrue term then Some True else None
    let (|False|_|) term = if isFalse term then Some False else None

    let (|ConcreteT|_|) = term >> function
        | Concrete(name, typ) -> Some(ConcreteT(name, typ))
        | _ -> None

    let (|UnionT|_|) = term >> function
        | Union gvs -> Some(UnionT gvs)
        | _ -> None

    let (|GuardedValues|_|) = function // TODO: this could be ineffective (because of unzip)
        | Union gvs -> Some(GuardedValues(List.unzip gvs))
        | _ -> None

    let (|UnaryMinus|_|) = function
        | Expression(Operator OperationType.UnaryMinus, [x], t) -> Some(UnaryMinus(x, t))
        | _ -> None

    let (|UnaryMinusT|_|) = term >> (|UnaryMinus|_|)

    let (|Add|_|) = term >> function
        | Expression(Operator OperationType.Add, [x;y], t) -> Some(Add(x, y, t))
        | _ -> None

    let (|Sub|_|) = term >> function
        | Expression(Operator OperationType.Subtract, [x;y], t) -> Some(Sub(x, y, t))
        | _ -> None

    let (|Mul|_|) = term >> function
        | Expression(Operator OperationType.Multiply, [x;y], t) -> Some(Mul(x, y, t))
        | _ -> None

    let (|Div|_|) = term >> function
        | Expression(Operator OperationType.Divide, [x;y], t) -> Some(Div(x, y, t))
        | _ -> None

    let (|Rem|_|) = term >> function
        | Expression(Operator OperationType.Remainder, [x;y], t) -> Some(Rem(x, y, t))
        | _ -> None

    let (|Negation|_|) = function
        | Expression(Operator OperationType.LogicalNeg, [x], _) -> Some(Negation x)
        | _ -> None

    let (|NegationT|_|) = term >> (|Negation|_|)

    let (|Conjunction|_|) = function
        | Expression(Operator OperationType.LogicalAnd, xs, _) -> Some(Conjunction xs)
        | _ -> None

    let (|Disjunction|_|) = function
        | Expression(Operator OperationType.LogicalOr, xs, _) -> Some(Disjunction xs)
        | _ -> None

    let (|Xor|_|) = term >> function
        | Expression(Operator OperationType.LogicalXor, [x;y], _) -> Some(Xor(x, y))
        | _ -> None

    let (|ShiftLeft|_|) = term >> function
        | Expression(Operator OperationType.ShiftLeft, [x;y], t) -> Some(ShiftLeft(x, y, t))
        | _ -> None

    let (|ShiftRight|_|) = term >> function
        | Expression(Operator OperationType.ShiftRight, [x;y], t) -> Some(ShiftRight(x, y, t))
        | _ -> None

    let (|ShiftRightThroughCast|_|) = function
        | CastExpr(ShiftRight(a, b, Numeric(Id t)), _, (Numeric(Id castType) as t')) when not <| TypeUtils.isLessForNumericTypes castType t ->
            Some(ShiftRightThroughCast(primitiveCast a t', b, t'))
        | _ -> None

    let (|ConcreteHeapAddress|_|) = function
        | Concrete(:? concreteHeapAddress as a, AddressType) -> ConcreteHeapAddress a |> Some
        | _ -> None

    let getConcreteHeapAddress = term >> function
        | ConcreteHeapAddress(addr) -> addr
        | _ -> __unreachable__()

    let isConcreteHeapAddress term =
        match term.term with
        | ConcreteHeapAddress _ -> true
        | _ -> false

    let rec timeOf (address : heapAddress) =
        match address.term with
        | ConcreteHeapAddress addr -> addr
        | Constant(_, source, _) -> source.Time
        | Union gvs -> List.fold (fun m (_, v) -> VectorTime.max m (timeOf v)) VectorTime.zero gvs
        | _ -> internalfailf "timeOf : expected heap address, but got %O" address

    let compareTerms t1 t2 =
        match t1.term, t2.term with
        | Concrete(:? IComparable as x, _), Concrete(:? IComparable as y, _) -> x.CompareTo y
        | Concrete(:? IComparable, _), _ -> -1
        | _, Concrete(:? IComparable, _) -> 1
        | _ -> compare (toString t1) (toString t2)

    let rec private foldChildren folder state term =
        match term.term with
        | Constant(_, source, _) ->
            foldSeq folder source.SubTerms state
        | Expression(_, args, _) ->
            foldSeq folder args state
        | Struct(fields, _) ->
            foldSeq folder (PersistentDict.values fields) state
        | Ref address ->
            foldAddress folder state address
        | Ptr(address, _, indent) ->
            let state = Option.fold (foldAddress folder) state address
            match indent with
            | None -> state
            | Some indent -> doFold folder state indent
        | GuardedValues(gs, vs) ->
            foldSeq folder gs state |> foldSeq folder vs
        | _ -> state

    and doFold folder state term =
        let state = foldChildren folder state term
        folder state term

    and foldAddress folder state = function
        | PrimitiveStackLocation _
        | StaticField _
        | BoxedLocation _ -> state
        | ClassField(addr, _) -> doFold folder state addr
        | ArrayIndex(addr, idcs, _) ->
            let state = doFold folder state addr
            foldSeq folder idcs state
        | StructField(addr, _) -> foldAddress folder state addr
        | ArrayLength(addr, idx, _)
        | ArrayLowerBound(addr, idx, _) ->
            let state = doFold folder state addr
            doFold folder state idx
        | StackBufferIndex(_, idx) -> doFold folder state idx

    and private foldSeq folder terms state =
        Seq.fold (doFold folder) state terms

    let fold folder state terms =
        foldSeq folder terms state

    let iter action term =
        doFold (fun () -> action) () term

    let discoverConstants terms =
        let result = HashSet<term>()
        let addConstant = function
            | {term = Constant _} as constant -> result.Add constant |> ignore
            | _ -> ()
        Seq.iter (iter addConstant) terms
        result :> ISet<term>


    let private foldFields isStatic folder acc typ =
        let dotNetType = Types.toDotNetType typ
        let fields = Reflection.fieldsOf isStatic dotNetType
        let addField heap (field, typ) =
            let termType = typ |> Types.Constructor.fromDotNetType
            folder heap field termType
        FSharp.Collections.Array.fold addField acc fields

    let private makeFields isStatic makeField typ =
        let folder fields field termType =
            let value = makeField field termType
            PersistentDict.add field value fields
        foldFields isStatic folder PersistentDict.empty typ

    let makeStruct isStatic makeField typ =
        let fields = makeFields isStatic makeField typ
        Struct fields typ

    let rec makeDefaultValue typ =
        match typ with
        | Bool -> False
        | Numeric(Id t) when t.IsEnum -> castConcrete (System.Activator.CreateInstance t) t
        | Numeric(Id t) -> castConcrete 0 t
        | ArrayType _
        | ClassType _
        | InterfaceType _ -> nullRef
        | TypeVariable(Id t) when TypeUtils.isReferenceTypeParameter t -> nullRef
        | TypeVariable(Id t) -> __insufficientInformation__ "Cannot instantiate value of undefined type %O" t
        | StructType _ -> makeStruct false (fun _ t -> makeDefaultValue t) typ
        | Pointer typ -> makeNullPtr typ
        | _ -> __notImplemented__()
