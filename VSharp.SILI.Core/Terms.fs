namespace VSharp.Core

open System.Reflection
open VSharp
open VSharp.CSharpUtils
open VSharp.TypeUtils
open EnumUtils
open System
open System.Collections.Generic

type IMethod =
    inherit IComparable
    abstract Name : string
    abstract FullName : string
    abstract ReturnType : Type
    abstract DeclaringType : Type
    abstract ReflectedType : Type
    abstract Parameters : Reflection.ParameterInfo[]
    abstract LocalVariables : IList<Reflection.LocalVariableInfo>
    abstract HasThis : bool
    abstract HasParameterOnStack : bool
    abstract IsConstructor : bool
    abstract IsStaticConstructor : bool
    abstract IsExternalMethod : bool
    abstract ContainsGenericParameters : bool
    abstract GenericArguments : Type[]
    abstract SubstituteTypeVariables : (Type -> Type) -> IMethod
    abstract ResolveOverrideInType : Type -> IMethod
    abstract CanBeOverriddenInType : Type -> bool
    abstract IsImplementedInType : Type -> bool
    abstract MethodBase : System.Reflection.MethodBase

[<CustomEquality;CustomComparison>]
type stackKey =
    | ThisKey of IMethod
    | ParameterKey of Reflection.ParameterInfo
    | LocalVariableKey of Reflection.LocalVariableInfo * IMethod
    | TemporaryLocalVariableKey of Type * int
    override x.ToString() =
        match x with
        | ThisKey _ -> "this"
        | ParameterKey pi -> if String.IsNullOrEmpty pi.Name then "#" + toString pi.Position else pi.Name
        | LocalVariableKey (lvi,_) -> "__loc__" + lvi.LocalIndex.ToString()
        | TemporaryLocalVariableKey (typ, index) -> $"__tmp__%s{Reflection.getFullTypeName typ}%d{index}"
    override x.GetHashCode() =
        let fullname =
            match x with
            | ThisKey m -> $"%s{m.FullName}##this"
            | ParameterKey pi -> $"{pi.Member}##{pi}##%d{pi.Position}"
            | LocalVariableKey (lvi, m) -> $"{m.FullName}##%s{lvi.ToString()}"
            | TemporaryLocalVariableKey (typ, index) -> $"temporary##%s{Reflection.getFullTypeName typ}%d{index}"
        fullname.GetDeterministicHashCode()
    interface IComparable with
        override x.CompareTo(other : obj) =
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
        | TemporaryLocalVariableKey (typ, _) -> typ
    member x.Map typeSubst =
        match x with
        | ThisKey m -> ThisKey (m.SubstituteTypeVariables typeSubst)
        | ParameterKey p -> ParameterKey (Reflection.concretizeParameter p typeSubst)
        | LocalVariableKey(l, m) ->
            let m' = m.SubstituteTypeVariables typeSubst
            let l' = m.LocalVariables[l.LocalIndex]
            LocalVariableKey(l', m')
        | TemporaryLocalVariableKey (typ, index) ->
            // TODO: index may become inconsistent here
            TemporaryLocalVariableKey((Reflection.concretizeType typeSubst typ), index)

type concreteHeapAddress = vectorTime

type arrayType =
    { elemType : Type; dimension : int; isVector : bool }
    with
    static member CreateVector (elemType : Type) =
        { elemType = elemType; dimension = 1; isVector = true }

    static member CharVector = { elemType = typeof<char>; dimension = 1; isVector = true }

    member x.IsVector = x.isVector && (assert(x.dimension = 1); true)

    member x.IsCharVector = x.IsVector && x.elemType = typeof<char>

[<StructuralEquality;NoComparison>]
type operation =
    | Operator of OperationType
    | Application of StandardFunction
    | Cast of Type * Type
    | Combine
    member x.priority =
        match x with
        | Operator op -> Operations.operationPriority op
        | Application _ -> Operations.maxPriority
        | Cast _ -> Operations.maxPriority - 1
        | Combine -> Operations.maxPriority

// TODO: get rid of Nop!
[<StructuralEquality;NoComparison>]
type termNode =
    | Nop
    | Concrete of obj * Type
    | Constant of string transparent * ISymbolicConstantSource * Type
    | Expression of operation * term list * Type
    | Struct of pdict<fieldId, term> * Type
    | HeapRef of heapAddress * Type
    | Ref of address
    // NOTE: use ptr only in case of reinterpretation: changed sight type or address arithmetic, otherwise use ref instead
    | Ptr of pointerBase * Type * term // base address * sight type * offset (in bytes)
    | Slice of term * list<term * term * term> // what term to slice * list of slices (start byte * end byte * position inside combine)
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

        let rec toStr parentPriority indent term k =
            match term with
            | Nop -> k "<VOID>"
            | Constant(name, _, _) -> k name.v
            | Concrete(_, (ClassType(t, _) as typ)) when isSubtypeOrEqual t typedefof<Delegate> ->
                $"<Lambda Expression {typ}>" |> k
            | Concrete(obj, AddressType) when (obj :?> int32 list) = [0] -> k "null"
            | Concrete(c, Char) when c :?> char = '\000' -> k "'\\000'"
            | Concrete(c, Char) -> k $"'{c}'"
            | Concrete(:? concreteHeapAddress as addr, AddressType) -> VectorTime.print addr |> k
            | Concrete(value, _) -> value.ToString() |> k
            | Expression(operation, operands, _) ->
                match operation with
                | Operator operator when Operations.operationArity operator = 1 ->
                    assert (List.length operands = 1)
                    let operand = List.head operands
                    let opStr = Operations.operationToString operator |> checkExpression operation.priority parentPriority
                    toStr operation.priority indent operand.term (fun printedOperand ->
                    sprintf (Printf.StringFormat<string->string>(opStr)) printedOperand |> k)
                | Operator operator ->
                    assert (List.length operands >= 2)
                    Cps.List.mapk (fun x k -> toStr operation.priority indent (getTerm x) k) operands (fun printedOperands ->
                    let sortedOperands = if Operations.isCommutative operator then List.sort printedOperands else printedOperands
                    sortedOperands
                        |> String.concat (Operations.operationToString operator)
                        |> checkExpression operation.priority parentPriority
                        |> k)
                | Cast(_, dest) ->
                    assert (List.length operands = 1)
                    toStr operation.priority indent (List.head operands).term (fun term ->
                    $"({dest} %s{term})"
                        |> checkExpression operation.priority parentPriority
                        |> k)
                | Application f ->
                    Cps.List.mapk (getTerm >> toStr -1 indent) operands (fun results ->
                    results |> join ", " |> sprintf "%O(%s)" f |> k)
                | Combine ->
                    Cps.List.mapk (getTerm >> toStr -1 indent) operands (fun results ->
                    results |> join ", " |> sprintf "Combine(%s)" |> k)
            | Struct(fields, t) ->
                fieldsToString indent fields |> sprintf "%O STRUCT [%s]" t |> k
            | Union(guardedTerms) ->
                let guardedToString (guard, term) k =
                    toStringWithParentIndent indent guard (fun guardString ->
                    toStringWithParentIndent indent term (fun termString ->
                    $"| %s{guardString} ~> %s{termString}" |> k))
                Cps.Seq.mapk guardedToString guardedTerms (fun guards ->
                let printed = guards |> Seq.sort |> join ("\n" + indent)
                formatIfNotEmpty (formatWithIndent indent) printed |> sprintf "UNION[%s]" |> k)
            | HeapRef({term = Concrete(obj, AddressType)}, _) when (obj :?> int32 list) = [0] -> k "NullRef"
            | HeapRef(address, baseType) -> $"(HeapRef {address} to {baseType})" |> k
            | Ref address -> $"({address.Zone()}Ref {address})" |> k
            | Ptr(HeapLocation(address, _), typ, shift) ->
                $"(HeapPtr {address} as {typ}, offset = {shift})" |> k
            | Ptr(StackLocation loc, typ, shift) ->
                $"(StackPtr {loc} as {typ}, offset = {shift})" |> k
            | Ptr(StaticLocation t, typ, shift) ->
                $"(StaticsPtr {t} as {typ}, offset = {shift})" |> k
            | Slice(term, slices) ->
                let slices = List.map (fun (s, e, _) -> $"[{s} .. {e}]") slices |> join ", "
                $"Slice({term}, {slices})" |> k

        and fieldsToString indent fields =
            let stringResult = PersistentDict.toString "| %O ~> %O" ("\n" + indent) toString toString toString fields
            formatIfNotEmpty (formatWithIndent indent) stringResult

        and toStringWithIndent indent term k = toStr -1 indent term.term k

        and toStringWithParentIndent parentIndent term k = toStringWithIndent (extendIndent parentIndent) term k

        toStr -1 "\t" x id

and heapAddress = term // only Concrete(:? concreteHeapAddress) or Constant of type AddressType!

and pointerBase =
    | StackLocation of stackKey
    | HeapLocation of heapAddress * Type // Null or virtual address * sight type of address
    | StaticLocation of Type
    member x.Zone() =
        match x with
        | StackLocation _ -> "Stack"
        | HeapLocation _ -> "Heap"
        | StaticLocation _ -> "Statics"

and address =
    | PrimitiveStackLocation of stackKey
    | StructField of address * fieldId
    | StackBufferIndex of stackKey * term
    | BoxedLocation of term * Type // TODO: delete type from boxed location?
    | ClassField of heapAddress * fieldId
    | ArrayIndex of heapAddress * term list * arrayType
    | ArrayLowerBound of heapAddress * term * arrayType
    | ArrayLength of heapAddress * term * arrayType
    | StaticField of Type * fieldId
    override x.ToString() =
        match x with
        | PrimitiveStackLocation key -> toString key
        | ClassField(addr, field) -> $"{addr}.{field}"
        | ArrayIndex(addr, indices, _) -> sprintf "%O[%s]" addr (List.map toString indices |> join ", ")
        | StaticField(typ, field) -> $"{typ}.{field}"
        | StructField(addr, field) -> $"{addr}.{field}"
        | ArrayLength(addr, dim, _) -> $"Length({addr}, {dim})"
        | BoxedLocation(addr, typ) -> $"{typ}^{addr}"
        | StackBufferIndex(key, idx) -> $"{key}[{idx}]"
        | ArrayLowerBound(addr, dim, _) -> $"LowerBound({addr}, {dim})"
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
    member x.TypeOfLocation with get() =
        match x with
        | ClassField(_, field)
        | StructField(_, field)
        | StaticField(_, field) -> field.typ
        | ArrayIndex(_, _, { elemType = elementType }) -> elementType
        | BoxedLocation(_, typ) -> typ
        | ArrayLength _
        | ArrayLowerBound  _ -> lengthType
        | StackBufferIndex _ -> typeof<int8>
        | PrimitiveStackLocation loc -> loc.TypeOfLocation

and
    [<CustomEquality;NoComparison>]
    term =
        {term : termNode; hc : int}
        override x.ToString() = x.term.ToString()
        override x.GetHashCode() = x.hc
        override x.Equals(o : obj) =
            match o with
            | :? term as other -> LanguagePrimitives.PhysicalEquality x.term other.term
            | _ -> false

and
    ISymbolicConstantSource =
        abstract SubTerms : term seq
        abstract Time : vectorTime
        abstract TypeOfLocation : Type

and delegateInfo =
    {
        methodInfo : System.Reflection.MethodInfo
        target : term
        delegateType : Type
    }
    static member Create(methodInfo, target, t) =
        { methodInfo = methodInfo; target = target; delegateType = t }

type INonComposableSymbolicConstantSource =
    inherit ISymbolicConstantSource

module HashMap =
    let hashMap = weakdict<termNode, term>()
    let addTerm node =
        let result = ref { term = node; hc = 0 }
        if hashMap.TryGetValue(node, result)
            then result.Value
            else
                let hc = hash node
                let term = { term = node; hc = hc }
                hashMap.Add(node, term)
                term
    let clear() = hashMap.Clear()

[<AutoOpen>]
module internal Terms =

    let mutable charsArePretty = true

    let configureChars arePretty =
        charsArePretty <- arePretty

    let term (term : term) = term.term

// --------------------------------------- Primitives ---------------------------------------

    let Nop() = HashMap.addTerm Nop
    let Concrete obj typ = HashMap.addTerm (Concrete(obj, typ))
    let Constant name source typ = HashMap.addTerm (Constant({v=name}, source, typ))
    let Expression op args typ = HashMap.addTerm (Expression(op, args, typ))
    let Struct fields typ = HashMap.addTerm (Struct(fields, typ))
    let HeapRef address baseType =
        HashMap.addTerm (HeapRef(address, baseType))
    let Ref address =
        match address with
        | ArrayIndex(_, indices, { dimension = dim }) -> assert(List.length indices = dim)
        | _ -> ()
        HashMap.addTerm (Ref address)
    let Ptr baseAddress typ offset = HashMap.addTerm (Ptr(baseAddress, typ, offset))
    let Slice term slices = HashMap.addTerm (Slice(term, slices))
    let ConcreteHeapAddress (addr : concreteHeapAddress) = Concrete addr addressType
    let Union gvs =
        if List.length gvs < 2 then internalfail "Empty and one-element unions are forbidden!"
        HashMap.addTerm (Union gvs)

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
        | Concrete(b, Bool) when (b :?> bool) -> true
        | _ -> false

    let isFalse = term >> function
        | Concrete(b, Bool) when not (b :?> bool) -> true
        | _ -> false

    let operationOf = term >> function
        | Expression(op, _, _) -> op
        | term -> internalfail $"expression expected, {term} received"

    let argumentsOf = term >> function
        | Expression(_, args, _) -> args
        | term -> internalfail $"expression expected, {term} received"

    let fieldsOf = term >> function
        | Struct(fields, _) -> fields
        | term -> internalfail $"struct or class expected, {term} received"

    let private typeOfUnion getType gvs =
        let types = List.map (snd >> getType) gvs
        match types with
        | [] -> __unreachable__()
        | t::ts ->
            let allSame =
                List.forall ((=) t) ts
            if allSame then t
            else internalfail $"evaluating type of unexpected union {gvs}!"

    let commonTypeOf getType term =
        match term.term with
        | Nop -> typeof<System.Void>
        | Union gvs -> typeOfUnion getType gvs
        | _ -> getType term

    let sightTypeOfPtr =
        let getTypeOfPtr = term >> function
            | Ptr(_, typ, _) -> typ
            | term -> internalfail $"expected pointer, but got {term}"
        commonTypeOf getTypeOfPtr

    let rec typeOfRef term =
        let getTypeOfRef term =
            match term.term with
            | HeapRef(_, t) -> t
            | Ref address -> address.TypeOfLocation
            | Ptr(_, sightType, _) -> sightType
            | _ when typeOf term |> isNative -> typeof<byte>
            | term -> internalfail $"expected reference, but got {term}"
        commonTypeOf getTypeOfRef term

    and typeOf term =
        let getType term =
            match term.term with
            | Concrete(_, t)
            | Constant(_, _, t)
            | Expression(_, _, t)
            | HeapRef(_, t)
            | Struct(_, t) -> t
            | Ref _ -> (typeOfRef term).MakeByRefType()
            | Ptr _ -> (sightTypeOfPtr term).MakePointerType()
            | _ -> internalfail $"getting type of unexpected term {term}"
        commonTypeOf getType term

    let symbolicTypeToArrayType = function
        | ArrayType(elementType, dim) ->
            match dim with
            | Vector -> arrayType.CreateVector elementType
            | ConcreteDimension d -> { elemType = elementType; dimension = d; isVector = false }
            | SymbolicDimension -> __insufficientInformation__ "Cannot process array of unknown dimension!"
        | typ -> internalfail $"symbolicTypeToArrayType: expected array type, but got {typ}"

    let arrayTypeToSymbolicType arrayType =
        if arrayType.isVector then arrayType.elemType.MakeArrayType()
        else arrayType.elemType.MakeArrayType(arrayType.dimension)

    let sizeOf = typeOf >> internalSizeOf

    let bitSizeOf term resultingType = bitSizeOfType (typeOf term) resultingType

    let isBool t = typeOf t = typeof<bool>
    let isNumeric t = typeOf t |> isNumeric

    let rec isStruct term =
        match term.term with
        | Struct _ -> true
        | Union gvs -> List.forall (snd >> isStruct) gvs
        | _ -> false

    let rec isReference term =
        match term.term with
        | HeapRef _
        | Ref _ -> true
        | Union gvs -> List.forall (snd >> isReference) gvs
        | _ -> false

    let rec isPtr term =
        match term.term with
        | Ptr _ -> true
        | Union gvs -> List.forall (snd >> isPtr) gvs
        | _ -> false

    let rec isRefOrPtr term =
        match term.term with
        | HeapRef _
        | Ref _
        | Ptr _ -> true
        | Union gvs -> List.forall (snd >> isRefOrPtr) gvs
        | _ -> false

    let zeroAddress() =
        Concrete VectorTime.zero addressType

    let makeNumber n =
        Concrete n (n.GetType())

    let makeNullPtr typ =
        Ptr (HeapLocation(zeroAddress(), typ)) typ (makeNumber 0)

    // Only for concretes: there will never be null type
    let canCastConcrete (concrete : obj) targetType =
        let actualType = getTypeOfConcrete concrete
        actualType = targetType || targetType.IsAssignableFrom(actualType)

    let castConcrete (concrete : obj) (t : Type) =
        let actualType = getTypeOfConcrete concrete
        let functionIsCastedToMethodPointer () =
            typedefof<System.Reflection.MethodBase>.IsAssignableFrom(actualType) && typedefof<IntPtr>.IsAssignableFrom(t)
        if actualType = t then
            Concrete concrete t
        elif actualType = typeof<int> && t = typeof<IntPtr> then
            Concrete (IntPtr(concrete :?> int)) t
        elif actualType = typeof<uint> && t = typeof<UIntPtr> then
            Concrete (UIntPtr(concrete :?> uint)) t
        elif t.IsEnum && TypeUtils.isNumeric actualType then
            let underlyingType = getEnumUnderlyingTypeChecked t
            let underlyingValue = convert concrete underlyingType
            let enumValue = convert underlyingValue t
            Concrete enumValue t
        elif actualType.IsEnum && TypeUtils.isNumeric t then
            try
                Concrete (Convert.ChangeType(concrete, t)) t
            with :? OverflowException ->
                Concrete concrete t
        elif canConvert actualType t then
            Concrete (convert concrete t) t
        elif t.IsAssignableFrom(actualType) then
            Concrete concrete t
        elif functionIsCastedToMethodPointer() then
            Concrete concrete actualType
        else
            let tName = Reflection.getFullTypeName t
            let actualTypeName = Reflection.getFullTypeName actualType
            raise (InvalidCastException $"Cannot cast {actualTypeName} to {tName}!")

    let True() =
        Concrete (box true) typeof<bool>

    let False() =
        Concrete (box false) typeof<bool>

    let makeBool predicate =
        if predicate then True() else False()

    let makeIndex (i : int) =
        Concrete i indexType

    let nullRef t =
        HeapRef (zeroAddress()) t

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

    let (|Combined|_|) = function
        | Expression(Combine, args, t) -> Some(Combined(args, t))
        | _ -> None

    let rec private makeCast term fromType toType =
        match term, toType with
        | _ when fromType = toType -> term
        | CastExpr(x, xType, Numeric t), Numeric toType
            when not (isLessForNumericTypes t toType) && numericSameSign t toType ->
                makeCast x xType toType
        | CastExpr(x, Numeric xType, Numeric t), Numeric toType
            when not <| isLessForNumericTypes t xType && not <| isLessForNumericTypes toType t && numericSameSign t toType ->
                makeCast x xType toType
        | _ -> Expression (Cast(fromType, toType)) [term] toType

    let rec primitiveCast term targetType =
        match term.term, targetType with
        | _ when typeOf term = targetType -> term
        | _, Pointer t
        | _, ByRef t ->
            makeDetachedPtr term t
        | Concrete(value, _), _ -> castConcrete value targetType
        | Combined(slices, t), _ when isIntegral t && isIntegral targetType && internalSizeOf t = internalSizeOf targetType ->
            // TODO: simplify for narrow cast
            combine slices targetType
        // TODO: make cast to Bool like function Transform2BooleanTerm
        | Constant(_, _, t), _
        | Expression(_, _, t), _ -> makeCast term t targetType
        | Union gvs, _ -> gvs |> List.map (fun (g, v) -> (g, primitiveCast v targetType)) |> Union
        | _ -> __unreachable__()

    // Detached pointer is pointer, which is not fixed to any object
    // It contains only offset
    and makeDetachedPtr value t =
        match value.term with
        | _ when isRefOrPtr value -> value
        | _ ->
            let offset = primitiveCast value typeof<int>
            Ptr (HeapLocation(zeroAddress(), typeof<Void>)) t offset

    and (|DetachedPtr|_|) = function
        | Ptr(HeapLocation(address, _), _, offset) when address = zeroAddress() ->
            Some(DetachedPtr offset)
        | _ -> None

    // This function is used only for creating IntPtr structure
    and makeIntPtr value =
        match value.term with
        | DetachedPtr offset -> primitiveCast offset typeof<IntPtr>
        | _ when isRefOrPtr value -> value
        | _ -> primitiveCast value typeof<IntPtr>

    // This function is used only for creating UIntPtr structure
    and makeUIntPtr value =
        match value.term with
        | DetachedPtr offset -> primitiveCast offset typeof<UIntPtr>
        | _ when isRefOrPtr value -> value
        | _ -> primitiveCast value typeof<UIntPtr>

    // Transforms IntPtr or UIntPtr to detached pointer
    and nativeToPointer ptr =
        match ptr.term with
        | _ when isRefOrPtr ptr -> ptr
        | Concrete _
        | Constant _
        | Expression _ ->
            makeDetachedPtr ptr typeof<Void>
        | Union gvs -> gvs |> List.map (fun (g, v) -> (g, nativeToPointer v)) |> Union
        | _ -> internalfail $"nativeToPointer: unexpected pointer {ptr}"

    and negate term =
        assert(isBool term)
        makeUnary OperationType.LogicalNot term typeof<bool>

    and createCombinedDelegate (delegates : term list) typ =
        assert(List.isEmpty delegates |> not)
        if List.length delegates = 1 then List.head delegates
        else Concrete delegates typ

    and concreteDelegate methodInfo target delegateType =
        Concrete (delegateInfo.Create(methodInfo, target, delegateType)) delegateType

    and (|True|_|) term = if isTrue term then Some True else None
    and (|False|_|) term = if isFalse term then Some False else None

    and (|ConcreteT|_|) = term >> function
        | Concrete(name, typ) -> Some(ConcreteT(name, typ))
        | _ -> None

    and (|UnionT|_|) = term >> function
        | Union gvs -> Some(UnionT gvs)
        | _ -> None

    and (|GuardedValues|_|) = function // TODO: this could be ineffective (because of unzip)
        | Union gvs -> Some(GuardedValues(List.unzip gvs))
        | _ -> None

    and (|UnaryMinus|_|) = function
        | Expression(Operator OperationType.UnaryMinus, [x], t) -> Some(UnaryMinus(x, t))
        | _ -> None

    and (|UnaryMinusT|_|) = term >> (|UnaryMinus|_|)

    and (|Add|_|) = term >> function
        | Expression(Operator OperationType.Add, [x;y], t) -> Some(Add(x, y, t))
        | _ -> None

    and (|Sub|_|) = term >> function
        | Expression(Operator OperationType.Subtract, [x;y], t) -> Some(Sub(x, y, t))
        | _ -> None

    and (|Mul|_|) = term >> function
        | Expression(Operator OperationType.Multiply, [x;y], t) -> Some(Mul(x, y, t))
        | _ -> None

    and (|Div|_|) = term >> function
        | Expression(Operator OperationType.Divide, [x;y], t) -> Some(Div(x, y, t, true))
        | Expression(Operator OperationType.Divide_Un, [x;y], t) -> Some(Div(x, y, t, false))
        | _ -> None

    and (|Rem|_|) = term >> function
        | Expression(Operator OperationType.Remainder, [x;y], t)
        | Expression(Operator OperationType.Remainder_Un, [x;y], t) -> Some(Rem(x, y, t))
        | _ -> None

    and (|Negation|_|) = function
        | Expression(Operator OperationType.LogicalNot, [x], _) -> Some(Negation x)
        | _ -> None

    and (|NegationT|_|) = term >> (|Negation|_|)

    and (|Conjunction|_|) = function
        | Expression(Operator OperationType.LogicalAnd, xs, _) -> Some(Conjunction xs)
        | _ -> None

    and (|Disjunction|_|) = function
        | Expression(Operator OperationType.LogicalOr, xs, _) -> Some(Disjunction xs)
        | _ -> None

    and (|Xor|_|) = term >> function
        | Expression(Operator OperationType.LogicalXor, [x;y], _) -> Some(Xor(x, y))
        | _ -> None

    and (|ShiftLeft|_|) = term >> function
        | Expression(Operator OperationType.ShiftLeft, [x;y], t) -> Some(ShiftLeft(x, y, t))
        | _ -> None

    and (|ShiftRight|_|) = term >> function
        | Expression(Operator OperationType.ShiftRight, [x;y], t) -> Some(ShiftRight(x, y, t, true))
        | Expression(Operator OperationType.ShiftRight_Un, [x;y], t) -> Some(ShiftRight(x, y, t, false))
        | _ -> None

    and (|ShiftRightThroughCast|_|) = function
        | CastExpr(ShiftRight(a, b, Numeric t, _), _, Numeric castType) when not <| isLessForNumericTypes castType t ->
            Some(ShiftRightThroughCast(primitiveCast a castType, b, castType))
        | _ -> None

    and (|CombinedTerm|_|) = term >> (|Combined|_|)

    and (|CombinedDelegate|_|) = function
        | Concrete(:? list<term> as delegates, t) ->
            assert(t.IsAssignableTo typeof<Delegate>)
            CombinedDelegate delegates |> Some
        | _ -> None

    and (|ConcreteDelegate|_|) (termNode : termNode) =
        match termNode with
        | Concrete(:? delegateInfo as d, t) ->
            assert(t.IsAssignableTo typeof<Delegate>)
            ConcreteDelegate d |> Some
        | _ -> None

    and (|ConcreteHeapAddress|_|) = function
        | Concrete(:? concreteHeapAddress as a, AddressType) -> ConcreteHeapAddress a |> Some
        | _ -> None

    and getConcreteHeapAddress term =
        match term.term with
        | ConcreteHeapAddress(address) -> address
        | _ -> __unreachable__()

    and tryIntListFromTermList (termList : term list) =
        let addElement term concreteList k =
            match term.term with
            | Concrete(:? int16 as i, _) -> int i :: concreteList |> k
            | Concrete(:? uint16 as i, _) -> int i :: concreteList |> k
            | Concrete(:? char as i, _) -> int i :: concreteList |> k
            | Concrete(:? int as i, _) -> i :: concreteList |> k
            | Concrete(:? uint as i, _) -> int i :: concreteList |> k
            | Concrete(:? int64 as i, _) -> int i :: concreteList |> k
            | Concrete(:? uint64 as i, _) -> int i :: concreteList |> k
            | _ -> None
        Cps.List.foldrk addElement List.empty termList Some

    and isConcreteHeapAddress term =
        match term.term with
        | ConcreteHeapAddress _ -> true
        | _ -> false

    and private structToBytes (s : ValueType) =
        let t = s.GetType()
        let size = internalSizeOf t
        let array : byte array = Array.zeroCreate size
        if t.IsGenericType then
            let fields = Reflection.fieldsOf false t
            for _, fi in fields do
                let fieldValue = fi.GetValue s
                let fieldBytes = concreteToBytes fieldValue
                let fieldOffset = LayoutUtils.GetFieldOffset fi
                Array.Copy(fieldBytes, 0, array, fieldOffset, Array.length fieldBytes)
        else
            assert(Reflection.fieldsOf false t |> Array.forall (fun (_, f) -> f.FieldType.IsValueType))
            let mutable ptr = IntPtr.Zero
            try
                ptr <- System.Runtime.InteropServices.Marshal.AllocHGlobal(size)
                System.Runtime.InteropServices.Marshal.StructureToPtr(s, ptr, true)
                System.Runtime.InteropServices.Marshal.Copy(ptr, array, 0, size)
            finally
                System.Runtime.InteropServices.Marshal.FreeHGlobal(ptr)
        array

    and private concreteToBytes (obj : obj) =
        match obj with
        | _ when obj = null -> internalSizeOf typeof<obj> |> Array.zeroCreate
        | :? byte as o -> Array.singleton o
        | :? sbyte as o -> Array.singleton (byte o)
        | :? int16 as o -> BitConverter.GetBytes o
        | :? uint16 as o -> BitConverter.GetBytes o
        | :? int as o -> BitConverter.GetBytes o
        | :? uint32 as o -> BitConverter.GetBytes o
        | :? int64 as o -> BitConverter.GetBytes o
        | :? uint64 as o -> BitConverter.GetBytes o
        | :? float32 as o -> BitConverter.GetBytes o
        | :? double as o -> BitConverter.GetBytes o
        | :? bool as o -> BitConverter.GetBytes o
        | :? char as o -> BitConverter.GetBytes o
        | _ when obj.GetType().IsEnum ->
            let i = Convert.ChangeType(obj, getEnumUnderlyingTypeChecked (obj.GetType()))
            concreteToBytes i
        | :? ValueType as o -> structToBytes o
        | _ -> internalfail $"getting bytes from concrete: unexpected obj {obj}"

    and private bytesToNullable (bytes : byte[]) (t : Type) =
        let fields = Reflection.fieldsOf false t
        assert(Array.length fields = 2)
        let hasValueField = fields |> Array.find (fun (f, _) -> f.name = "hasValue") |> snd
        assert(hasValueField.FieldType = typeof<bool>)
        let hasValueOffset = LayoutUtils.GetFieldOffset hasValueField
        let hasValueSize = internalSizeOf typeof<bool>
        let hasValueBytes = bytes[hasValueOffset .. hasValueOffset + hasValueSize - 1]
        let hasValue = bytesToObj hasValueBytes typeof<bool> :?> bool
        if hasValue then
            let valueField = fields |> Array.find (fun (f, _) -> f.name = "value") |> snd
            let underlyingType = Nullable.GetUnderlyingType t
            assert(valueField.FieldType = underlyingType)
            let valueOffset = LayoutUtils.GetFieldOffset valueField
            let valueSize = internalSizeOf underlyingType
            bytesToObj bytes[valueOffset .. valueOffset + valueSize - 1] underlyingType
        else null

    and private bytesToStruct (bytes : byte[]) (t : Type) =
        if t.IsGenericType then
            let fields = Reflection.fieldsOf false t
            assert(fields |> Array.forall (fun (_, f) -> f.FieldType.IsValueType))
            if isNullable t then bytesToNullable bytes t
            else
                let obj = Reflection.defaultOf t
                for _, fi in fields do
                    let offset = LayoutUtils.GetFieldOffset fi
                    let fieldType = fi.FieldType
                    let size = internalSizeOf fieldType
                    let value = bytesToObj bytes[offset .. offset + size - 1] fieldType
                    fi.SetValue(obj, value)
                obj
        else
            assert(Reflection.fieldsOf false t |> Array.forall (fun (_, f) -> f.FieldType.IsValueType))
            let size = internalSizeOf t
            let mutable ptr = IntPtr.Zero
            try
                ptr <- System.Runtime.InteropServices.Marshal.AllocHGlobal(size)
                System.Runtime.InteropServices.Marshal.Copy(bytes, 0, ptr, size)
                System.Runtime.InteropServices.Marshal.PtrToStructure(ptr, t)
            finally
                System.Runtime.InteropServices.Marshal.FreeHGlobal(ptr)

    and private bytesToObj (bytes : byte[]) t : obj =
        let span = ReadOnlySpan<byte>(bytes)
        match t with
        | _ when t = typeof<byte> -> Array.head bytes :> obj
        | _ when t = typeof<sbyte> -> sbyte (Array.head bytes) :> obj
        | _ when t = typeof<int16> -> BitConverter.ToInt16 span :> obj
        | _ when t = typeof<uint16> -> BitConverter.ToUInt16 span :> obj
        | _ when t = typeof<int> -> BitConverter.ToInt32 span :> obj
        | _ when t = typeof<uint32> -> BitConverter.ToUInt32 span :> obj
        | _ when t = typeof<int64> -> BitConverter.ToInt64 span :> obj
        | _ when t = typeof<uint64> -> BitConverter.ToUInt64 span :> obj
        | _ when t = typeof<float32> -> BitConverter.ToSingle span :> obj
        | _ when t = typeof<double> -> BitConverter.ToDouble span :> obj
        | _ when t = typeof<bool> -> BitConverter.ToBoolean span :> obj
        | _ when t = typeof<char> -> BitConverter.ToChar span :> obj
        | _ when t = typeof<IntPtr> ->
            if sizeof<IntPtr> = sizeof<int> then
                BitConverter.ToInt32 span |> IntPtr :> obj
            elif sizeof<IntPtr> = sizeof<int64> then
                BitConverter.ToInt64 span |> IntPtr :> obj
            else internalfail "bytesToObj: unexpected IntPtr size"
        | _ when t = typeof<UIntPtr> ->
            if sizeof<UIntPtr> = sizeof<uint> then
                BitConverter.ToUInt32 span |> UIntPtr :> obj
            elif sizeof<UIntPtr> = sizeof<uint64> then
                BitConverter.ToUInt64 span |> UIntPtr :> obj
            else internalfail "bytesToObj: unexpected UIntPtr size"
        | _ when t = typeof<Reflection.Pointer> ->
            let intPtr =
                if sizeof<IntPtr> = sizeof<int> then
                    BitConverter.ToInt32 span |> IntPtr
                elif sizeof<IntPtr> = sizeof<int64> then
                    BitConverter.ToInt64 span |> IntPtr
                else internalfail "bytesToObj: unexpected IntPtr size"
            Reflection.Pointer.Box(intPtr.ToPointer(), typeof<Void>.MakePointerType())
        | _ when t.IsEnum ->
            let i = getEnumUnderlyingTypeChecked t |> bytesToObj bytes
            Enum.ToObject(t, i)
        | StructType _ -> bytesToStruct bytes t
        | _ when not t.IsValueType && Array.forall (fun b -> b = 0uy) bytes -> null
        | _ -> internalfailf $"Creating object from bytes: unexpected object type {t}"

    and reinterpretConcretes (sliceTerms : term list) t =
        let bytes : byte array = internalSizeOf t |> Array.zeroCreate
        let combineLength = Array.length bytes
        let mutable solidPartBytes = Array.empty
        let mutable solidPartSize = 0
        for slice in sliceTerms do
            match slice.term with
            | Slice(term, [{term = Concrete(s, _)}, {term = Concrete(e, _)}, {term = Concrete(pos, _)}]) ->
                let o = slicingTerm term
                let sliceBytes = concreteToBytes o
                let sliceLength = Array.length sliceBytes
                let s = convert s typeof<int> :?> int
                assert(s >= 0 && s < sliceLength)
                let e = convert e typeof<int> :?> int
                assert(e > 0 && e <= sliceLength)
                let pos = convert pos typeof<int> :?> int
                assert(pos >= 0 && pos < combineLength)
                let count = e - s
                assert(count > 0)
                Array.blit sliceBytes s bytes pos count
            | Concrete(o, _) ->
                let sliceBytes = concreteToBytes o
                let sliceSize = Array.length sliceBytes
                if not (solidPartBytes = sliceBytes) then
                    let intersectingEnd = (min sliceSize solidPartSize) - 1
                    assert(Array.isEmpty solidPartBytes || sliceBytes[0..intersectingEnd] = solidPartBytes[0..intersectingEnd])
                    assert(sliceSize <= combineLength)
                    Array.blit sliceBytes 0 bytes 0 sliceSize
                    solidPartBytes <- sliceBytes
                    solidPartSize <- sliceSize
            | _ -> internalfailf $"Expected concrete slice, but got {slice}"
        bytesToObj bytes t

    and private slicingTerm term =
        match term with
        | {term = Concrete(o, _)} -> o
        | CombinedTerm(slices, t) -> reinterpretConcretes slices t
        | _ -> internalfail $"Getting slicing term: unexpected term {term}"

    and private allSlicesAreConcrete slices =
        let rec sliceIsConcrete t =
            match t.term with
            | Slice({term = Concrete _}, [({term = Concrete _}, {term = Concrete _}, {term = Concrete _})])
            | Concrete _ -> true
            | Combined(slices, _) -> allSlicesAreConcrete slices
            | _ -> false
        List.forall sliceIsConcrete slices

    and private isEmptySlice term =
        match term.term with
        | Slice(_, cuts) -> List.isEmpty cuts
        | _ -> false

    and createSlice term slices =
        assert(match term.term with Combined _ -> false | _ -> true)
        let termSize = sizeOf term
        let mutable cuts = List.empty
        let mutable left = 0
        let mutable right = termSize
        let mutable position = 0
        let mutable isValid = true
        let narrowed () =
            left > 0 || right < termSize || position <> 0
        let narrow (l, r, p as current) _ =
            let wereConcrete = List.isEmpty cuts
            match l.term, r.term, p.term with
            // TODO: need to simplify after symbolic elements?
            | _ when not isValid -> ()
            | Concrete(l, _), Concrete(r, _), Concrete(p, _) when wereConcrete ->
                let l = convert l typeof<int> :?> int
                let r = convert r typeof<int> :?> int
                let p = convert p typeof<int> :?> int
                let sliceLeft = max (l - position) 0
                left <- sliceLeft + left
                let sliceRight = min (r - position) right
                let sliceSize = sliceRight - sliceLeft
                right <- min (left + sliceSize) right
                position <- max p 0
                if right - left > 0 then
                    assert(left >= 0 && left < termSize)
                    assert(right > 0 && right <= termSize)
                    assert(position >= 0)
                else
                    isValid <- false
            // Case, when concrete cut is not whole term
            | _ when wereConcrete && narrowed() ->
                let cut = (makeNumber left, makeNumber right, makeNumber position)
                cuts <- current :: List.singleton cut
            | _ ->
                cuts <- current :: cuts
        List.foldBack narrow slices ()
        match cuts with
        | _ when not isValid -> Slice term List.empty
        | [] when narrowed() ->
            let cut = (makeNumber left, makeNumber right, makeNumber position)
            Slice term (List.singleton cut)
        | [] ->
            assert(left = 0 && right = termSize && position = 0)
            term
        | _ -> Slice term cuts

    and combine terms t =
        // TODO: filter slices with position >= internalSizeOf t
        let terms = List.filter (not << isEmptySlice) terms |> List.distinct
        let defaultCase() =
            Expression Combine terms t
        let isSolid term typeOfTerm =
            typeOfTerm = t || isRefOrPtr term && (not t.IsValueType || t.IsByRef || isNative t || t.IsPrimitive)
        let simplify p s e pos =
            let typ = typeOf p
            let termSize = lazy (internalSizeOf typ)
            let combineSize = lazy (internalSizeOf t)
            if s = 0 && pos = 0 then
                if e = termSize.Value && isSolid p typ then p
                elif combineSize.Value <= termSize.Value && e = combineSize.Value && isIntegral typ && isIntegral t then
                    primitiveCast p t
                else defaultCase()
            else defaultCase()
        match terms with
        // 'ReportError' case
        | _ when List.isEmpty terms ->
            makeDefaultValue t
        | _ when allSlicesAreConcrete terms ->
            let obj = reinterpretConcretes terms t
            valueTypeToTerm obj t
        | [{term = Slice(p, cuts)}] ->
            match cuts with
            | [({term = Concrete(s, _)}, {term = Concrete(e, _)}, {term = Concrete(pos, _)})] ->
                let s = convert s typeof<int> :?> int
                let e = convert e typeof<int> :?> int
                let pos = convert pos typeof<int> :?> int
                simplify p s e pos
            | _ -> defaultCase()
        | [nonSliceTerm] ->
            simplify nonSliceTerm 0 (sizeOf nonSliceTerm) 0
        | _ -> defaultCase()

    and timeOf (address : heapAddress) =
        match address.term with
        | ConcreteHeapAddress addr -> addr
        | Constant(_, source, _) -> source.Time
        | HeapRef(address, _) -> timeOf address
        | Union gvs -> List.fold (fun m (_, v) -> VectorTime.max m (timeOf v)) VectorTime.zero gvs
        | _ -> internalfail $"timeOf : expected heap address, but got {address}"

    and compareTerms t1 t2 =
        match t1.term, t2.term with
        | Concrete(:? IComparable as x, _), Concrete(:? IComparable as y, _) -> x.CompareTo y
        | Concrete(:? IComparable, _), _ -> -1
        | _, Concrete(:? IComparable, _) -> 1
        | _ -> compare (toString t1) (toString t2)

    and private foldChildren folder state term k =
        match term.term with
        | Constant(_, source, _) ->
            foldSeq folder source.SubTerms state k
        | Expression(_, args, _) ->
            foldSeq folder args state k
        | Struct(fields, _) ->
            foldSeq folder (PersistentDict.values fields) state k
        | Ref address ->
            foldAddress folder state address k
        | Ptr(address, _, indent) ->
            foldPointerBase folder state address (fun state ->
            doFold folder state indent k)
        | GuardedValues(gs, vs) ->
            foldSeq folder gs state (fun state ->
            foldSeq folder vs state k)
        | Slice(t, slices) ->
            let foldSlice state (s, e, pos) k =
                doFold folder state s (fun state ->
                doFold folder state e (fun state ->
                doFold folder state pos k))
            doFold folder state t (fun state ->
            Cps.List.foldlk foldSlice state slices k)
        | _ -> k state

    and doFold folder state term k =
        folder state term k (fun state ->
        foldChildren folder state term k)

    and foldAddress folder state address k =
        match address with
        | PrimitiveStackLocation _
        | StaticField _
        | BoxedLocation _ -> k state
        | ClassField(addr, _) -> doFold folder state addr k
        | ArrayIndex(addr, indices, _) ->
            doFold folder state addr (fun state ->
            foldSeq folder indices state k)
        | StructField(addr, _) -> foldAddress folder state addr k
        | ArrayLength(addr, idx, _)
        | ArrayLowerBound(addr, idx, _) ->
            doFold folder state addr (fun state ->
            doFold folder state idx k)
        | StackBufferIndex(_, idx) -> doFold folder state idx k

    and foldPointerBase folder state pointerBase k =
        match pointerBase with
        | HeapLocation(heapAddress, _) -> doFold folder state heapAddress k
        | StackLocation _
        | StaticLocation _ -> k state

    and private foldSeq folder terms state k =
        Cps.Seq.foldlk (doFold folder) state terms k

    and fold folder state terms =
        foldSeq folder terms state id

    and iter action term =
        doFold action () term id

    and iterSeq action terms =
        Cps.Seq.foldlk (doFold action) () terms id

    and discoverConstants terms =
        let result = HashSet<term>()
        let addConstant _ t _ into =
            match t.term with
            | Constant _ -> result.Add t |> ignore |> into
            | _ -> into ()
        Seq.iter (iter addConstant) terms
        result :> ISet<term>

    and private foldFields isStatic folder acc typ =
        let fields = Reflection.fieldsOf isStatic typ
        let addField heap (fieldId, fieldInfo : Reflection.FieldInfo) =
            folder heap fieldInfo fieldId fieldInfo.FieldType
        FSharp.Collections.Array.fold addField acc fields

    and private makeFields isStatic makeField typ =
        let folder fields fieldInfo field termType =
            let value = makeField fieldInfo field termType
            PersistentDict.add field value fields
        foldFields isStatic folder PersistentDict.empty typ

    and makeStruct isStatic makeField typ =
        let fields = makeFields isStatic makeField typ
        Struct fields typ

    and makeDefaultValue typ =
        match typ with
        | Bool -> False()
        | Numeric t when t.IsEnum -> castConcrete (getEnumDefaultValue t) t
        // NOTE: XML serializer does not support special char symbols, so creating test with char > 32 #XMLChar
        // TODO: change serializer
        | Numeric t when t = typeof<char> ->
            if charsArePretty then makeNumber (char 33)
            else makeNumber '\000'
        | Numeric t -> castConcrete 0 t
        | _ when typ = typeof<IntPtr> -> makeIntPtr (makeNumber 0)
        | _ when typ = typeof<UIntPtr> -> makeUIntPtr (makeNumber 0)
        | ByRef _
        | ArrayType _
        | ClassType _
        | InterfaceType _ -> nullRef typ
        | TypeVariable t when isReferenceTypeParameter t -> nullRef typ
        | TypeVariable t -> __insufficientInformation__ $"Cannot instantiate value of undefined type {t}"
        | StructType _ -> makeStruct false (fun _ _ t -> makeDefaultValue t) typ
        | Pointer typ -> makeNullPtr typ
        | AddressType -> zeroAddress()
        | _ -> __notImplemented__()

    and private valueTypeToTerm obj t =
        assert t.IsValueType
        if isPrimitive t then Concrete obj t
        else
            let makeField (fieldInfo : FieldInfo) _ _ =
                let value = fieldInfo.GetValue(obj)
                valueTypeToTerm value fieldInfo.FieldType
            makeStruct false makeField t
