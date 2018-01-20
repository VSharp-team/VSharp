namespace VSharp

open JetBrains.Decompiler.Ast
open global.System
open System.Collections.Generic
open Types.Constructor

type StackKey = string * string // Name and token

type LocationBinding = JetBrains.Decompiler.Ast.INode
type StackHash = int list
type ConcreteHeapAddress = int list
type TermOrigin = { location : LocationBinding; stack : StackHash }
type TermMetadata = { origins : TermOrigin list; mutable misc : HashSet<obj> }

[<StructuralEquality;NoComparison>]
type public Operation =
    | Operator of OperationType * bool
    | Application of FunctionIdentifier
    | Cast of TermType * TermType * bool
    member x.priority =
        match x with
        | Operator (op, _) -> Operations.operationPriority op
        | Application _ -> Operations.maxPriority
        | Cast _ -> Operations.maxPriority - 1

[<StructuralEquality;NoComparison>]
type public TermNode =
    | Nop
    | Error of Term
    | Concrete of obj * TermType
    | Constant of string Transparent * SymbolicConstantSource * TermType
    | Array of Term                                       // Dimension
               * Term                                     // Overal length (product of lengths by dimensions)
               * SymbolicHeap                             // Lower bounds
               * (Term * ArrayInstantiator) list          // Element instantiator with guards
               * SymbolicHeap                             // Contents
               * SymbolicHeap                             // Lengths by dimensions
               * TermType                                 // Type
    | Expression of Operation * Term list * TermType
    | Struct of SymbolicHeap * TermType
    | StackRef of StackKey * (Term * TermType) list * TermType option                       // If last field is (Some T) then this is pointer T*
    | HeapRef of (Term * TermType) NonEmptyList * timestamp Transparent * TermType option   // If last field is (Some T) then this is pointer T*
    | StaticRef of string * (Term * TermType) list * TermType option                        // If last field is (Some T) then this is pointer T*
    | Union of (Term * Term) list

    member x.IndicesToString() =
        let sortKeyFromTerm = (fun t -> t.term) >> function
            | Concrete(value, t) when t = Numeric typedefof<int> -> value :?> int
            | _ -> Int32.MaxValue
        let indicesArrayConcreteContentsToString contents =
            let separator = ", "
            Heap.toString "%s%s" separator (always "") toString (fst >> sortKeyFromTerm) contents
        let indicesArraySymbolicContentsToString contents =
            let separator = ", "
            Heap.toString "%s: %s" separator toString toString (fst >> sortKeyFromTerm) contents
        let indicesArrayToString = function
            | Array(d, _, _, [(_, instantiator)], contents, _, _) ->
                let printed =
                    match instantiator with
                    | DefaultInstantiator _ -> ""
                    | LazyInstantiator(constant, _) -> sprintf "%O: " constant
                match d.term with
                | Concrete _ -> sprintf "%s%s" printed (indicesArrayConcreteContentsToString contents)
                | _ -> sprintf "%s(%s)" printed (indicesArraySymbolicContentsToString contents)
            | _ -> __unreachable__()
        indicesArrayToString x

    override x.ToString() =
        let getTerm (term : Term) = term.term

        let checkExpression curChecked parentChecked priority parentPriority str =
            match curChecked, parentChecked with
            | true, _ when curChecked <> parentChecked -> sprintf "checked(%s)" str
            | false, _ when curChecked <> parentChecked -> sprintf "unchecked(%s)" str
            | _ when priority < parentPriority -> sprintf "(%s)" str
            | _ -> str

        let isCheckNeed curChecked parentChecked = if curChecked <> parentChecked then curChecked else parentChecked

        let formatIfNotEmpty indent value =
            match value with
            | _ when String.IsNullOrEmpty value -> value
            | _ -> sprintf "\n%s%s" indent value

        let extendIndent = (+) "\t"

        let rec toStr parentPriority parentChecked indent term =
            match term with
            | Error e -> sprintf "<ERROR: %O>" (toStringWithIndent indent e)
            | Nop -> "<VOID>"
            | Constant(name, _, _) -> name.v
            | Concrete(lambda, t) when Types.IsFunction t -> sprintf "<Lambda Expression %O>" t
            | Concrete(_, Null) -> "null"
            | Concrete(:? ConcreteHeapAddress as k, _) -> k |> List.map toString |> join "."
            | Concrete(value, _) -> value.ToString()
            | Expression(operation, operands, _) ->
                match operation with
                | Operator(operator, isChecked) when Operations.operationArity operator = 1 ->
                    assert (List.length operands = 1)
                    let operand = List.head operands
                    let opStr = Operations.operationToString operator |> checkExpression isChecked parentChecked operation.priority parentPriority
                    let printedOperand = toStr operation.priority (isCheckNeed isChecked parentChecked) indent operand.term
                    sprintf (Printf.StringFormat<string->string>(opStr)) printedOperand
                | Operator(operator, isChecked) ->
                    assert (List.length operands >= 2)
                    let printedOperands = operands |> List.map (getTerm >> toStr operation.priority (isCheckNeed isChecked parentChecked) indent)
                    let sortedOperands = if Operations.isCommutative operator && not isChecked then List.sort printedOperands else printedOperands
                    sortedOperands
                        |> String.concat (Operations.operationToString operator)
                        |> checkExpression isChecked parentChecked operation.priority parentPriority
                | Cast(orig, dest, isChecked) ->
                    assert (List.length operands = 1)
                    sprintf "(%O)%s" dest (toStr operation.priority (isCheckNeed isChecked parentChecked) indent (List.head operands).term) |>
                        checkExpression isChecked parentChecked operation.priority parentPriority
                | Application f -> operands |> List.map (getTerm >> toStr -1 parentChecked indent) |> join ", " |> sprintf "%O(%s)" f
            | Struct(fields, t) ->
                let fieldsString = Heap.toString "| %O ~> %O" ("\n" + indent) toString (toStringWithParentIndent indent) (fst >> toString) fields
                sprintf "STRUCT %O[%s]" t (formatIfNotEmpty indent fieldsString)
            | Array(_, _, _, instantiators, contents, dimensions, typ) ->
                let tryGetConstant = function
                    | DefaultInstantiator(_, t) -> sprintf "default of %s" (toString t)
                    | LazyInstantiator(_, t) -> toString t
                let guardedTerms = instantiators |> List.map (fun (l, r) -> l, tryGetConstant r)
                let guardedToString (guard, str) =
                    let guardString = toStringWithParentIndent indent guard
                    sprintf "| %s ~> %s" guardString str
                let printed = guardedTerms |> Seq.map guardedToString |> Seq.sort |> join ("\n" + indent)
                let printedOne =
                    match instantiators with
                    | [_, i] ->
                        match i with
                        | DefaultInstantiator _ -> ""
                        | LazyInstantiator(_, t) -> sprintf "%O: " t
                    | _ -> sprintf "%s: " printed
                sprintf "%s[|%s ... %s ... |]" printedOne (arrayContentsToString contents indent) (Heap.toString "%O%O" " x " (always "") toString (fst >> toString) dimensions)
            | Union(guardedTerms) ->
                let guardedToString (guard, term) =
                    let guardString = toStringWithParentIndent indent guard
                    let termString = toStringWithParentIndent indent term
                    sprintf "| %s ~> %s" guardString termString
                let printed = guardedTerms |> Seq.map guardedToString |> Seq.sort |> join ("\n" + indent)
                sprintf "UNION[%s]" (formatIfNotEmpty indent printed)
            | HeapRef(((z, _), []), _, _) when z.term = Concrete([0], Types.pointerType) -> "null"
            | StackRef(_, _, mbtyp)
            | HeapRef(_, _, mbtyp)
            | StaticRef(_, _, mbtyp) ->
                let templateRef name contents =
                    match mbtyp with
                    | Some typ -> sprintf "(%sPtr %s as %O)" name contents typ
                    | None -> sprintf "(%sRef %s)" name contents
                let printref name key path = templateRef name <| sprintf "(%O, %O)" key (List.map fst path)
                match term with
                | StackRef(key, path, _) -> printref "Stack" key path
                | StaticRef(key, path, _) -> printref "Static" key path
                | HeapRef(((z, _), path), _, _) ->
                    let ks =
                        match z.term with
                        | Concrete(:? ConcreteHeapAddress as k, _) -> k |> List.map toString |> join "."
                        | t -> toString t
                    path |> List.map (fst >> toStringWithIndent indent) |> cons ks |> join "." |> templateRef "Heap"

                | _ -> __unreachable__()

        and toStringWithIndent indent term = toStr -1 false indent term.term

        and toStringWithParentIndent parentIndent = toStringWithIndent <| extendIndent parentIndent

        and sortKeyFromTerm = getTerm >> function
            | Concrete(value, t) when t = Numeric typedefof<int> -> value :?> int
            | _ -> Int32.MaxValue

        and arrayContentsToString contents parentIndent =
            let separator = ";\n" + parentIndent
            let mapper = toStringWithParentIndent parentIndent
            let keyMapper key =
                match key.term with
                | Array _ -> key.term.IndicesToString()
                | _ -> toStringWithParentIndent parentIndent key
            let stringResult = Heap.toString "%s: %s" separator keyMapper mapper (fun (k, v) -> sprintf "%s: %s" (keyMapper k) (mapper v)) contents
            match stringResult with
            | _ when String.IsNullOrEmpty stringResult -> stringResult
            | _ -> "\n" + parentIndent + stringResult + separator

        toStr -1 false "\t" x

and
    [<StructuralEquality;NoComparison>]
    ArrayInstantiator =
        | DefaultInstantiator of Term * TermType
        | LazyInstantiator of Term * TermType

and
    [<CustomEquality;NoComparison>]
    Term =
        {term : TermNode; metadata : TermMetadata}
        override x.ToString() = x.term.ToString()
        override x.GetHashCode() = x.term.GetHashCode()
        override x.Equals(o : obj) =
            match o with
            | :? Term as other -> x.term.Equals(other.term)
            | _ -> false

and
    SymbolicConstantSource =
        abstract SubTerms : Term seq

and SymbolicHeap = Heap<Term, Term>

[<AutoOpen>]
module public Terms =

    module Metadata =
        let empty = { origins = List.empty; misc = null }
        let combine m1 m2 = { origins = List.append m1.origins m2.origins |> List.distinct; misc = null }
        let combine3 m1 m2 m3 = { origins = List.append3 m1.origins m2.origins m3.origins |> List.distinct; misc = null }
        let addMisc t obj =
            if t.metadata.misc = null then t.metadata.misc <- new HashSet<obj>()
            t.metadata.misc.Add obj |> ignore
        let miscContains t obj = t.metadata.misc <> null && t.metadata.misc.Contains(obj)
        let isEmpty m = List.isEmpty m.origins
        let firstOrigin m = List.head m.origins
        let clone m = { m with misc = if m.misc <> null then new System.Collections.Generic.HashSet<obj>(m.misc) else null}

    let public term (term : Term) = term.term

    let public Nop = { term = Nop; metadata = Metadata.empty }
    let public Error metadata term = { term = Error term; metadata = metadata }
    let public Concrete metadata obj typ = { term = Concrete(obj, typ); metadata = metadata }
    let public Constant metadata name source typ = { term = Constant({v=name}, source, typ); metadata = metadata }
    let public Array metadata dimension length lower constant contents lengths typ = { term = Array(dimension, length, lower, constant, contents, lengths, typ); metadata = metadata }
    let public Expression metadata op args typ = { term = Expression(op, args, typ); metadata = metadata }
    let public Struct metadata fields typ = { term = Struct(fields, typ); metadata = metadata }
    let public StackView metadata key path view = { term = StackRef(key, path, view); metadata = metadata }
    let public StackPtr metadata key path typ = { term = StackRef(key, path, Some typ); metadata = metadata }
    let public StackRef metadata key path = { term = StackRef(key, path, None); metadata = metadata }
    let public HeapView metadata path time view = { term = HeapRef(path, time, view); metadata = metadata }
    let public HeapPtr metadata path time typ = { term = HeapRef(path, time, Some typ); metadata = metadata }
    let public HeapRef metadata path time = { term = HeapRef(path, time, None); metadata = metadata }
    let public StaticView metadata key path view = { term = StaticRef(key, path, view); metadata = metadata }
    let public StaticPtr metadata key path typ = { term = StaticRef(key, path, Some typ); metadata = metadata }
    let public StaticRef metadata key path = { term = StaticRef(key, path, None); metadata = metadata }
    let public Union metadata gvs = { term = Union gvs; metadata = metadata }

    let public ZeroAddress = TermNode.Concrete([0], Types.pointerType)

    let public MakeZeroAddress mtd = Concrete mtd [0] Types.pointerType


    let public (|StackPtr|_|) = function
        | StackRef(key, path, Some typ) -> Some(StackPtr(key, path, typ))
        | _ -> None

    let public (|StaticPtr|_|) = function
        | StaticRef(key, path, Some typ) -> Some(StaticPtr(key, path, typ))
        | _ -> None

    let public (|HeapPtr|_|) = function
        | HeapRef(path, time, Some typ) -> Some(HeapPtr(path, time, typ))
        | _ -> None

    let internal CastReferenceToPointer mtd targetType = term >> function
        | StackRef(key, path, _) -> StackPtr mtd key path targetType
        | StaticRef(key, path, _) -> StaticPtr mtd key path targetType
        | HeapRef(path, time, _) -> HeapPtr mtd path time targetType
        | t -> internalfailf "Expected reference or pointer, got %O" t

    let internal GetReferenceFromPointer mtd = term >> function
        | StackPtr(key, path, _) -> StackRef mtd key path
        | StaticPtr(key, path, _) -> StaticRef mtd key path
        | HeapPtr(path, time, _) -> HeapRef mtd path time
        | t -> internalfailf "Expected pointer, got %O" t

    let public IsVoid = term >> function
        | Nop -> true
        | _ -> false

    let public IsError = term >> function
        | Error _ -> true
        | _ -> false

    let public IsConcrete = term >> function
        | Concrete _ -> true
        | _ -> false

    let public IsExpression = term >> function
        | Expression _ -> true
        | _ -> false

    let public IsArray = term >> function
        | Array _ -> true
        | _ -> false

    let public IsUnion = term >> function
        | Union _ -> true
        | _ -> false

    let public IsTrue = term >> function
        | Concrete(b, t) when Types.IsBool t && (b :?> bool) -> true
        | _ -> false

    let public IsFalse = term >> function
        | Concrete(b, t) when Types.IsBool t && not (b :?> bool) -> true
        | _ -> false

    let rec public Just predicate term =
        predicate term ||
            match term.term with
            | Union gvs -> List.forall predicate (List.map snd gvs)
            | _ -> false

    let public IsNull = term >> function
        | HeapRef(((z, _), _), _, _) when z.term = ZeroAddress -> true
        | _ -> false

    let public IsStackRef = term >> function
        | StackRef _ -> true
        | _ -> false

    let public IsHeapRef = term >> function
        | HeapRef _ -> true
        | _ -> false

    let rec public IsRef term =
        match term.term with
        | HeapRef _
        | StaticRef _
        | StackRef _ -> true
        | Union gvs -> List.forall (snd >> IsRef) gvs
        | _ -> false

    let public OperationOf = term >> function
        | Expression(op, _, _) -> op
        | term -> internalfailf "expression expected, %O recieved" term

    let public ArgumentsOf = term >> function
        | Expression(_, args, _) -> args
        | term -> internalfailf "expression expected, %O recieved" term

    // Doesn't match stack and static references with empty path!
    let internal (|ReferenceTo|_|) = function
        | StackRef(_, addrs, None)
        | StaticRef(_, addrs, None) -> List.tryLast addrs |> Option.map snd
        | HeapRef(addrs, _, None) -> addrs |> NonEmptyList.toList |> List.last |> snd |> Some
        | _ -> None

    let internal (|PointerTo|_|) = function
        | HeapPtr(_, _, t)
        | StaticPtr(_, _, t)
        | StackPtr(_, _, t) -> Some t
        | _ -> None

    let rec public TypeOf term =
        match term.term with
        | Error _ -> TermType.Bottom
        | Nop -> TermType.Void
        | Concrete(_, t) -> t
        | Constant(_, _, t) -> t
        | Expression(_, _, t) -> t
        | Struct(_, t) -> t
        | PointerTo t -> Pointer t
        | ReferenceTo t -> Reference t
        | StackRef(_, [], _) -> Reference VSharp.Void // TODO: this is temporary hack, support normal typing
        | StaticRef(qtn, [], _) -> Type.GetType(qtn) |> FromDotNetType |> Reference
        | Array(_, _, _, _, _, _, t) -> t
        | Union gvs ->
            let nonEmptyTypes = List.filter (fun t -> not (Types.IsBottom t || Types.IsVoid t)) (List.map (snd >> TypeOf) gvs)
            match nonEmptyTypes with
            | [] -> TermType.Bottom
            | t::ts ->
                let allSame =
                    List.forall ((=) t) ts
                    || List.forall Types.IsReference nonEmptyTypes
                    || List.forall Types.IsPointer nonEmptyTypes
                if allSame then t
                else
                    internalfailf "evaluating type of unexpected union %O!" term
        | _ -> __unreachable__()


    let public SizeOf =                 TypeOf >> Types.SizeOf

    let public IsBool =                 TypeOf >> Types.IsBool
    let public IsInteger =              TypeOf >> Types.IsInteger
    let public IsReal =                 TypeOf >> Types.IsReal
    let public IsNumeric =              TypeOf >> Types.IsNumeric
    let public IsString =               TypeOf >> Types.IsString
    let public IsFunction =             TypeOf >> Types.IsFunction
    let public IsPrimitive =            TypeOf >> Types.IsPrimitive
    let public DomainOf =               TypeOf >> Types.DomainOf
    let public RangeOf =                TypeOf >> Types.RangeOf

    let public CastConcrete value (t : System.Type) metadata =
        let actualType = if box value = null then t else value.GetType()
        try
            if actualType = t then
                Concrete metadata value (FromDotNetType t)
            elif typedefof<IConvertible>.IsAssignableFrom(actualType) then
                let casted =
                    if t.IsPointer
                    then new IntPtr(Convert.ChangeType(value, typedefof<int64>) :?> int64) |> box
                    else Convert.ChangeType(value, t)
                Concrete metadata casted (FromDotNetType t)
            elif t.IsAssignableFrom(actualType) then
                Concrete metadata value (FromDotNetType t)
            else raise(new InvalidCastException(sprintf "Cannot cast %s to %s!" t.FullName actualType.FullName))
        with
        | _ ->
            internalfailf "cannot cast %s to %s!" t.FullName actualType.FullName

    let public MakeTrue metadata =
        Concrete metadata (box true) Bool

    let public MakeFalse metadata =
        Concrete metadata (box false) Bool

    let public True = MakeTrue Metadata.empty

    let public False = MakeFalse Metadata.empty

    let public MakeBool predicate metadata =
        if predicate then MakeTrue metadata else MakeFalse metadata

    let public MakeNullRef typ metadata =
        HeapRef metadata (((MakeZeroAddress metadata), typ), []) {v = Timestamp.zero}

    let public MakeNullPtr typ metadata =
        HeapPtr metadata (((MakeZeroAddress metadata), typ), []) {v = Timestamp.zero} typ

    let public MakeNumber n metadata =
        Concrete metadata n (Numeric(n.GetType()))

    let public MakeConcreteString (s : string) metadata =
        Concrete metadata s VSharp.String

    let public MakeBinary operation x y isChecked t metadata =
        assert(Operations.isBinary operation)
        Expression metadata (Operator(operation, isChecked)) [x; y] t

    let public MakeNAry operation x isChecked t metadata =
        match x with
        | [] -> raise(new ArgumentException("List of args should be not empty"))
        | [x] -> x
        | _ -> Expression metadata (Operator(operation, isChecked)) x t

    let public MakeUnary operation x isChecked t metadata =
        assert(Operations.isUnary operation)
        Expression metadata (Operator(operation, isChecked)) [x] t

    let public MakeCast srcTyp dstTyp expr isChecked metadata =
        if srcTyp = dstTyp then expr
        else Expression metadata (Cast(srcTyp, dstTyp, isChecked)) [expr] dstTyp

    let public MakeStringKey typeName =
        MakeConcreteString typeName Metadata.empty

    let public Negate term metadata =
        assert(IsBool term)
        MakeUnary OperationType.LogicalNeg term false Bool metadata


    let (|True|_|) term = if IsTrue term then Some True else None
    let (|False|_|) term = if IsFalse term then Some False else None
    let (|Null|_|) term = if IsNull term then Some Null else None

    let (|ConcreteT|_|) = term >> function
        | Concrete(name, typ) -> Some(ConcreteT(name, typ))
        | _ -> None

    let (|ErrorT|_|) = term >> function
        | Error e -> Some(ErrorT e)
        | _ -> None

    let (|UnionT|_|) = term >> function
        | Union gvs -> Some(UnionT gvs)
        | _ -> None

    let (|GuardedValues|_|) = function
        | Union gvs -> Some(GuardedValues(List.unzip gvs))
        | _ -> None

    let (|UnaryMinus|_|) = function
        | Expression(Operator(OperationType.UnaryMinus, isChecked), [x], t) -> Some(UnaryMinus(x, isChecked, t))
        | _ -> None

    let (|UnaryMinusT|_|) = term >> (|UnaryMinus|_|)

    let (|Add|_|) = term >> function
        | Expression(Operator(OperationType.Add, isChecked), [x;y], t) -> Some(Add(x, y, isChecked, t))
        | _ -> None

    let (|Sub|_|) = term >> function
        | Expression(Operator(OperationType.Subtract, isChecked), [x;y], t) -> Some(Sub(x, y, isChecked, t))
        | _ -> None

    let (|Mul|_|) = term >> function
        | Expression(Operator(OperationType.Multiply, isChecked), [x;y], t) -> Some(Mul(x, y, isChecked, t))
        | _ -> None

    let (|Div|_|) = term >> function
        | Expression(Operator(OperationType.Divide, isChecked), [x;y], t) -> Some(Div(x, y, isChecked, t))
        | _ -> None

    let (|Rem|_|) = term >> function
        | Expression(Operator(OperationType.Remainder, isChecked), [x;y], t) -> Some(Rem(x, y, isChecked, t))
        | _ -> None

    let (|Negation|_|) = function
        | Expression(Operator(OperationType.LogicalNeg, _), [x], t) -> Some(Negation(x, t))
        | _ -> None

    let (|NegationT|_|) = term >> (|Negation|_|)

    let (|Conjunction|_|) = term >> function
        | Expression(Operator(OperationType.LogicalAnd, _), [x;y], t) -> Some(Conjunction(x, y, t))
        | _ -> None

    let (|ConjunctionList|_|) = function
        | Expression(Operator(OperationType.LogicalAnd, _), xs, t) -> Some(ConjunctionList(xs, t))
        | _ -> None

    let (|Disjunction|_|) = term >> function
        | Expression(Operator(OperationType.LogicalOr, _), [x;y], t) -> Some(Disjunction(x, y, t))
        | _ -> None

    let (|DisjunctionList|_|) = function
        | Expression(Operator(OperationType.LogicalOr, _), xs, t) -> Some(DisjunctionList(xs, t))
        | _ -> None

    let (|Xor|_|) = term >> function
        | Expression(Operator(OperationType.LogicalXor, _), [x;y], t) -> Some(Xor(x, y, t))
        | _ -> None

    let (|ShiftLeft|_|) = term >> function
        | Expression(Operator(OperationType.ShiftLeft, isChecked), [x;y], t) -> Some(ShiftLeft(x, y, isChecked, t))
        | _ -> None

    let (|ShiftRight|_|) = term >> function
        | Expression(Operator(OperationType.ShiftRight, isChecked), [x;y], t) -> Some(ShiftRight(x, y, isChecked, t))
        | _ -> None

    // TODO: can we already get rid of visited?
    let rec private foldChildren folder (visited : HashSet<Term>) state term =
        match term.term with
        | Constant(name, source, t) when visited.Add(term) ->
            foldSeq folder visited source.SubTerms state
        | Array(dimension, len, lowerBounds, constant, contents, lengths, _) ->
            constant
            |> Seq.fold (fun s (_, i) ->
                match i with
                | DefaultInstantiator _ -> s
                | LazyInstantiator(t, _) -> doFold folder visited s t) state
            |> fun acc -> doFold folder visited state dimension
            |> fun acc -> doFold folder visited state len
            |> foldSeq folder visited (Heap.locations lowerBounds)
            |> foldSeq folder visited (Heap.values lowerBounds)
            |> foldSeq folder visited (Heap.locations contents)
            |> foldSeq folder visited (Heap.values contents)
            |> foldSeq folder visited (Heap.locations lengths)
            |> foldSeq folder visited (Heap.values lengths)
        | Expression(_, args, _) ->
            foldSeq folder visited args state
        | Struct(fields, _) ->
            foldSeq folder visited (Heap.values fields) state
        | HeapRef(path, _, _) ->
            foldSeq folder visited (NonEmptyList.toList path |> Seq.map fst) state
        | StackRef(_, path, _)
        | StaticRef(_, path, _) ->
            foldSeq folder visited (path |> Seq.map fst) state
        | GuardedValues(gs, vs) ->
            foldSeq folder  visited gs state |> foldSeq folder visited vs
        | Error e ->
            doFold folder visited state e
        | _ -> state

    and doFold folder (visited : HashSet<Term>) state term =
        let state = foldChildren folder visited state term
        folder state term

    and private foldSeq folder visited terms state =
        Seq.fold (doFold folder visited) state terms

    let public fold folder state terms =
        foldSeq folder (new HashSet<Term>()) state terms

    let public iter action term =
        doFold (fun () -> action) (new HashSet<Term>()) () term

    let public filterMapConstants mapper terms =
        let folder state term = mapper state term |> optCons state
        fold folder [] terms
