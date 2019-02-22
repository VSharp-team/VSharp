namespace VSharp.Core

open VSharp
open global.System
open System.Collections.Generic
open Types.Constructor

type stackKey = string * string // Name and token

type locationBinding = obj
type stackHash = int list
type concreteHeapAddress = int list
type termOrigin = { location : locationBinding; stack : stackHash }
type termMetadata = { origins : termOrigin list; mutable misc : HashSet<obj> }

type IFunctionIdentifier =
    abstract ReturnType : termType
type StandardFunctionIdentifier(id : StandardFunction) =
    interface IFunctionIdentifier with
        override x.ReturnType = Numeric typeof<double>
    member x.Function = id
    override x.ToString() = id.ToString()

type EmptyIdentifier() =
    interface IFunctionIdentifier with
        override x.ReturnType = Core.Void

[<StructuralEquality;NoComparison>]
type operation =
    | Operator of OperationType * bool
    | Application of IFunctionIdentifier
    | Cast of termType * termType * bool
    member x.priority =
        match x with
        | Operator (op, _) -> Operations.operationPriority op
        | Application _ -> Operations.maxPriority
        | Cast _ -> Operations.maxPriority - 1

[<StructuralEquality;NoComparison>]
type termNode =
    | Nop
    | Error of term
    | Concrete of obj * termType
    | Constant of string transparent * ISymbolicConstantSource * termType
    | Array of term                                       // Dimension
               * term                                     // Overal length (product of lengths by dimensions)
               * symbolicHeap                             // Lower bounds
               * (term * arrayInstantiator) list          // Element instantiator with guards
               * symbolicHeap                             // Contents
               * symbolicHeap                             // Lengths by dimensions
               * termType                                 // Type
    | Expression of operation * term list * termType
    | Struct of heap<string, term, fql> * termType
    | Ref of topLevelAddress * pathSegment list
    | Ptr of topLevelAddress * pathSegment list * termType * term option // contents * type sight * indent
    | Union of (term * term) list

    member x.IndicesToString() =
        let sortKeyFromTerm = (fun t -> t.term) >> function
            | Concrete(value, t) when t = Numeric typedefof<int> -> value :?> int
            | _ -> Int32.MaxValue
        let arrayOfIndicesConcreteContentsToString contents =
            let separator = ", "
            Heap.toString "%s%s" separator (always "") toString (fst >> sortKeyFromTerm) contents
        let arrayOfIndicesSymbolicContentsToString contents =
            let separator = ", "
            Heap.toString "%s: %s" separator toString toString (fst >> sortKeyFromTerm) contents
        let arrayOfIndicesToString = function
            | Array(d, _, _, [(_, instantiator)], contents, _, _) ->
                let printed =
                    match instantiator with
                    | DefaultInstantiator _ -> ""
                    | LazyInstantiator(constant, _) -> sprintf "%O: " constant
                match d.term with
                | Concrete _ -> sprintf "%s%s" printed (arrayOfIndicesConcreteContentsToString contents)
                | _ -> sprintf "%s(%s)" printed (arrayOfIndicesSymbolicContentsToString contents)
            | x -> toString x
        arrayOfIndicesToString x

    override x.ToString() =
        let getTerm (term : term) = term.term

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
            | Concrete(_, t) when Types.isFunction t -> sprintf "<Lambda Expression %O>" t
            | Concrete(_, Null) -> "null"
            | Concrete(c, Numeric t) when t = typedefof<char> && c :?> char = '\000' -> "'\\000'"
            | Concrete(c, Numeric t) when t = typedefof<char> -> sprintf "'%O'" c
            | Concrete(:? concreteHeapAddress as k, _) -> k |> List.map toString |> join "."
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
                | Cast(_, dest, isChecked) ->
                    assert (List.length operands = 1)
                    sprintf "(%O)%s" dest (toStr operation.priority (isCheckNeed isChecked parentChecked) indent (List.head operands).term) |>
                        checkExpression isChecked parentChecked operation.priority parentPriority
                | Application f -> operands |> List.map (getTerm >> toStr -1 parentChecked indent) |> join ", " |> sprintf "%O(%s)" f
            | Struct(fields, t) ->
                let fieldsString = Heap.toString "| %O ~> %O" ("\n" + indent) toString (toStringWithParentIndent indent) (fst >> toString) fields
                sprintf "STRUCT %O[%s]" t (formatIfNotEmpty indent fieldsString)
            | Array(_, _, _, instantiators, contents, dimensions, _) ->
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
            | Ref(topLevel, path) -> printRef topLevel path indent None
            | Ptr(topLevel, path, typ, shift) ->
                let basePtr = printRef topLevel path indent (Some typ)
                match shift with
                | Some shift -> sprintf "(IndentedPtr %O[%O])" basePtr shift
                | None -> basePtr
            | _ -> __unreachable__()

        and pathToString indent path =
            path 
            |> List.map (function
                | StructField(field, _) -> field
                | ArrayIndex(idx, _) -> sprintf "[%s]" (toStringWithIndent indent idx)
                | ArrayLowerBound idx -> sprintf "LowerBoundDimension_%O" idx
                | ArrayLength idx -> sprintf "LengthDimension_%O" idx)

        and printRef topLevel path indent mbtyp =
            let templateRef name contents =
                match mbtyp with
                | Some typ -> sprintf "(%sPtr %s as %O)" name contents typ
                | None -> sprintf "(%sRef %s)" name contents
            let printref name key path = templateRef name <| sprintf "(%O, %s)" key (pathToString indent path |> join ".")
            match topLevel with
            | NullAddress -> "null"
            | TopLevelStack key -> printref "Stack" key path
            | TopLevelStatics typ -> printref "Static" typ path
            | TopLevelHeap(addr, _, _) ->
                let addrStr =
                    match addr.term with
                    | Concrete(:? concreteHeapAddress as k, _) -> k |> List.map toString |> join "."
                    | t -> toString t
                path |> pathToString indent |> cons addrStr |> join "." |> templateRef "Heap"

        and toStringWithIndent indent term = toStr -1 false indent term.term

        and toStringWithParentIndent parentIndent = toStringWithIndent <| extendIndent parentIndent

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

and topLevelAddress =
    | NullAddress
    | TopLevelStack of stackKey
    | TopLevelHeap of term * termType * termType // Address * Base type * Sight type
    | TopLevelStatics of termType

and pathSegment =
    | StructField of string * termType
    | ArrayIndex of term * termType
    | ArrayLowerBound of term
    | ArrayLength of term

and fql = topLevelAddress * pathSegment list

and
    [<StructuralEquality;NoComparison>]
    arrayInstantiator =
        | DefaultInstantiator of term * termType
        | LazyInstantiator of term * termType

and
    [<CustomEquality;NoComparison>]
    term =
        {term : termNode; metadata : termMetadata}
        override x.ToString() = x.term.ToString()
        override x.GetHashCode() = x.term.GetHashCode()
        override x.Equals(o : obj) =
            match o with
            | :? term as other -> x.term.Equals(other.term)
            | _ -> false

and
    ISymbolicConstantSource =
        abstract SubTerms : term seq

and symbolicHeap = heap<term, term, fql>

type INonComposableSymbolicConstantSource =
    inherit ISymbolicConstantSource

[<AutoOpen>]
module internal Terms =

    module internal Metadata =
        let empty = { origins = List.empty; misc = null }
        let combine m1 m2 = { origins = List.append m1.origins m2.origins |> List.distinct; misc = null }
        let combine3 m1 m2 m3 = { origins = List.append3 m1.origins m2.origins m3.origins |> List.distinct; misc = null }
        let addMisc t obj =
            if t.metadata.misc = null then t.metadata.misc <- new HashSet<obj>()
            t.metadata.misc.Add obj |> ignore
        let miscContains t obj = t.metadata.misc <> null && t.metadata.misc.Contains(obj)
        let firstOrigin m =
            match m.origins with
            | [] -> null
            | x::_ -> x.location
        let clone m = { m with misc = if m.misc <> null then new System.Collections.Generic.HashSet<obj>(m.misc) else null}

    let term (term : term) = term.term

    let Nop = { term = Nop; metadata = Metadata.empty }
    let Error metadata term = { term = Error term; metadata = metadata }
    let Concrete metadata obj typ = { term = Concrete(obj, typ); metadata = metadata }
    let Constant metadata name source typ = { term = Constant({v=name}, source, typ); metadata = metadata }
    let Array metadata dimension length lower constant contents lengths typ = { term = Array(dimension, length, lower, constant, contents, lengths, typ); metadata = metadata }
    let Expression metadata op args typ = { term = Expression(op, args, typ); metadata = metadata }
    let Struct metadata fields typ = { term = Struct(fields, typ); metadata = metadata }
    let StackRef metadata key path = { term = Ref(TopLevelStack key, path); metadata = metadata }
    let HeapRef metadata addr baseType sightType path = { term = Ref(TopLevelHeap(addr, baseType, sightType), path); metadata = metadata }
    let StaticRef metadata typ path = { term = Ref(TopLevelStatics typ, path); metadata = metadata }
    let StackPtr metadata key path typ = { term = Ptr(TopLevelStack key, path, typ, None); metadata = metadata }
    let HeapPtr metadata addr baseType sightType path ptrTyp = { term = Ptr(TopLevelHeap(addr, baseType, sightType), path, ptrTyp, None); metadata = metadata }
    let AnyPtr metadata topLevel path typ shift = { term = Ptr(topLevel, path, typ, shift); metadata = metadata }
    let IndentedPtr metadata topLevel path typ shift = { term = Ptr(topLevel, path, typ, Some shift); metadata = metadata }
    let Ref metadata topLevel path = { term = Ref(topLevel, path); metadata = metadata }
    let Ptr metadata topLevel path typ = { term = Ptr(topLevel, path, typ, None); metadata = metadata }
    let Union metadata gvs = { term = Union gvs; metadata = metadata }

    let reverseFQL fql = Option.map (mapsnd List.rev) fql
    let addToFQL key fql = mapsnd (cons key) fql
    let addToOptionFQL fql key = Option.map (addToFQL key) fql
    let makeTopLevelFQL constr key = Some (constr key, [])

    let makeKey key fql = {key = key; FQL = reverseFQL fql}
    let makeTopLevelKey constr key = {key = key; FQL = makeTopLevelFQL constr key}
    let makePathKey fql constr key = {key = key; FQL = constr key |> addToOptionFQL fql |> reverseFQL}
    let getFQLOfKey = function
        | {FQL = Some fql} -> fql
        | {FQL = None} as k -> internalfail "requested fql from unexpected key %O" k

    let makeFQLRef metadata (tl, path) = Ref metadata tl path

    let castReferenceToPointer mtd targetType = term >> function
        | Ref(topLevel, path)
        | Ptr(topLevel, path, _, None) -> Ptr mtd topLevel path targetType
        | Ptr(topLevel, path, _, Some indent) -> IndentedPtr mtd topLevel path targetType indent
        | t -> internalfailf "Expected reference or pointer, got %O" t

    let getReferenceFromPointer mtd = term >> function
        | Ptr(topLevel, path, _, _) -> Ref mtd topLevel path
        | t -> internalfailf "Expected pointer, got %O" t

    let isVoid = term >> function
        | Nop -> true
        | _ -> false

    let isError = term >> function
        | Error _ -> true
        | _ -> false

    let isConcrete = term >> function
        | Concrete _ -> true
        | _ -> false

    let isExpression = term >> function
        | Expression _ -> true
        | _ -> false

    let isArray = term >> function
        | Array _ -> true
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

    let isConcreteNull = term >> function
        | Ref(NullAddress, _) -> true
        | _ -> false

    let rec isRefOrPtr term =
        match term.term with
        | Ref _
        | Ptr _ -> true
        | Union gvs -> List.forall (snd >> isRefOrPtr) gvs
        | _ -> false

    let isArrayIndex = function
        | ArrayIndex _ -> true
        | _ -> false

    let isArrayLengthSeg = function
        | ArrayLength _ -> true
        | _ -> false

    let isArrayLowerBoundSeg = function
        | ArrayLowerBound _ -> true
        | _ -> false

    let operationOf = term >> function
        | Expression(op, _, _) -> op
        | term -> internalfailf "expression expected, %O recieved" term

    let argumentsOf = term >> function
        | Expression(_, args, _) -> args
        | term -> internalfailf "expression expected, %O recieved" term

    let private typeOfTopLevel = function
        | NullAddress -> Null
        | TopLevelHeap(_, _, sightTyp) -> sightTyp
        | TopLevelStatics typ -> typ
        | TopLevelStack _ -> Core.Void // TODO: this is temporary hack, support normal typing

    let typeOfPathSegment = function
        | StructField(_, t)
        | ArrayIndex(_, t) -> t
        | ArrayLowerBound _
        | ArrayLength _ -> Types.indexType

    let rec typeOf term =
        match term.term with
        | Error _ -> termType.Bottom
        | Nop -> termType.Void
        | Concrete(_, t)
        | Constant(_, _, t)
        | Expression(_, _, t)
        | Struct(_, t)
        | Array(_, _, _, _, _, _, t) -> t
        | Ref(tl, []) -> typeOfTopLevel tl |> Reference
        | Ref(_, path) -> List.last path |> typeOfPathSegment
        | Ptr(_, _, typ, _) -> Pointer typ
        | Union gvs ->
            let nonEmptyTypes = List.filter (fun t ->
                not (Types.isBottom t || Types.isVoid t)) (List.map (snd >> typeOf) gvs)
            match nonEmptyTypes with
            | [] -> termType.Bottom
            | t::ts ->
                let allSame =
                    List.forall ((=) t) ts
                    || List.forall Types.isReference nonEmptyTypes
                    || List.forall Types.isPointer nonEmptyTypes
                if allSame then t
                else
                    internalfailf "evaluating type of unexpected union %O!" term


    let sizeOf = typeOf >> Types.sizeOf
    let bitSizeOf term resultingType = Types.bitSizeOfType (typeOf term) resultingType

    let isBool =                 typeOf >> Types.isBool
    let isInteger =              typeOf >> Types.isInteger
    let isReal =                 typeOf >> Types.isReal
    let isNumeric =              typeOf >> Types.isNumeric
    let isString =               typeOf >> Types.isString
    let isFunction =             typeOf >> Types.isFunction
    let isPrimitive =            typeOf >> Types.isPrimitive
    let domainOf =               typeOf >> Types.domainOf
    let rangeOf =                typeOf >> Types.rangeOf

    let CastConcrete value (t : System.Type) metadata =
        let actualType = if box value = null then t else value.GetType()
        try
            if actualType = t then
                Concrete metadata value (fromDotNetType t)
            elif typedefof<IConvertible>.IsAssignableFrom(actualType) then
                let casted =
                    if t.IsPointer
                    then new IntPtr(Convert.ChangeType(value, typedefof<int64>) :?> int64) |> box
                    else Convert.ChangeType(value, t)
                Concrete metadata casted (fromDotNetType t)
            elif t.IsAssignableFrom(actualType) then
                Concrete metadata value (fromDotNetType t)
            else raise(new InvalidCastException(sprintf "Cannot cast %s to %s!" t.FullName actualType.FullName))
        with
        | _ ->
            internalfailf "cannot cast %s to %s!" t.FullName actualType.FullName

    let makeTrue metadata =
        Concrete metadata (box true) Bool

    let makeFalse metadata =
        Concrete metadata (box false) Bool

    let True = makeTrue Metadata.empty

    let False = makeFalse Metadata.empty

    let makeBool predicate metadata =
        if predicate then makeTrue metadata else makeFalse metadata

    let makeNullRef metadata =
        Ref metadata NullAddress []

    let makeNullPtr metadata typ =
        Ptr metadata NullAddress [] typ

    let makeZero metadata =
        Concrete metadata [0] (Numeric typedefof<int>)

    let makeZeroIndex metadata =
        Concrete metadata [0] Types.indexType

    let makeZeroAddress metadata =
        Concrete metadata [0] Types.pointerType

    let makeNumber n metadata =
        Concrete metadata n (Numeric(n.GetType()))

    let makeConcreteString (s : string) metadata =
        Concrete metadata s Types.String

    let makeBinary operation x y isChecked t metadata =
        assert(Operations.isBinary operation)
        Expression metadata (Operator(operation, isChecked)) [x; y] t

    let makeNAry operation x isChecked t metadata =
        match x with
        | [] -> raise(new ArgumentException("List of args should be not empty"))
        | [x] -> x
        | _ -> Expression metadata (Operator(operation, isChecked)) x t

    let makeUnary operation x isChecked t metadata =
        assert(Operations.isUnary operation)
        Expression metadata (Operator(operation, isChecked)) [x] t

    let makeCast srcTyp dstTyp expr isChecked metadata =
        if srcTyp = dstTyp then expr
        else Expression metadata (Cast(srcTyp, dstTyp, isChecked)) [expr] dstTyp

    let makeStringKey typeName =
        makeConcreteString typeName Metadata.empty

    let negate term metadata =
        assert(isBool term)
        makeUnary OperationType.LogicalNeg term false Bool metadata

    let makePathNumericKey fql refTarget i mtd = makePathKey fql refTarget <| makeNumber i mtd

    let (|True|_|) term = if isTrue term then Some True else None
    let (|False|_|) term = if isFalse term then Some False else None

    let (|Null|_|) = function
        | Ref(NullAddress, []) -> Some(Null)
        | _ -> None

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
        | Expression(Operator(OperationType.LogicalNeg, _), [x], _) -> Some(Negation x)
        | _ -> None

    let (|NegationT|_|) = term >> (|Negation|_|)

    let (|Conjunction|_|) = function
        | Expression(Operator(OperationType.LogicalAnd, _), xs, _) -> Some(Conjunction xs)
        | _ -> None

    let (|Disjunction|_|) = function
        | Expression(Operator(OperationType.LogicalOr, _), xs, _) -> Some(Disjunction xs)
        | _ -> None

    let (|Xor|_|) = term >> function
        | Expression(Operator(OperationType.LogicalXor, _), [x;y], _) -> Some(Xor(x, y))
        | _ -> None

    let (|ShiftLeft|_|) = term >> function
        | Expression(Operator(OperationType.ShiftLeft, isChecked), [x;y], t) -> Some(ShiftLeft(x, y, isChecked, t))
        | _ -> None

    let (|ShiftRight|_|) = term >> function
        | Expression(Operator(OperationType.ShiftRight, isChecked), [x;y], t) -> Some(ShiftRight(x, y, isChecked, t))
        | _ -> None

    // TODO: can we already get rid of visited?
    let rec private foldChildren folder (visited : HashSet<term>) state term =
        match term.term with
        | Constant(_, source, _) when visited.Add(term) ->
            foldSeq folder visited source.SubTerms state
        | Array(dimension, len, lowerBounds, constant, contents, lengths, _) ->
            constant
            |> Seq.fold (fun s (_, i) ->
                match i with
                | DefaultInstantiator _ -> s
                | LazyInstantiator(t, _) -> doFold folder visited s t) state
            |> fun state -> doFold folder visited state dimension
            |> fun state -> doFold folder visited state len
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
        | Ref(topLevel, path) ->
            let state = foldTopLevel folder visited state topLevel
            foldPath folder visited state path
        | Ptr(topLevel, path, _, indent) ->
            let state = foldTopLevel folder visited state topLevel
            let state = foldPath folder visited state path
            match indent with
            | None -> state
            | Some indent -> doFold folder visited state indent
        | GuardedValues(gs, vs) ->
            foldSeq folder  visited gs state |> foldSeq folder visited vs
        | Error e ->
            doFold folder visited state e
        | _ -> state

    and doFold folder (visited : HashSet<term>) state term =
        let state = foldChildren folder visited state term
        folder state term

    and foldTopLevel folder visited state = function
        | TopLevelHeap(addr, _, _) -> doFold folder visited state addr
        | NullAddress
        | TopLevelStack _
        | TopLevelStatics _ -> state

    and foldPathSegment folder visited state = function
        | StructField _ -> state
        | ArrayIndex(idx, _)
        | ArrayLowerBound idx
        | ArrayLength idx -> doFold folder visited state idx

    and private foldPath folder visited =
        Seq.fold (foldPathSegment folder visited)

    and private foldSeq folder visited terms state =
        Seq.fold (doFold folder visited) state terms

    let fold folder state terms =
        foldSeq folder (new HashSet<term>()) terms state

    let iter action term =
        doFold (fun () -> action) (new HashSet<term>()) () term

    let discoverConstants terms =
        let result = new HashSet<term>()
        let addConstant = function
            | {term = Constant _} as constant -> result.Add constant |> ignore
            | _ -> ()
        Seq.iter (iter addConstant) terms
        result :> ISet<term>

    let unwrapReferenceType = function
        | Reference t -> t
        | t -> t
