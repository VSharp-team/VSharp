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

type ICodeLocation =
    abstract Location : obj

type IFunctionIdentifier =
    inherit ICodeLocation
    abstract ReturnType : Type

type StandardFunctionIdentifier(id : StandardFunction) =
    interface IFunctionIdentifier with
        override x.Location = id :> obj
        override x.ReturnType = typeof<double>
    member x.Function = id
    override x.ToString() = id.ToString()

type EmptyIdentifier() =
    interface IFunctionIdentifier with
        override x.Location = null
        override x.ReturnType = typeof<Void>

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
               * term heap                                // Lower bounds
               * (term * arrayInstantiator) list          // Element instantiator with guards
               * term heap                                // Contents
               * term heap                                // Lengths by dimensions
    | Expression of operation * term list * termType
    | Struct of string heap * termType
    | Class of string heap
    | Ref of topLevelAddress * pathSegment list
    | Ptr of topLevelAddress * pathSegment list * termType * term option // contents * type sight * indent
    | Union of (term * term) list

    member x.IndicesToString() =
        let sortKeyFromTerm = (fun t -> t.term) >> function
            | Concrete(value, t) when t = Numeric typedefof<int> -> value :?> int
            | _ -> Int32.MaxValue
        let arrayOfIndicesConcreteContentsToString contents =
            let separator = ", "
            Heap.toString "%s%s" separator (always "") (always toString) sortKeyFromTerm contents
        let arrayOfIndicesSymbolicContentsToString contents =
            let separator = ", "
            Heap.toString "%s: %s" separator toString (always toString) sortKeyFromTerm contents
        let arrayOfIndicesToString = function
            | Array(d, _, _, [(_, instantiator)], contents, _) ->
                let printed =
                    match instantiator with
                    | DefaultInstantiator _ -> ""
                    | LazyInstantiator t -> sprintf "LI(%O): " t
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

        let formatIfNotEmpty format value =
            match value with
            | _ when String.IsNullOrEmpty value -> value
            | _ -> format value

        let formatWithIndent indent = sprintf "\n%s%s" indent

        let extendIndent = (+) "\t"

        let rec toStr parentPriority parentChecked indent term =
            match term with
            | Error e -> sprintf "<ERROR: %O>" (toStringWithIndent indent e)
            | Nop -> "<VOID>"
            | Constant(name, _, _) -> name.v
            | Concrete(_, ClassType(t, _)) when t.IsSubclassOf(typedefof<System.Delegate>) ->
                sprintf "<Lambda Expression %O>" t
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
                    let printedOperand = toStr operation.priority isChecked indent operand.term
                    sprintf (Printf.StringFormat<string->string>(opStr)) printedOperand
                | Operator(operator, isChecked) ->
                    assert (List.length operands >= 2)
                    let printedOperands = operands |> List.map (getTerm >> toStr operation.priority isChecked indent)
                    let sortedOperands = if Operations.isCommutative operator && not isChecked then List.sort printedOperands else printedOperands
                    sortedOperands
                        |> String.concat (Operations.operationToString operator)
                        |> checkExpression isChecked parentChecked operation.priority parentPriority
                | Cast(_, dest, isChecked) ->
                    assert (List.length operands = 1)
                    sprintf "(%O)%s" dest (toStr operation.priority isChecked indent (List.head operands).term) |>
                        checkExpression isChecked parentChecked operation.priority parentPriority
                | Application f -> operands |> List.map (getTerm >> toStr -1 parentChecked indent) |> join ", " |> sprintf "%O(%s)" f
            | Struct(fields, t) ->
                fieldsToString indent fields |> sprintf "%O STRUCT [%s]" t
            | Class fields ->
                fieldsToString indent fields |> sprintf "CLASS [%s]"
            | Array(_, _, _, instantiators, contents, lengths) ->
                let printInstor = function
                    | DefaultInstantiator t -> sprintf "default of %O" t
                    | LazyInstantiator t -> toString t
                let guardedToString (guard, instor) =
                    let guardString = toStringWithParentIndent indent guard
                    let instorString = printInstor instor
                    sprintf "| %s ~> %s" guardString instorString
                let printedInstors = instantiators |> List.map guardedToString |> Seq.sort |> join ("\n" + indent)
                let simplifiedInstors =
                    match instantiators with
                    | [_, i] ->
                        match i with
                        | DefaultInstantiator _ -> ""
                        | LazyInstantiator t -> sprintf "%O: " t
                    | _ -> sprintf "%s: " printedInstors
                let printedLengths = Heap.toString "%O%O" " x " (always "") (toStringWithParentIndent indent |> always) toString lengths
                let printedContents = arrayContentsToString contents indent
                sprintf "%s[|%s... %s ... |]" simplifiedInstors printedContents printedLengths
            | Union(guardedTerms) ->
                let guardedToString (guard, term) =
                    let guardString = toStringWithParentIndent indent guard
                    let termString = toStringWithParentIndent indent term
                    sprintf "| %s ~> %s" guardString termString
                let printed = guardedTerms |> Seq.map guardedToString |> Seq.sort |> join ("\n" + indent)
                formatIfNotEmpty (formatWithIndent indent) printed |> sprintf "UNION[%s]"
            | Ref(topLevel, path) -> printRef topLevel path None
            | Ptr(topLevel, path, typ, shift) ->
                let basePtr = printRef topLevel path (Some typ)
                match shift with
                | Some shift -> sprintf "(IndentedPtr %O[%O])" basePtr shift
                | None -> basePtr
            | _ -> __unreachable__()

        and fieldsToString indent fields =
            let stringResult = Heap.toString "| %O ~> %O" ("\n" + indent) toString (toStringWithParentIndent indent |> always) toString fields
            formatIfNotEmpty (formatWithIndent indent) stringResult

        and printRef topLevel path pointerType =
            let fqlStr = List.map toString path |> cons (toString topLevel) |> join "."
            let makeRef sort =
                match pointerType with
                | Some typ -> sprintf "(%sPtr %s as %O)" sort fqlStr typ
                | None -> sprintf "(%sRef %s)" sort fqlStr
            match topLevel with
            | NullAddress ->
                assert(List.isEmpty path)
                fqlStr
            | TopLevelHeap _ -> makeRef "Heap"
            | TopLevelStack _ -> makeRef "Stack"
            | TopLevelStatics _ -> makeRef "Static"

        and toStringWithIndent indent term = toStr -1 false indent term.term

        and toStringWithParentIndent parentIndent = toStringWithIndent <| extendIndent parentIndent

        and isPrimitiveTerm term =
            match term.term with
            | Struct _
            | Class _
            | Array _
            | Union _ -> false
            | _ -> true

        and arrayContentsToString contents indent =
            let contentsIsLinear = Heap.forall (snd >> isPrimitiveTerm) contents
            let separator = if contentsIsLinear then " " else "\n" + indent
            let heapSeparator = ";" + separator
            let mapper = toStringWithParentIndent indent
            let keyMapper key =
                match key.term with
                | Array _ -> key.term.IndicesToString()
                | _ -> toStringWithParentIndent indent key
            let sortKeyFromIndex index = // TODO: change when index will be term list
                let stringIndex = keyMapper index
                let id = ref 0
                let parseOne (str : string) = if Int32.TryParse(str, id) then !id else Int32.MaxValue
                Array.foldBack (parseOne >> cons) (stringIndex.Split(',')) List.empty
            let stringResult = Heap.toString "%s ~> %s" heapSeparator keyMapper (always mapper) sortKeyFromIndex contents
            let format contents = sprintf "%s%s%s" separator contents separator
            formatIfNotEmpty format stringResult

        toStr -1 false "\t" x

and topLevelAddress =
    | NullAddress
    | TopLevelStack of stackKey
    | TopLevelHeap of term * termType * termType // Address * Base type * Sight type
    | TopLevelStatics of termType
    override x.ToString() =
        match x with
        | TopLevelStack(name, _) -> name
        | TopLevelStatics typ -> toString typ
        | TopLevelHeap(key, _, _) -> toString key
        | NullAddress -> "null"

and pathSegment =
    | BlockField of string * termType
    | ArrayIndex of term * termType
    | ArrayLowerBound of term
    | ArrayLength of term
    override x.ToString() =
        match x with
        | BlockField(field, _) -> field
        | ArrayIndex(idx, _) -> idx.term.IndicesToString() |> sprintf "[%s]"
        | ArrayLowerBound idx
        | ArrayLength idx -> toString idx

and fql = topLevelAddress * pathSegment list // TODO: change fql and create ToString() for it

and
    [<StructuralEquality;NoComparison>]
    arrayInstantiator =
        | DefaultInstantiator of termType
        | LazyInstantiator of termType

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

and 'key heap when 'key : equality = heap<'key, term, fql, termType>
and 'key memoryCell when 'key : equality = memoryCell<'key, fql, termType>

type INonComposableSymbolicConstantSource =
    inherit ISymbolicConstantSource

[<AutoOpen>]
module internal Terms =

    module internal Metadata =
        let empty<'a> = { origins = List.empty; misc = null }
        let combine m1 m2 = { origins = List.append m1.origins m2.origins |> List.distinct; misc = null }
        let combine3 m1 m2 m3 = { origins = List.append3 m1.origins m2.origins m3.origins |> List.distinct; misc = null }
        let addMisc t obj =
            if t.metadata.misc = null then t.metadata.misc <- new HashSet<obj>()
            t.metadata.misc.Add obj |> ignore
        let removeMisc t obj =
            if t.metadata.misc <> null then
                t.metadata.misc.Remove obj |> ignore
                if t.metadata.misc.Count = 0 then t.metadata.misc <- null
        let miscContains t obj = t.metadata.misc <> null && t.metadata.misc.Contains(obj)
        let firstOrigin m =
            match m.origins with
            | [] -> null
            | x::_ -> x.location
        let clone m = { m with misc = if m.misc <> null then new System.Collections.Generic.HashSet<obj>(m.misc) else null}

    let term (term : term) = term.term

// --------------------------------------- Primitives ---------------------------------------

    let Nop<'a> = { term = Nop; metadata = Metadata.empty<'a> }
    let Error metadata term = { term = Error term; metadata = metadata }
    let Concrete metadata obj typ = { term = Concrete(obj, typ); metadata = metadata }
    let Constant metadata name source typ = { term = Constant({v=name}, source, typ); metadata = metadata }
    let Array metadata dimension length lower constant contents lengths = { term = Array(dimension, length, lower, constant, contents, lengths); metadata = metadata }
    let Expression metadata op args typ = { term = Expression(op, args, typ); metadata = metadata }
    let Struct metadata fields typ = { term = Struct(fields, typ); metadata = metadata }
    let Class metadata fields = { term = Class fields; metadata = metadata }
    let Block metadata fields typ = Option.fold (Struct metadata fields |> always) (Class metadata fields) typ
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

    // TODO: get rid of fql reversing (by changing fql) (a lot of bugs are hidden here)
    let reverseFQL fql = mapsnd List.rev fql
    let reverseOptionFQL fql = Option.map reverseFQL fql

    let addToFQL key fql = mapsnd (cons key) fql
    let addToOptionFQL fql key = Option.map (addToFQL key) fql

    let makeTopLevelFQL constr key = Some (constr key, [])

    let makeKey key fql typ = {key = key; FQL = reverseOptionFQL fql; typ = typ} // TODO: makeKey should take only fql
    let makeTopLevelKey constr key typ = {key = key; FQL = makeTopLevelFQL constr key; typ = typ}
    let makePathKey fql constr key typ = {key = key; FQL = constr key |> addToOptionFQL fql |> reverseOptionFQL; typ = typ}
    let getFQLOfKey = function
        | {FQL = Some fql} -> fql
        | {FQL = None} as k -> internalfailf "requested fql from unexpected key %O" k
    let getFQLOfRef = term >> function
        | Ref(tl, path) -> (tl, List.rev path)
        | t -> internalfailf "Expected reference, got %O" t

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

    let isRef = term >> function
        | Ref _ -> true
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

    let private isSymbolicTopLevel = function
        | TopLevelHeap(a, _, _) -> isConcrete a |> not
        | NullAddress -> false
        | _ -> __notImplemented__()

    let isSymbolicRef = term >> function
        | Ref(tl, _) -> isSymbolicTopLevel tl
        | _ -> false

    let (|SymbolicRef|_|) = term >> function
        | Ref(tl, _) when isSymbolicTopLevel tl -> Some()
        | _ -> None

    let (|ConcreteRef|_|) = term >> function
        | Ref(tl, _) when isSymbolicTopLevel tl |> not -> Some()
        | _ -> None

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

    let (|Block|_|) = function
        | Struct(fields, typ) -> Some(Block(fields, Some typ))
        | Class fields -> Some(Block(fields, None))
        | _ -> None

    let fieldsOf = term >> function
        | Block(fields, _) -> fields
        | term -> internalfailf "struct or class expected, %O recieved" term

    let private typeOfTopLevel needBaseType = function
        | TopLevelHeap(_, baseType, _) when needBaseType -> baseType
        | TopLevelHeap(_, _, sightType) -> sightType
        | TopLevelStatics typ -> typ
        | TopLevelStack _ -> Core.Void // TODO: this is temporary hack, support normal typing
        | NullAddress -> Null

    let typeOfPath = List.last >> function
        | BlockField(_, t)
        | ArrayIndex(_, t) -> t
        | ArrayLowerBound _
        | ArrayLength _ -> Types.lengthType

    let private commonTypeOfFQL needBaseType = function
        | tl, [] -> typeOfTopLevel needBaseType tl
        | _, path -> typeOfPath path

    let sightTypeOfFQL = commonTypeOfFQL false
    let baseTypeOfFQL = commonTypeOfFQL true

    let baseTypeOfKey k = k |> getFQLOfKey |> baseTypeOfFQL

    let private typeOfUnion getType gvs =
        let folder types (_, v) =
            if isError v || isVoid v then types else getType v :: types
        let nonEmptyTypes = List.fold folder [] gvs
        match nonEmptyTypes with
        | [] -> termType.Bottom
        | t::ts ->
            let allSame =
                List.forall ((=) t) ts
                || List.forall Types.concreteIsReferenceType nonEmptyTypes // TODO: unhack this hack (goes from TryCatch.MakeOdd)
            if allSame then t
            else internalfailf "evaluating type of unexpected union %O!" gvs

    let commonTypeOf getType term =
        match term.term with
        | Nop -> termType.Void
        | Error _ -> termType.Bottom
        | Union gvs -> typeOfUnion getType gvs
        | _ -> getType term

    let private commonTypeOfRef needBaseType =
        let getTypeOfRef = term >> function
            | Ref(tl, path) -> commonTypeOfFQL needBaseType (tl, path)
            | term -> internalfailf "expected reference, but got %O" term
        commonTypeOf getTypeOfRef

    let sightTypeOfRef = commonTypeOfRef false
    let baseTypeOfRef = commonTypeOfRef true

    let commonTypeOfPtr baseType =
        let getTypeOfPtr = term >> function
            | Ptr(tl, path, _, _) when baseType -> baseTypeOfFQL (tl, path)
            | Ptr(_, _, typ, _) -> typ
            | term -> internalfailf "expected pointer, but got %O" term
        commonTypeOf getTypeOfPtr

    let sightTypeOfPtr = commonTypeOfPtr false
    let baseTypeOfPtr = commonTypeOfPtr true

    let typeOf =
        let getType term =
            match term.term with
            | Concrete(_, t)
            | Constant(_, _, t)
            | Expression(_, _, t)
            | Struct(_, t) -> t
            | Ref _ -> sightTypeOfRef term
            | Ptr _ -> sightTypeOfPtr term |> Pointer
            | _ -> __unreachable__()
        commonTypeOf getType

    let sizeOf = typeOf >> Types.sizeOf
    let bitSizeOf term resultingType = Types.bitSizeOfType (typeOf term) resultingType

    let rec private isPrimitiveTerm term =
        match term.term with
        | Block _
        | Array _ -> false
        | Union gvs -> List.forall (snd >> isPrimitiveTerm) gvs
        | _ -> true

    let isBool t =     isPrimitiveTerm t && typeOf t |> Types.isBool
    let isNumeric t =  isPrimitiveTerm t && typeOf t |> Types.isNumeric

    let rec isStruct term = // TODO: use common function
        match term.term with
        | Struct _ -> true
        | Union gvs -> List.forall (snd >> isStruct) gvs
        | _ -> false

    let rec isReference term =
        match term.term with
        | Union gvs -> List.forall (snd >> isReference) gvs
        | _ -> isRef term

    let CastConcrete isChecked (value : obj) (t : System.Type) metadata =
        let actualType = if box value = null then t else value.GetType()
        try
            if actualType = t then
                Concrete metadata value (fromDotNetType t)
            elif typedefof<IConvertible>.IsAssignableFrom(actualType) then
                let casted =
                    if t.IsPointer then
                        new IntPtr(Convert.ChangeType(value, typedefof<int64>) :?> int64) |> box
                    //TODO: ability to convert negative integers to UInt32 without overflowException
                    elif not isChecked && TypeUtils.isIntegral t then
                        TypeUtils.uncheckedChangeType value t
                    else Convert.ChangeType(value, t)
                Concrete metadata casted (fromDotNetType t)
            elif t.IsAssignableFrom(actualType) then
                Concrete metadata value (fromDotNetType t)
            else raise(new InvalidCastException(sprintf "Cannot cast %s to %s!" t.FullName actualType.FullName))
        with
        | _ ->
            internalfailf "cannot cast %s to %s!" actualType.FullName t.FullName

    let makeTrue metadata =
        Concrete metadata (box true) Bool

    let makeFalse metadata =
        Concrete metadata (box false) Bool

    let True = makeTrue Metadata.empty

    let False = makeFalse Metadata.empty

    let makeBool metadata predicate =
        if predicate then makeTrue metadata else makeFalse metadata

    let makeNullRef metadata =
        Ref metadata NullAddress []

    let makeNullPtr metadata typ =
        Ptr metadata NullAddress [] typ

    let makeIndex metadata i =
        Concrete metadata i Types.indexType

    let makeZeroAddress metadata =
        Concrete metadata [0] Types.pointerType

    let makeNumber metadata n =
        Concrete metadata n (Numeric(n.GetType()))

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

    let negate term metadata =
        assert(isBool term)
        makeUnary OperationType.LogicalNeg term false Bool metadata

    let makePathIndexKey mtd refTarget i fql typ = makePathKey fql refTarget (makeIndex mtd i) typ

    let (|True|_|) term = if isTrue term then Some True else None
    let (|False|_|) term = if isFalse term then Some False else None

    let (|ConcreteT|_|) = term >> function
        | Concrete(name, typ) -> Some(ConcreteT(name, typ))
        | _ -> None

    let (|ErrorT|_|) = term >> function
        | Error e -> Some(ErrorT e)
        | _ -> None

    let (|UnionT|_|) = term >> function
        | Union gvs -> Some(UnionT gvs)
        | _ -> None

    let (|GuardedValues|_|) = function // TODO: this could be ineffective (because of unzip)
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
        | Array(dimension, len, lowerBounds, _, contents, lengths) ->
            doFold folder visited state dimension
            |> fun state -> doFold folder visited state len
            |> foldSeq folder visited (Heap.locations lowerBounds)
            |> foldSeq folder visited (Heap.values lowerBounds)
            |> foldSeq folder visited (Heap.locations contents)
            |> foldSeq folder visited (Heap.values contents)
            |> foldSeq folder visited (Heap.locations lengths)
            |> foldSeq folder visited (Heap.values lengths)
        | Expression(_, args, _) ->
            foldSeq folder visited args state
        | Block(fields, _) ->
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
            foldSeq folder visited gs state |> foldSeq folder visited vs
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
        | BlockField _ -> state
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
