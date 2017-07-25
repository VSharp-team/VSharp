namespace VSharp

open JetBrains.Decompiler.Ast
open global.System
open System.Collections.Generic

[<StructuralEquality;NoComparison>]
type FunctionIdentifier =
    | MetadataMethodIdentifier of JetBrains.Metadata.Reader.API.IMetadataMethod
    | DelegateIdentifier of JetBrains.Decompiler.Ast.INode
    | StandardFunctionIdentifier of Operations.StandardFunction
    override this.ToString() =
        match this with
        | MetadataMethodIdentifier mm -> mm.Name
        | DelegateIdentifier _ -> "<delegate>"
        | StandardFunctionIdentifier sf -> sf.ToString()

[<StructuralEquality;NoComparison>]
type public Operation =
    | Operator of OperationType * bool
    | Application of FunctionIdentifier
    | Cast of TermType * TermType * bool

    member this.priority =
        match this with
        | Operator (op, _) -> Operations.operationPriority op
        | Application _ -> Operations.maxPriority
        | Cast _ -> Operations.maxPriority - 1

[<StructuralEquality;NoComparison>]
type public Term =
    | Error of Term
    | Nop
    | Concrete of Object * TermType
    | Constant of string * SymbolicConstantSource * TermType
    | Array of Term array               // Lower bounds
                * Term option           // Symbolic constant (or None if array has default contents)
                * (Term * Term) list    // Contents: index ~> mutation timestamp * value
                * Term array            // Lengths of dimensions
                * TermType              // Type
    | Expression of (Operation * Term list * TermType)
    | Struct of Map<string, Term> * TermType
    | StackRef of (string * string) * string list * TermType
    | HeapRef of Term * string list * TermType
    | Union of (Term * Term) list

    override this.ToString() =
        let checkExpression curChecked parentChecked priority parentPriority str = 
            match curChecked, parentChecked with 
            | true, _ when curChecked <> parentChecked -> sprintf "checked(%s)" str
            | false, _ when curChecked <> parentChecked -> sprintf "unchecked(%s)" str
            | _ when priority < parentPriority -> sprintf "(%s)" str
            | _ -> str

        let isCheckNeed curChecked parentChecked = if curChecked <> parentChecked then curChecked else parentChecked

        let arrayContentsToString contents innerSeparator outerSeparator =
            contents
                |> Seq.map (fun kvp -> (toString (fst kvp), toString (snd kvp)))
                |> Seq.sortBy fst
                |> Seq.map (fun (k, v) -> k + innerSeparator + v)
                |> join outerSeparator

        let rec toStr parentPriority parentChecked indent term =
            match term with
            | Error e -> String.Format("<ERROR: {0}>", (toString e))
            | Nop -> "<VOID>"
            | Constant(name, _, _) -> name
            | Concrete(lambda, t) when Types.IsFunction t -> String.Format("<Lambda Expression {0}>", t)
            | Concrete(null, _) -> "null"
            | Concrete(value, _) -> value.ToString()
            | Expression(operation, operands, _) ->
                match operation with
                | Operator(operator, isChecked) when Operations.operationArity operator = 1 ->
                    assert (List.length operands = 1)
                    let opStr = Operations.operationToString operator |> checkExpression isChecked parentChecked operation.priority parentPriority in
                    let printedOperands = operands |> List.map (toStr operation.priority (isCheckNeed isChecked parentChecked) indent) in
                    printedOperands |> List.map box |> List.toArray |> Wrappers.format opStr
                | Operator(operator, isChecked) ->
                    assert (List.length operands >= 2)
                    let printedOperands = operands |> List.map (toStr operation.priority (isCheckNeed isChecked parentChecked) indent)
                    let sortedOperands = if Operations.isCommutative operator && not isChecked then List.sort printedOperands else printedOperands
                    sortedOperands |> String.concat (Operations.operationToString operator)
                        |> checkExpression isChecked parentChecked operation.priority parentPriority
                | Cast(orig, dest, isChecked) ->
                    assert (List.length operands = 1)
                    let format = checkExpression isChecked parentChecked operation.priority parentPriority "({0}){1}" in
                    format2 format (dest.ToString()) (toStr operation.priority (isCheckNeed isChecked parentChecked) indent (List.head operands))
                | Application f -> operands |> List.map (toStr -1 parentChecked indent) |> Wrappers.join ", " |> format2 "{0}({1})" f
            | Struct(fields, t) ->
                let fieldToString name term = String.Format("| {0} ~> {1}", name, toStr -1 false (indent + "\t") term)
                let printed = fields |> Map.map fieldToString |> Map.toSeq |> Seq.map snd |> Seq.sort
                String.Format("STRUCT {0}[\n" + indent + "{1}]", t.ToString(), String.Join("\n" + indent, printed))
            | Array(_, None, contents, dimensions, _) ->
                String.Format("[| {0} ... {1} ... |]", arrayContentsToString contents ": " "; ", Array.map toString dimensions |> join " x ")
            | Array(_, Some constant, contents, dimensions, _) ->
                String.Format("{0}: [| {1} ({2}) |]", toString constant, arrayContentsToString contents ": " "; ", Array.map toString dimensions |> join " x ")
            | StackRef(key, path, _) -> let path = List.sort path in String.Format("(StackRef {0})", (key, path).ToString())
            | HeapRef(addr, [], _) -> String.Format("(HeapRef {0})", toStr -1 false indent addr)
            | HeapRef(addr, path, _) -> String.Format("(HeapRef {0} with path {1})", toStr -1 false indent addr, (List.sort path).ToString())
            | Union(guardedTerms) ->
                let guardedToString (guard, term) = String.Format("| {0} ~> {1}", toStr -1 false indent guard, toStr -1 false indent term)
                let printed = guardedTerms |> Seq.map guardedToString |> Seq.sort
                String.Format("UNION[\n" + indent + "{0}]", String.Join("\n" + indent, printed))
        in
        toStr -1 false "\t" this

and
    [<CustomEquality;NoComparison>]
    TermRef =
        | TermRef of Term ref
        override this.GetHashCode() =
            Microsoft.FSharp.Core.LanguagePrimitives.PhysicalHash(this)
        override this.Equals(o : obj) =
            match o with
            | :? TermRef as other -> this.GetHashCode() = other.GetHashCode()
            | _ -> false

and SymbolicConstantSource =
    | LocationAccess of Term
    | ArrayAccess of Term * Term
    | FieldAccess of string * SymbolicConstantSource
    | UnboundedRecursion of TermRef
    | Symbolization of Term
    | SymbolicArrayLength of Term * int * bool // (Array constant) * dimension * (length if true or lower bound if false)
    | SymbolicConstantType of TermType

module public Terms =

    let public IsVoid = function
        | Nop -> true
        | _ -> false

    let public IsError = function
        | Error _ -> true
        | _ -> false

    let public IsConcrete = function
        | Concrete _ -> true
        | _ -> false

    let public IsExpression = function
        | Expression _ -> true
        | _ -> false

    let public IsArray = function
        | Array _ -> true
        | _ -> false

    let public IsUnion = function
        | Union _ -> true
        | _ -> false

    let public IsTrue = function
        | Concrete(b, t) when Types.IsBool t && (b :?> bool) -> true
        | _ -> false

    let public IsFalse = function
        | Concrete(b, t) when Types.IsBool t && not (b :?> bool) -> true
        | _ -> false

    let rec public Just predicate = function
        | t when predicate t -> true
        | Union gvs -> List.forall predicate (snd (List.unzip gvs))
        | _ -> false

    let public IsNull = function
        | Concrete(null, _) -> true
        | _ -> false

    let public IsStackRef = function
        | StackRef _ -> true
        | _ -> false

    let public IsHeapRef = function
        | HeapRef _ -> true
        | _ -> false

    let rec public IsRef = function
        | HeapRef _
        | StackRef _ -> true
        | Union gvs -> List.forall (snd >> IsRef) gvs
        | _ -> false

    let public OperationOf = function
        | Expression(op, _, _) -> op
        | term -> raise(new ArgumentException(String.Format("Expression expected, {0} recieved", term)))

    let public ArgumentsOf = function
        | Expression(_, args, _) -> args
        | term -> raise(new ArgumentException(String.Format("Expression expected, {0} recieved", term)))

    let rec public TypeOf = function
        | Error _ -> TermType.Bottom
        | Nop -> TermType.Void
        | Concrete(_, t) -> t
        | Constant(_, _, t) -> t
        | Expression(_, _, t) -> t
        | Struct(_, t) -> t
        | StackRef(_, _, t) -> t
        | HeapRef(_, _, t) -> t
        | Array(_, _, _, _, t) -> t
        | Union gvs ->
            match (List.filter (fun t -> not (Types.IsBottom t || Types.IsVoid t)) (List.map (snd >> TypeOf) gvs)) with
            | [] -> TermType.Bottom
            | t::ts ->
                let allSame = List.forall ((=) t) ts in
                if allSame then t
                else
                    // TODO: return union of types!
                    __notImplemented__()


    let public IsBool =                 TypeOf >> Types.IsBool
    let public IsInteger =              TypeOf >> Types.IsInteger
    let public IsReal =                 TypeOf >> Types.IsReal
    let public IsNumeric =              TypeOf >> Types.IsNumeric
    let public IsString =               TypeOf >> Types.IsString
    let public IsFunction =             TypeOf >> Types.IsFunction
    let public IsPrimitive =            TypeOf >> Types.IsPrimitive
    let public DomainOf =               TypeOf >> Types.DomainOf
    let public RangeOf =                TypeOf >> Types.RangeOf
    let public IsRelation =             TypeOf >> Types.IsRelation

    let public FreshConstant name source t =
        Constant(name, source, Types.FromDotNetType t)

    let public MakeConcrete value (t : System.Type) =
        let actualType = if (value :> obj) = null then t else value.GetType() in
        try
            if actualType = t then Concrete(value, Types.FromDotNetType t)
            else
                if typedefof<IConvertible>.IsAssignableFrom(actualType)
                then
                    let casted =
                        if t.IsPointer
                        then new IntPtr(Convert.ChangeType(value, typedefof<int64>) :?> int64) :> obj
                        else Convert.ChangeType(value, t) in
                    Concrete(casted, Types.FromDotNetType t)
                else
                    if t.IsAssignableFrom(actualType)
                    then Concrete(value, Types.FromDotNetType t)
                    else raise(new InvalidCastException(format2 "Cannot cast {0} to {1}!" t.FullName actualType.FullName))
        with
        | e ->
            // TODO: this is for debug, remove it when becomes relevant!
            raise(new InvalidCastException(format2 "Cannot cast {0} to {1}!" t.FullName actualType.FullName))
            Error(Concrete(e :> obj, Types.FromDotNetType (e.GetType())))

    let public MakeTrue =
        Concrete(true :> obj, Bool)

    let public MakeFalse =
        Concrete(false :> obj, Bool)

    let public MakeBool predicate =
        if predicate then MakeTrue else MakeFalse
    
    let public MakeNull typ =
        MakeConcrete null typ

    let public MakeError exn =
        Error (MakeConcrete exn (exn.GetType()))

    let public MakeNumber n =
        Concrete(n, Numeric(n.GetType()))

    let public MakeBinary operation x y isChecked t =
        assert(Operations.isBinary operation)
        Expression(Operator(operation, isChecked), [x; y], t)

    let public MakeNAry operation x isChecked t =
        match x with 
        | [] -> raise(new ArgumentException("List of args should be not empty"))
        | [x] -> x
        | _ -> Expression(Operator(operation, isChecked), x, t)

    let public MakeUnary operation x isChecked t =
        assert(Operations.isUnary operation)
        Expression(Operator(operation, isChecked), [x], t)

    let public Negate term =
        assert(IsBool term)
        MakeUnary OperationType.LogicalNeg term false Bool

    let (|True|_|) term = if IsTrue term then Some True else None
    let (|False|_|) term = if IsFalse term then Some False else None
    let (|Null|_|) term = if IsNull term then Some Null else None

    let (|GuardedValues|_|) = function
        | Union(gvs) -> Some(GuardedValues(List.unzip gvs))
        | _ -> None

    let (|UnaryMinus|_|) = function
        | Expression(Operator(OperationType.UnaryMinus, isChecked), [x], t) -> Some(UnaryMinus(x, isChecked, t))
        | _ -> None

    let (|Add|_|) = function
        | Expression(Operator(OperationType.Add, isChecked), [x;y], t) -> Some(Add(x, y, isChecked, t))
        | _ -> None

    let (|Sub|_|) = function
        | Expression(Operator(OperationType.Subtract, isChecked), [x;y], t) -> Some(Sub(x, y, isChecked, t))
        | _ -> None

    let (|Mul|_|) = function
        | Expression(Operator(OperationType.Multiply, isChecked), [x;y], t) -> Some(Mul(x, y, isChecked, t))
        | _ -> None

    let (|Div|_|) = function
        | Expression(Operator(OperationType.Divide, isChecked), [x;y], t) -> Some(Div(x, y, isChecked, t))
        | _ -> None

    let (|Rem|_|) = function
        | Expression(Operator(OperationType.Remainder, isChecked), [x;y], t) -> Some(Rem(x, y, isChecked, t))
        | _ -> None

    let (|Negation|_|) term =
        match term with
        | Expression(Operator(OperationType.LogicalNeg, _), [x], t) -> Some(Negation(x, t))
        | _ -> None

    let (|Conjunction|_|) term =
        match term with
        | Expression(Operator(OperationType.LogicalAnd, _), [x;y], t) -> Some(Conjunction(x, y, t))
        | _ -> None

    let (|ConjunctionList|_|) term =
        match term with
        | Expression(Operator(OperationType.LogicalAnd, _), x, t) -> Some(ConjunctionList(x, t))
        | _ -> None

    let (|Disjunction|_|) term =
        match term with
        | Expression(Operator(OperationType.LogicalOr, _), [x;y], t) -> Some(Disjunction(x, y, t))
        | _ -> None

    let (|DisjunctionList|_|) term =
        match term with
        | Expression(Operator(OperationType.LogicalOr, _), x, t) -> Some(DisjunctionList(x, t))
        | _ -> None

    let (|Xor|_|) term =
        match term with
        | Expression(Operator(OperationType.LogicalXor, _), [x;y], t) -> Some(Xor(x, y, t))
        | _ -> None

    let rec private addConstants mapper (visited : HashSet<Term>) acc = function
        | Constant(name, source, t) as term when visited.Add(term) ->
            Console.WriteLine("{0};;; ;;;{1}", term, visited);
            let acc =
                match source with
                | LocationAccess loc -> addConstants mapper visited acc loc
                | ArrayAccess(arr, idx) -> addConstants mapper visited (addConstants mapper visited acc arr) idx
                | FieldAccess(_, src) -> addConstants mapper visited acc (Constant(name, src, t))
                | UnboundedRecursion (TermRef app) -> addConstants mapper visited acc !app
                | Symbolization loc -> addConstants mapper visited acc loc
                | SymbolicArrayLength(arr, _, _) -> addConstants mapper visited acc arr
            in
            match mapper acc term with
            | Some value -> value::acc
            | None -> acc
        | Array(lowerBounds, constant, contents, lengths, _) ->
            let indices, values = List.unzip contents in
            match constant with
            | Some c -> addConstants mapper visited acc c
            | None -> acc
            |> addConstantsMany mapper visited (Seq.ofArray lowerBounds)
            |> addConstantsMany mapper visited indices
            |> addConstantsMany mapper visited values
            |> addConstantsMany mapper visited lengths
        | Expression(_, args, _) ->
            addConstantsMany mapper visited args acc
        | Struct(fields, _) ->
            addConstantsMany mapper visited (fields |> Map.toList |> List.unzip |> snd) acc
        | HeapRef(addr, _, _) ->
            addConstants mapper visited acc addr
        | GuardedValues(gs, vs) ->
            addConstantsMany mapper visited gs acc |> addConstantsMany mapper visited vs
        | Error e ->
            addConstants mapper visited acc e
        | _ -> acc

    and private addConstantsMany mapper visited terms acc =
        Seq.fold (addConstants mapper visited) acc terms

    let public filterMapConstants mapper terms =
        List.fold (addConstants mapper (new HashSet<Term>())) [] terms
