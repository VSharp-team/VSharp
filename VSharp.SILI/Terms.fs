namespace VSharp

open JetBrains.Decompiler.Ast
open System

[<StructuralEquality;NoComparison>]
type public Operation =
    | Operator of OperationType * bool
    | Application of string
    | Cast of TermType * TermType
    | Cond

[<StructuralEquality;NoComparison>]
type public Term =
    | Error of System.Exception
    | Nop // TODO: should we get rid of it?
    | Concrete of Object * TermType
    | Constant of string * TermType
    | Expression of (Operation * Term list * TermType)
    | Union of (Term * Term) list

    override this.ToString() =
        match this with
        | Error e -> String.Format("<ERROR: {0}>", e)
        | Nop -> "<VOID>"
        | Constant(name, _) -> name
        | Expression(operation, operands, _) ->
            let printedOperands = operands |> List.map Wrappers.toString
            match operation with
            | Operator(operator, isChecked) when Operations.operationArity operator = 1 ->
                assert (List.length operands = 1) 
                let format = Operations.operationToStringFormat operator
                let checkedFormat = if isChecked then format + "✓" else format
                printedOperands |> List.map box |> List.toArray |> Wrappers.format checkedFormat
            | Operator(operator, isChecked) ->
                if List.length operands < 2 then 
                    printf " "
                else printf ""
                assert (List.length operands >= 2) 
                printedOperands |> String.concat (Operations.operationToStringFormat operator) |> sprintf (if isChecked then "(%s)✓" else"(%s)")
            | Cast(orig, dest) ->
                assert (List.length printedOperands = 1)
                format2 "({0}){1}" (dest.ToString()) (List.head printedOperands)
            | Application f -> printedOperands |> Wrappers.join ", " |> format2 "{0}({1})" f
            | Cond -> printedOperands |> List.map box |> List.toArray |> format "(if {0} then {1} else {2})"
        | Concrete(value, _) -> value.ToString()
        | Union(guardedTerms) ->
            let guardedToString (guard, term) =
                String.Format("| {0} ~> {1}", guard, term)
            let printed = guardedTerms |> Seq.map guardedToString
            String.Format("UNION\n\t{0}", String.Join("\n\t", printed))

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

    let public IsUnion = function
        | Union _ -> true
        | _ -> false

    let public IsTrue = function
        | Concrete(b, t) when Types.IsBool t && (b :?> bool) -> true
        | _ -> false

    let public IsFalse = function
        | Concrete(b, t) when Types.IsBool t && not (b :?> bool) -> true
        | _ -> false

    let public OperationOf = function
        | Expression(op, _, _) -> op
        | term -> raise(new ArgumentException(String.Format("Expression expected, {0} recieved", term)))

    let public ArgumentsOf = function
        | Expression(_, args, _) -> args
        | term -> raise(new ArgumentException(String.Format("Expression expected, {0} recieved", term)))

    let rec public TypeOf = function
        | Error _
        | Nop -> TermType.Void
        | Concrete(_, t) -> t
        | Constant(_, t) -> t
        | Expression(_, _, t) -> t
        | Union ts ->
            if List.isEmpty ts then TermType.Void
            else List.head ts |> snd |> TypeOf

    let public IsBool =                 TypeOf >> Types.IsBool
    let public IsInteger =              TypeOf >> Types.IsInteger
    let public IsReal =                 TypeOf >> Types.IsReal
    let public IsNumeric =              TypeOf >> Types.IsNumeric
    let public IsString =               TypeOf >> Types.IsString
    let public IsFunction =             TypeOf >> Types.IsFunction
    let public IsPrimitive =            TypeOf >> Types.IsPrimitive
    let public IsPrimitiveSolvable =    TypeOf >> Types.IsPrimitiveSolvable
    let public IsSolvable =             TypeOf >> Types.IsSolvable
    let public DomainOf =               TypeOf >> Types.DomainOf
    let public RangeOf =                TypeOf >> Types.RangeOf
    let public IsRelation =             TypeOf >> Types.IsRelation

    let public FreshConstant name t =
        Constant(name, Types.FromDotNetType t)

    let public MakeConcrete value (t : System.Type) =
        try
            Concrete(Convert.ChangeType(value, t), Types.FromDotNetType t)
        with
        | e ->
            failwith "Typecast error occured!" // TODO: this is for debug, remove it when becomes relevant!
            Error e

    let public MakeTrue =
        Concrete(true :> obj, Bool)

    let public MakeFalse =
        Concrete(false :> obj, Bool)

    let public MakeBinary operation x y isChecked t =
        assert(Operations.isBinary operation)
        Expression(Operator(operation, isChecked), [x; y], t)

    let public MakeBinaryOverList operation x isChecked t =
        assert(Operations.isBinary operation)
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

    let (|If|_|) = function
        | Expression(Cond, [x;y;z], t) -> Some(If(x, y, z, t))
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

    let (|Lambda|_|) = function
        | Concrete(pair, t) when Types.IsFunction t && (pair :? IFunctionSignature * IBlockStatement) ->
            Some(Lambda(pair :?> IFunctionSignature * IBlockStatement))
        | _ -> None
