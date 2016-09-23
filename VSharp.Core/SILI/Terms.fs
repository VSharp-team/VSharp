namespace VSharp.Core.Symbolic

open JetBrains.Decompiler.Ast
open System
open VSharp.Core.Utils

type public Operation =
    | Operator of OperationType * bool
    | Application of string

type public Term =
    | Bottom
    | Nop
    | Concrete of Object * TermType
    | Constant of string * TermType
    | Expression of Operation * Term list * TermType
    | Union of (Term * Term) list

    override this.ToString() =
        match this with
        | Bottom -> "<ERROR!>"
        | Nop -> "<VOID>"
        | Constant(name, _) -> name
        | Expression(operation, operands, _) ->
            let printedOperands = operands |> List.map Wrappers.toString
            match operation with
            | Operator(operator, isChecked) ->
                let format = Operators.operatorToStringFormat operator
                let count = Operators.operatorArity operator
                let checkedFormat = if isChecked then format + "✓" else format
                if (List.length operands) <> count then
                    raise(new ArgumentException(String.Format("Wrong number of arguments for {0}: expected {1}, got {2}", operator.ToString(), count, List.length operands)))
                else String.Format(checkedFormat, printedOperands |> List.map box |> List.toArray)
            | Application f -> String.Format("{0}({1})", f, String.Join(", ", printedOperands))
        | Concrete(value, _) -> value.ToString()
        | Union(guardedTerms) ->
            let guardedToString (guard, term) =
                String.Format("| {0} -> {1}", guard, term)
            let printed = guardedTerms |> Seq.map guardedToString
            String.Format("UNION\n\t{0}", String.Join("\n\t", printed))

module public Terms =

    let public IsVoid term =
        match term with
        | Nop -> true
        | _ -> false

    let public IsError term =
        match term with
        | Bottom -> true
        | _ -> false

    let rec public TypeOf term =
        match term with
        | Bottom
        | Nop -> TermType.Void
        | Concrete(_, t) -> t
        | Constant(_, t) -> t
        | Expression(_, _,  t) -> t
        | Union ts ->
            if List.isEmpty ts then TermType.Void
            else (fst >> TypeOf) (List.head ts)

    let public IsBool =                 TypeOf >> Types.IsBool
    let public IsInteger =              TypeOf >> Types.IsInteger
    let public IsReal =                 TypeOf >> Types.IsReal
    let public IsNumeric t =            TypeOf >> Types.IsNumeric
    let public IsString t =             TypeOf >> Types.IsString
    let public IsPrimitiveSolvable =    TypeOf >> Types.IsPrimitiveSolvable
    let public IsSolvable t =           TypeOf >> Types.IsSolvable
    let public DomainOf =               TypeOf >> Types.DomainOf
    let public RangeOf =                TypeOf >> Types.RangeOf
    let public IsRelation =             TypeOf >> Types.IsRelation

    let public FreshConstant name t =
        Constant(name, Types.FromPrimitiveDotNetType t)

    let public MakeConcrete value t =
        Concrete(value, Types.FromPrimitiveDotNetType t)

    let public MakeBinary operation x y isChecked t =
        assert(Operators.isBinary operation)
        Expression(Operator(operation, isChecked), [x; y], t)

    let public MakeUnary operation x isChecked t =
        assert(Operators.isUnary operation)
        Expression(Operator(operation, isChecked), [x], t)
