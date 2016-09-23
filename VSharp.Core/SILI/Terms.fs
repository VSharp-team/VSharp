namespace VSharp.Core.Symbolic

open JetBrains.Decompiler.Ast
open System

type Operation =
    | Operator of OperationType
    | Application of string

type Term =
    | Void
    | Constant of string * Type
    | Expression of Operation * Term list
    | Concrete of Object * Type

module Terms =
    let private operatorToStringFormat (op : OperationType) =
        match op with
        | OperationType.Add -> ("({0} + {1})", 2)
        | OperationType.Assignment -> ("({0} = {1})", 2)
        | OperationType.AssignmentAdd -> ("({0} += {1})", 2)
        | OperationType.AssignmentDivide -> ("({0} /= {1})", 2)
        | OperationType.AssignmentLogicalAnd -> ("({0} &= {1})", 2)
        | OperationType.AssignmentLogicalOr -> ("({0} |= {1})", 2)
        | OperationType.AssignmentLogicalXor -> ("({0} ^= {1})", 2)
        | OperationType.AssignmentMultiply -> ("({0} *= {1})", 2)
        | OperationType.AssignmentRemainder -> ("({0} %= {1})", 2)
        | OperationType.AssignmentShiftLeft -> ("({0} <<= {1})", 2)
        | OperationType.AssignmentShiftRight -> ("({0} >>= {1})", 2)
        | OperationType.AssignmentSubtract -> ("({0} -= {1}_", 2)
        | OperationType.ConditionalAnd -> ("({0} && {1})", 2)
        | OperationType.ConditionalOr -> ("({0} || {1})", 2)
        | OperationType.Divide -> ("({0} / {1}_", 2)
        | OperationType.Equal -> ("({0} == {1})", 2)
        | OperationType.Greater -> ("({0} > {1})", 2)
        | OperationType.GreaterOrEqual -> ("({0} >= {1})", 2)
        | OperationType.Less -> ("({0} < {1})", 2)
        | OperationType.LessOrEqual -> ("({0} <= {1})", 2)
        | OperationType.LogicalAnd -> ("({0} & {1})", 2)
        | OperationType.LogicalOr -> ("({0} | {1})", 2)
        | OperationType.LogicalNeg -> ("(~{0})", 1)
        | OperationType.LogicalXor -> ("({0} ^ {1})", 2)
        | OperationType.Multiply -> ("({0} * {1})", 2)
        | OperationType.Not -> ("(!{0})", 1)
        | OperationType.NotEqual -> ("({0} != {1})", 2)
        | OperationType.NullCoalescing -> ("({0} ?? {1})", 2)
        | OperationType.PostfixIncrement -> ("({0}++)", 1)
        | OperationType.PostfixDecrement -> ("({0}--)", 1)
        | OperationType.PrefixIncrement -> ("(++{0})", 1)
        | OperationType.PrefixDecrement -> ("(--{0})", 1)
        | OperationType.Remainder -> ("({0} % {1})", 2)
        | OperationType.ShiftLeft -> ("({0} << {1})", 2)
        | OperationType.ShiftRight -> ("({0} >> {1})", 2)
        | OperationType.Subtract -> ("({0} - {1})", 2)
        | OperationType.UnaryMinus -> ("(-{0})", 1)
        | OperationType.UnaryPlus -> ("(+{0})", 1)
        | _ -> ("", 0)

    let public IsVoid term =
        match term with
        | Void -> true
        | _ -> false

    let public FreshConstant name t =
        Constant(name, t)

    let public MakeConcrete value t =
        Concrete(value, t)

    let public MakeBinary operation x y =
        assert((snd (operatorToStringFormat operation)) = 2)
        Expression(Operator operation, [x; y])

    let public MakeUnary operation x =
        assert((snd (operatorToStringFormat operation)) = 1)
        Expression(Operator operation, [x])

    let rec internal toString term =
        match term with
        | Void -> "<VOID>"
        | Constant(name, _) -> name
        | Expression(operation, operands) ->
            let printedOperands = operands |> List.map toString
            match operation with
            | Operator operator ->
                match (operatorToStringFormat operator) with
                | (format, count) ->
                    if (List.length operands) <> count then
                        raise(new ArgumentException(String.Format("Wrong number of arguments for {0}: expected {1}, got {2}", operator.ToString(), count, List.length operands)))
                    else String.Format(format, printedOperands |> List.map box |> List.toArray)
            | Application f -> String.Format("{0}({1})", f, String.Join(", ", printedOperands))
        | Concrete(value, _) -> value.ToString()
