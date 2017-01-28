namespace VSharp.Core.Symbolic

open JetBrains.Decompiler.Ast

module internal Operations =
    let internal operationArity op =
        match op with
        | OperationType.LogicalNeg       | OperationType.Not
        | OperationType.PostfixIncrement | OperationType.PostfixDecrement
        | OperationType.PrefixIncrement  | OperationType.PrefixDecrement
        | OperationType.UnaryMinus       | OperationType.UnaryPlus
            -> 1
        | _ -> 2

    let internal operationToStringFormat op =
        match op with
        | OperationType.Add -> "({0} + {1})"
        | OperationType.Assignment -> "({0} = {1})"
        | OperationType.AssignmentAdd -> "({0} += {1})"
        | OperationType.AssignmentDivide -> "({0} /= {1})"
        | OperationType.AssignmentLogicalAnd -> "({0} &= {1})"
        | OperationType.AssignmentLogicalOr -> "({0} |= {1})"
        | OperationType.AssignmentLogicalXor -> "({0} ^= {1})"
        | OperationType.AssignmentMultiply -> "({0} *= {1})"
        | OperationType.AssignmentRemainder -> "({0} %= {1})"
        | OperationType.AssignmentShiftLeft -> "({0} <<= {1})"
        | OperationType.AssignmentShiftRight -> "({0} >>= {1})"
        | OperationType.AssignmentSubtract -> "({0} -= {1})"
        | OperationType.ConditionalAnd -> "({0} && {1})"
        | OperationType.ConditionalOr -> "({0} || {1})"
        | OperationType.Divide -> "({0} / {1})"
        | OperationType.Equal -> "({0} == {1})"
        | OperationType.Greater -> "({0} > {1})"
        | OperationType.GreaterOrEqual -> "({0} >= {1})"
        | OperationType.Less -> "({0} < {1})"
        | OperationType.LessOrEqual -> "({0} <= {1})"
        | OperationType.LogicalAnd -> "({0} & {1})"
        | OperationType.LogicalOr -> "({0} | {1})"
        | OperationType.LogicalNeg -> "(~{0})"
        | OperationType.LogicalXor -> "({0} ^ {1})"
        | OperationType.Multiply ->"({0} * {1})"
        | OperationType.Not -> "(!{0})"
        | OperationType.NotEqual -> "({0} != {1})"
        | OperationType.NullCoalescing -> "({0} ?? {1})"
        | OperationType.PostfixIncrement -> "({0}++)"
        | OperationType.PostfixDecrement -> "({0}--)"
        | OperationType.PrefixIncrement -> "(++{0})"
        | OperationType.PrefixDecrement -> "(--{0})"
        | OperationType.Remainder -> "({0} % {1})"
        | OperationType.ShiftLeft -> "({0} << {1})"
        | OperationType.ShiftRight -> "({0} >> {1})"
        | OperationType.Subtract -> "({0} - {1})"
        | OperationType.UnaryMinus -> "(-{0})"
        | OperationType.UnaryPlus -> "(+{0})"
        | _ -> ""

    let internal isUnary op = operationArity op = 1
    let internal isBinary op = operationArity op = 2

    let internal isAssignment = (=) OperationType.Assignment

    let internal isOperationAssignment op =
        match op with
        | OperationType.AssignmentAdd
        | OperationType.AssignmentDivide
        | OperationType.AssignmentLogicalAnd
        | OperationType.AssignmentLogicalOr
        | OperationType.AssignmentLogicalXor
        | OperationType.AssignmentMultiply
        | OperationType.AssignmentRemainder
        | OperationType.AssignmentShiftLeft
        | OperationType.AssignmentShiftRight
        | OperationType.AssignmentSubtract
        | OperationType.PostfixIncrement
        | OperationType.PostfixDecrement
        | OperationType.PrefixIncrement
        | OperationType.PrefixDecrement -> true
        | _ -> false

    let internal getAssignmentOperation op =
        match op with
        | OperationType.AssignmentAdd -> OperationType.Add
        | OperationType.AssignmentDivide -> OperationType.Divide
        | OperationType.AssignmentLogicalAnd -> OperationType.LogicalAnd
        | OperationType.AssignmentLogicalOr -> OperationType.LogicalOr
        | OperationType.AssignmentLogicalXor -> OperationType.LogicalXor
        | OperationType.AssignmentMultiply -> OperationType.Multiply
        | OperationType.AssignmentRemainder -> OperationType.Remainder
        | OperationType.AssignmentShiftLeft -> OperationType.ShiftLeft
        | OperationType.AssignmentShiftRight -> OperationType.ShiftRight
        | OperationType.AssignmentSubtract -> OperationType.Subtract
        | _ -> raise(new System.ArgumentException(op.ToString() + " is not an assignment operation"))
