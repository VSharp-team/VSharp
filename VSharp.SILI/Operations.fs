namespace VSharp

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
        | OperationType.Add -> " + "
        | OperationType.Assignment -> " = "
        | OperationType.AssignmentAdd -> " += "
        | OperationType.AssignmentDivide -> " /= "
        | OperationType.AssignmentLogicalAnd -> " &= "
        | OperationType.AssignmentLogicalOr -> " |= "
        | OperationType.AssignmentLogicalXor -> " ^= "
        | OperationType.AssignmentMultiply -> " *= "
        | OperationType.AssignmentRemainder -> " %= "
        | OperationType.AssignmentShiftLeft -> " <<= "
        | OperationType.AssignmentShiftRight -> " >>= "
        | OperationType.AssignmentSubtract -> " -= "
        | OperationType.ConditionalAnd -> " && "
        | OperationType.ConditionalOr -> " || "
        | OperationType.Divide -> " / "
        | OperationType.Equal -> " == "
        | OperationType.Greater -> " > "
        | OperationType.GreaterOrEqual -> " >= "
        | OperationType.Less -> " < "
        | OperationType.LessOrEqual -> " <= "
        | OperationType.LogicalAnd -> " & "
        | OperationType.LogicalOr -> " | "
        | OperationType.LogicalNeg -> "(!{0})"
        | OperationType.LogicalXor -> " ^ "
        | OperationType.Multiply ->" * "
        | OperationType.Not -> "(~{0})"
        | OperationType.NotEqual -> " != "
        | OperationType.NullCoalescing -> " ?? "
        | OperationType.PostfixIncrement -> "({0}++)"
        | OperationType.PostfixDecrement -> "({0}--)"
        | OperationType.PrefixIncrement -> "(++{0})"
        | OperationType.PrefixDecrement -> "(--{0})"
        | OperationType.Remainder -> " % "
        | OperationType.ShiftLeft -> " << "
        | OperationType.ShiftRight -> " >> "
        | OperationType.Subtract -> " - "
        | OperationType.UnaryMinus -> "(-{0})"
        | OperationType.UnaryPlus -> "(+{0})"
        | _ -> ""

    let internal isUnary op = operationArity op = 1
    let internal isBinary op = operationArity op = 2

    let internal isAssignment = (=) OperationType.Assignment

    let internal isOperationAssignment = function
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

    let internal getAssignmentOperation = function
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
        | op -> raise(new System.ArgumentException("Internal error: " + op.ToString() + " is not an assignment operation"))
