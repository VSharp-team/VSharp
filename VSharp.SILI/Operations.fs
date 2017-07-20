namespace VSharp

open JetBrains.Decompiler.Ast

module Operations =
    let internal operationArity op =
        match op with
        | OperationType.LogicalNeg       | OperationType.Not
        | OperationType.PostfixIncrement | OperationType.PostfixDecrement
        | OperationType.PrefixIncrement  | OperationType.PrefixDecrement
        | OperationType.UnaryMinus       | OperationType.UnaryPlus
            -> 1
        | _ -> 2

    let internal maxPriority = 13

    let internal operationPriority op =
        match op with
        | OperationType.PostfixIncrement | OperationType.PostfixDecrement -> maxPriority
        | OperationType.PrefixIncrement  | OperationType.PrefixDecrement
        | OperationType.LogicalNeg       | OperationType.Not
        | OperationType.PrefixIncrement  | OperationType.PrefixDecrement
        | OperationType.UnaryMinus       | OperationType.UnaryPlus -> maxPriority - 1
        | OperationType.Multiply         | OperationType.Divide
        | OperationType.Remainder -> maxPriority - 2
        | OperationType.Add              | OperationType.Subtract -> maxPriority - 3
        | OperationType.ShiftLeft        | OperationType.ShiftRight -> maxPriority - 4
        | OperationType.Less             | OperationType.LessOrEqual
        | OperationType.Greater          | OperationType.GreaterOrEqual -> maxPriority - 5
        | OperationType.Equal            | OperationType.NotEqual -> maxPriority - 6
        | OperationType.LogicalAnd -> maxPriority - 7
        | OperationType.LogicalXor -> maxPriority - 8
        | OperationType.LogicalOr -> maxPriority - 9
        | OperationType.ConditionalAnd -> maxPriority - 10
        | OperationType.ConditionalOr -> maxPriority - 11
        | OperationType.Assignment
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
        | OperationType.AssignmentRemainder
        | OperationType.AssignmentShiftLeft -> maxPriority - 12
        | _ -> -1 // condition = maxPriority - 13

    let internal isCommutative  = function
        | OperationType.Add | OperationType.Multiply | OperationType.Equal | OperationType.NotEqual
        | OperationType.LogicalAnd | OperationType.LogicalOr | OperationType.LogicalXor -> true
        | _ -> false

    let internal operationToString op =
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
        | OperationType.LogicalNeg -> "!{0}"
        | OperationType.LogicalXor -> " ^ "
        | OperationType.Multiply ->" * "
        | OperationType.Not -> "~{0}"
        | OperationType.NotEqual -> " != "
        | OperationType.NullCoalescing -> " ?? "
        | OperationType.PostfixIncrement -> "{0}++"
        | OperationType.PostfixDecrement -> "{0}--"
        | OperationType.PrefixIncrement -> "++{0}"
        | OperationType.PrefixDecrement -> "--{0}"
        | OperationType.Remainder -> " % "
        | OperationType.ShiftLeft -> " << "
        | OperationType.ShiftRight -> " >> "
        | OperationType.Subtract -> " - "
        | OperationType.UnaryMinus -> "-{0}"
        | OperationType.UnaryPlus -> "+{0}"
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

    type StandardFunction =
        | Arccosine
        | Arcsine
        | Arctangent
        | Arctangent2
        | Ceiling
        | Cosine
        | HyperbolicCosine
        | Floor
        | Sine
        | Tangent
        | HyperbolicSine
        | HyperbolicTangent
        | Round
        | SquareRoot
        | Logarithm
        | Logarithm10
        | Exponent
        | Power
        | Absolute
        | AbsoluteS
        override this.ToString() =
            match this with
            | Arccosine -> "arccos"
            | Arcsine -> "arcsin"
            | Arctangent -> "arctan"
            | Arctangent2 -> "arctan"
            | Ceiling -> "ceiling"
            | Cosine -> "cos"
            | HyperbolicCosine -> "cosh"
            | Floor -> "floor"
            | Sine -> "sin"
            | Tangent -> "tan"
            | HyperbolicSine -> "sinh"
            | HyperbolicTangent -> "tanh"
            | Round -> "round"
            | SquareRoot -> "sqrt"
            | Logarithm -> "log"
            | Logarithm10 -> "log10"
            | Exponent -> "exp"
            | Power -> "pow"
            | Absolute -> "abs"
            | AbsoluteS -> "abs"
