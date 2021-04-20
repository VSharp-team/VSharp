namespace VSharp.Core

type OperationType =
    | UnaryMinus
    | BitwiseNot
    | BitwiseAnd
    | BitwiseOr
    | BitwiseXor
    | LogicalNot
    | LogicalAnd
    | LogicalOr
    | LogicalXor
    | Equal
    | NotEqual
    | Greater
    | Less
    | GreaterOrEqual
    | LessOrEqual
    | Add
    | Subtract
    | Divide
    | Multiply
    | Remainder
    | ShiftLeft
    | ShiftRight

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

    override x.ToString() =
        match x with
        | Arccosine -> "arccos"
        | Arcsine -> "arcsin"
        | Arctangent -> "arctan"
        | Arctangent2 -> "arctan"
        | Ceiling -> "ceil"
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

module internal Operations =
    open VSharp

    let operationArity = function
        | OperationType.LogicalNot
        | OperationType.BitwiseNot
        | OperationType.UnaryMinus
            -> 1
        | _ -> 2
    let isUnary op = operationArity op = 1
    let isBinary op = operationArity op = 2

    let maxPriority = 10
    let operationPriority = function
        | OperationType.BitwiseNot
        | OperationType.LogicalNot
        | OperationType.UnaryMinus -> maxPriority
        | OperationType.Multiply
        | OperationType.Divide
        | OperationType.Remainder -> maxPriority - 1
        | OperationType.Add
        | OperationType.Subtract -> maxPriority - 2
        | OperationType.ShiftLeft
        | OperationType.ShiftRight -> maxPriority - 3
        | OperationType.Less
        | OperationType.LessOrEqual
        | OperationType.Greater
        | OperationType.GreaterOrEqual -> maxPriority - 4
        | OperationType.Equal
        | OperationType.NotEqual -> maxPriority - 5
        | OperationType.BitwiseAnd
        | OperationType.LogicalAnd -> maxPriority - 6
        | OperationType.BitwiseXor
        | OperationType.LogicalXor -> maxPriority - 7
        | OperationType.BitwiseOr
        | OperationType.LogicalOr -> maxPriority - 8

    let isCommutative = function
        | OperationType.Add
        | OperationType.Multiply
        | OperationType.Equal
        | OperationType.BitwiseAnd
        | OperationType.BitwiseOr
        | OperationType.BitwiseXor
        | OperationType.LogicalAnd
        | OperationType.LogicalOr
        | OperationType.LogicalXor -> true
        | _ -> false

    let operationToString = function
        | OperationType.Add -> " + "
        | OperationType.Divide -> " / "
        | OperationType.Equal -> " == "
        | OperationType.NotEqual -> " != "
        | OperationType.Greater -> " > "
        | OperationType.GreaterOrEqual -> " >= "
        | OperationType.Less -> " < "
        | OperationType.LessOrEqual -> " <= "
        | OperationType.BitwiseAnd -> " && "
        | OperationType.LogicalAnd -> " & "
        | OperationType.BitwiseOr -> " || "
        | OperationType.LogicalOr -> " | "
        | OperationType.BitwiseNot -> "~%s"
        | OperationType.LogicalNot -> "!%s"
        | OperationType.LogicalXor -> " ^ "
        | OperationType.Multiply -> " * "
        | OperationType.Remainder -> " % "
        | OperationType.ShiftLeft -> " << "
        | OperationType.ShiftRight -> " >> "
        | OperationType.Subtract -> " - "
        | OperationType.UnaryMinus -> "-%s"
        | _ -> ""

    let deduceArithmeticBinaryExpressionTargetType op x y =
        match op with
        | OperationType.Equal
        | OperationType.NotEqual
        | OperationType.Greater
        | OperationType.GreaterOrEqual
        | OperationType.Less
        | OperationType.LessOrEqual -> TypeUtils.deduceComparisonTargetType x y
        | OperationType.Multiply
        | OperationType.Add
        | OperationType.Subtract
        | OperationType.Divide
        | OperationType.Remainder -> TypeUtils.deduceSimpleArithmeticOperationTargetType x y
        | OperationType.ShiftLeft
        | OperationType.ShiftRight -> TypeUtils.deduceShiftTargetType x y
        | OperationType.BitwiseAnd
        | OperationType.BitwiseOr
        | OperationType.BitwiseXor -> TypeUtils.deduceLogicalArithmeticOperationTargetType x y
        | _ -> TypeUtils.failDeduceBinaryTargetType (operationToString op) x y
