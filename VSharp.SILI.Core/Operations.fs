namespace VSharp.Core

type OperationType =
    | UnaryMinus = 0
    | BitwiseNot = 1
    | BitwiseAnd = 2
    | BitwiseOr = 3
    | BitwiseXor = 4
    | LogicalNot = 5
    | LogicalAnd = 6
    | LogicalOr = 7
    | LogicalXor = 8
    | Equal = 9
    | NotEqual = 10
    | Greater = 11
    | Greater_Un = 12
    | Less = 13
    | Less_Un = 14
    | GreaterOrEqual = 15
    | GreaterOrEqual_Un = 16
    | LessOrEqual = 17
    | LessOrEqual_Un = 18
    | Add = 19
    | AddNoOvf = 20
    | AddNoOvf_Un = 21
    | Subtract = 22
    | SubNoOvf = 23
    | SubNoOvf_Un = 24
    | Divide = 25
    | Divide_Un = 26
    | Multiply = 27
    | MultiplyNoOvf = 28
    | MultiplyNoOvf_Un = 29
    | Remainder = 30
    | Remainder_Un = 31
    | ShiftLeft = 32
    | ShiftRight = 33
    | ShiftRight_Un = 34

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
        | OperationType.Divide_Un
        | OperationType.Remainder
        | OperationType.Remainder_Un -> maxPriority - 1
        | OperationType.Add
        | OperationType.Subtract -> maxPriority - 2
        | OperationType.ShiftLeft
        | OperationType.ShiftRight
        | OperationType.ShiftRight_Un -> maxPriority - 3
        | OperationType.AddNoOvf
        | OperationType.AddNoOvf_Un
        | OperationType.MultiplyNoOvf
        | OperationType.MultiplyNoOvf_Un
        | OperationType.SubNoOvf
        | OperationType.SubNoOvf_Un
        | OperationType.Less
        | OperationType.Less_Un
        | OperationType.LessOrEqual
        | OperationType.LessOrEqual_Un
        | OperationType.Greater
        | OperationType.Greater_Un
        | OperationType.GreaterOrEqual
        | OperationType.GreaterOrEqual_Un -> maxPriority - 4
        | OperationType.Equal
        | OperationType.NotEqual -> maxPriority - 5
        | OperationType.BitwiseAnd
        | OperationType.LogicalAnd -> maxPriority - 6
        | OperationType.BitwiseXor
        | OperationType.LogicalXor -> maxPriority - 7
        | OperationType.BitwiseOr
        | OperationType.LogicalOr -> maxPriority - 8
        | _ -> __unreachable__()

    let isCommutative = function
        | OperationType.Add
        | OperationType.AddNoOvf
        | OperationType.AddNoOvf_Un
        | OperationType.Multiply
        | OperationType.MultiplyNoOvf
        | OperationType.MultiplyNoOvf_Un
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
        | OperationType.Divide
        | OperationType.Divide_Un -> " / "
        | OperationType.Equal -> " == "
        | OperationType.NotEqual -> " != "
        | OperationType.Greater
        | OperationType.Greater_Un -> " > "
        | OperationType.GreaterOrEqual
        | OperationType.GreaterOrEqual_Un -> " >= "
        | OperationType.Less
        | OperationType.Less_Un -> " < "
        | OperationType.LessOrEqual
        | OperationType.LessOrEqual_Un -> " <= "
        | OperationType.BitwiseAnd -> " && "
        | OperationType.LogicalAnd -> " & "
        | OperationType.BitwiseOr -> " || "
        | OperationType.LogicalOr -> " | "
        | OperationType.BitwiseNot -> "~%s"
        | OperationType.LogicalNot -> "!%s"
        | OperationType.BitwiseXor -> " ^^ "
        | OperationType.LogicalXor -> " ^ "
        | OperationType.Multiply -> " * "
        | OperationType.Remainder
        | OperationType.Remainder_Un -> " % "
        | OperationType.ShiftLeft -> " << "
        | OperationType.ShiftRight
        | OperationType.ShiftRight_Un -> " >> "
        | OperationType.Subtract -> " - "
        | OperationType.UnaryMinus -> "-%s"
        | OperationType.AddNoOvf
        | OperationType.AddNoOvf_Un -> " + (no ovf) "
        | OperationType.MultiplyNoOvf
        | OperationType.MultiplyNoOvf_Un -> " * (no ovf) "
        | OperationType.SubNoOvf
        | OperationType.SubNoOvf_Un -> " - (no ovf) "
        | _ -> ""

    let deduceArithmeticBinaryExpressionTargetType op x y =
        match op with
        | OperationType.AddNoOvf
        | OperationType.AddNoOvf_Un
        | OperationType.MultiplyNoOvf
        | OperationType.MultiplyNoOvf_Un
        | OperationType.SubNoOvf
        | OperationType.SubNoOvf_Un
        | OperationType.Equal
        | OperationType.NotEqual
        | OperationType.Greater
        | OperationType.Greater_Un
        | OperationType.GreaterOrEqual
        | OperationType.GreaterOrEqual_Un
        | OperationType.Less
        | OperationType.Less_Un
        | OperationType.LessOrEqual
        | OperationType.LessOrEqual_Un -> TypeUtils.deduceComparisonTargetType x y
        | OperationType.Multiply
        | OperationType.Add
        | OperationType.Subtract
        | OperationType.Divide
        | OperationType.Divide_Un
        | OperationType.Remainder
        | OperationType.Remainder_Un -> TypeUtils.deduceSimpleArithmeticOperationTargetType x y
        | OperationType.ShiftLeft
        | OperationType.ShiftRight
        | OperationType.ShiftRight_Un -> TypeUtils.deduceShiftTargetType x y
        | OperationType.BitwiseAnd
        | OperationType.BitwiseOr
        | OperationType.BitwiseXor -> TypeUtils.deduceLogicalArithmeticOperationTargetType x y
        | _ -> TypeUtils.failDeduceBinaryTargetType (operationToString op) x y
