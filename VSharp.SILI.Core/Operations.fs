namespace VSharp.Core

type OperationType =
    | Not = 0
    | UnaryMinus = 1
    | LogicalNeg = 2
    | LogicalAnd = 3
    | LogicalOr = 4
    | LogicalXor = 5
    | Equal = 6
    | NotEqual = 7
    | Greater = 8
    | Less = 9
    | GreaterOrEqual = 10
    | LessOrEqual = 11
    | Add = 12
    | Subtract = 13
    | Divide = 14
    | Multiply = 15
    | Remainder = 16
    | ShiftLeft = 17
    | ShiftRight = 18

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

    let operationArity = function
        | OperationType.LogicalNeg
        | OperationType.Not
        | OperationType.UnaryMinus
            -> 1
        | _ -> 2
    let isUnary op = operationArity op = 1
    let isBinary op = operationArity op = 2

    let maxPriority = 10
    let operationPriority = function
        | OperationType.LogicalNeg
        | OperationType.Not
        | OperationType.UnaryMinus-> maxPriority
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
        | OperationType.LogicalAnd -> maxPriority - 6
        | OperationType.LogicalXor -> maxPriority - 7
        | OperationType.LogicalOr -> maxPriority - 8
        | _ -> -1

    let isCommutative = function
        | OperationType.Add
        | OperationType.Multiply
        | OperationType.Equal
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
        | OperationType.LogicalAnd -> " & "
        | OperationType.LogicalOr -> " | "
        | OperationType.LogicalNeg -> "!%s"
        | OperationType.LogicalXor -> " ^ "
        | OperationType.Multiply -> " * "
        | OperationType.Not -> "~%s"
        | OperationType.Remainder -> " % "
        | OperationType.ShiftLeft -> " << "
        | OperationType.ShiftRight -> " >> "
        | OperationType.Subtract -> " - "
        | OperationType.UnaryMinus -> "-%s"
        | _ -> ""
