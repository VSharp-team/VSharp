namespace VSharp.Core.Symbolic.Reduction

open JetBrains.Decompiler.Ast
open VSharp.Core.Symbolic

module internal Arithmetics =

    let internal reduceOperation op x y isChecked =
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

    let internal reduceAddition op x y isChecked =
        match (x, y) with
        //| (Concrete(x, typeOfX), Concrete(y, typeOfY)) -> calcAddition op x typeOfX y typeOfY isChecked psiModule
        | _ -> Terms.MakeBinary op x y
