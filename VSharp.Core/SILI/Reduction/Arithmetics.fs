namespace VSharp.Core.Symbolic.Reduction

open JetBrains.Decompiler.Ast
open VSharp.Core.Symbolic

module internal Arithmetics =

    let internal simplifyAddition x y isChecked t =
        match (x, y) with
        | (Concrete(x, typeOfX), Concrete(y, typeOfY)) ->
            if isChecked then
                Terms.MakeConcrete (VSharp.CSharpUtils.Calculator.Add(x, y, t)) t
            else
                let success = ref true
                let result = VSharp.CSharpUtils.Calculator.AddChecked(x, y, t, success)
                if !success then Terms.MakeConcrete result t
                else Bottom
        | _ -> Terms.MakeBinary OperationType.Add x y isChecked (Types.FromPrimitiveDotNetType t)

    let internal simplifyBinaryOperation op x y isChecked t =
        match op with
        | OperationType.Add -> simplifyAddition x y isChecked t
        | OperationType.Divide
        | OperationType.Equal
        | OperationType.Greater
        | OperationType.GreaterOrEqual
        | OperationType.Less
        | OperationType.LessOrEqual
        | OperationType.LogicalAnd
        | OperationType.LogicalOr
        | OperationType.LogicalXor
        | OperationType.Multiply
        | OperationType.NotEqual
        | OperationType.NullCoalescing
        | OperationType.Remainder
        | OperationType.ShiftLeft
        | OperationType.ShiftRight
        | OperationType.Subtract -> raise(new System.NotImplementedException())
        | _ -> raise(new System.ArgumentException(op.ToString() + " is not a binary arithmetical operator"))

    let internal simplifyUnaryOperation op x y isChecked =
        match op with
        | OperationType.LogicalNeg -> ("(~{0})", 1)
        | OperationType.PostfixIncrement -> ("({0}++)", 1)
        | OperationType.PostfixDecrement -> ("({0}--)", 1)
        | OperationType.PrefixIncrement -> ("(++{0})", 1)
        | OperationType.PrefixDecrement -> ("(--{0})", 1)
        | OperationType.UnaryMinus -> ("(-{0})", 1)
        | OperationType.UnaryPlus -> ("(+{0})", 1)
        | _ -> raise(new System.ArgumentException(op.ToString() + " is not an unary arithmetical operator"))
