namespace VSharp.Core.Symbolic.Reduction

open JetBrains.Decompiler.Ast
open VSharp.CSharpUtils
open VSharp.Core.Symbolic

module internal Arithmetics =

// ------------------------------- Simplification of "+" -------------------------------

    let internal simplifyAddition x y isChecked t =
        match (x, y) with
        | (Concrete(x, typeOfX), Concrete(y, typeOfY)) ->
            if isChecked then
                Terms.MakeConcrete (Calculator.Add(x, y, t)) t
            else
                let success = ref true
                let result = Calculator.AddChecked(x, y, t, success)
                if !success then Terms.MakeConcrete result t
                else Bottom
        | (Concrete(x, typeOfX), _) when Calculator.IsZero(x) -> y
        | (_, Concrete(y, typeOfY)) when Calculator.IsZero(y) -> x
        | _ -> Terms.MakeBinary OperationType.Add x y isChecked (Types.FromPrimitiveDotNetType t)

// ------------------------------- General methods -------------------------------
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
        | _ -> raise(new System.ArgumentException(op.ToString() + " is not a binary arithmetic operator"))

    let internal simplifyUnaryOperation op x isChecked =
        match op with
        | OperationType.LogicalNeg
        | OperationType.PostfixIncrement
        | OperationType.PostfixDecrement
        | OperationType.PrefixIncrement
        | OperationType.PrefixDecrement
        | OperationType.UnaryMinus -> raise(new System.NotImplementedException())
        | OperationType.UnaryPlus -> x
        | _ -> raise(new System.ArgumentException(op.ToString() + " is not an unary arithmetic operator"))
