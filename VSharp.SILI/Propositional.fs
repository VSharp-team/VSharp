namespace VSharp

open JetBrains.Decompiler.Ast
open VSharp.CSharpUtils
open VSharp.Terms

[<AutoOpen>]
module internal Propositional =

// ------------------------------- Simplification of logical operations -------------------------------

    let private simplifyNegation x k =
        // TODO!
        Terms.MakeUnary OperationType.Not x false Bool |> k

    let private simplifyConnective operation opposite x y stopValue ignoreValue k =
        // TODO!
        Terms.MakeBinary operation x y false Bool |> k

    let private simplifyAnd x y k =
        simplifyConnective OperationType.LogicalAnd OperationType.LogicalOr x y false true k

    let private simplifyOr x y k =
        simplifyConnective OperationType.LogicalOr OperationType.LogicalAnd x y false true k

// ------------------------------- Simplification of logical operations -------------------------------

    let private ite condition thenValue elseValue =
        // TODO!
        assert(TypeOf thenValue = TypeOf elseValue) // Should we infer here most general type?
        Expression(Cond, [condition; thenValue; elseValue], TypeOf thenValue)

// ------------------------------- General functions -------------------------------

    let internal (!!) x =
        simplifyNegation x id

    let internal (&&&) x y =
        simplifyAnd x y id

    let internal (|||) x y =
        simplifyOr x y id

    let internal simplifyBinaryConnective op x y k =
        match op with
        | OperationType.LogicalAnd -> simplifyAnd x y k
        | OperationType.LogicalOr -> simplifyOr x y k
        | OperationType.LogicalXor -> raise(new System.NotImplementedException())
        | _ -> raise(new System.ArgumentException(op.ToString() + " is not a binary logical operator"))

    let internal simplifyUnaryConnective op x k =
        match op with
        | OperationType.LogicalNeg
        | OperationType.Not -> simplifyNegation x k
        | _ -> raise(new System.ArgumentException(op.ToString() + " is not an unary logical operator"))

    let internal isLogicalOperation op =
        match op with
        | OperationType.LogicalAnd
        | OperationType.LogicalOr
        | OperationType.LogicalXor
        | OperationType.LogicalNeg
        | OperationType.Not -> true
        | _ -> false
