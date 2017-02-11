namespace VSharp.Core.Symbolic

open JetBrains.Decompiler.Ast
open VSharp.CSharpUtils
open VSharp.Core.Symbolic.Terms

module internal Propositional =

// ------------------------------- Simplification of logical operations -------------------------------
    let internal x2b (x : obj) = x :?> bool

    let internal simplifyConnective operation opposite x y stopValue ignoreValue =
        // TODO!
        Terms.MakeBinary operation x y false Bool

    let rec internal simplifyNegation x k =
        match x with
        | Union _ -> assert false 
        | Concrete(b,t) -> not (x2b b) |> fun b' -> Concrete(b',t) |> k
        | Negation (x, _)  -> k x
        | Conjunction(x, y, _) -> 
            simplifyNegation x 
                (fun x' -> simplifyNegation y 
                    (fun y' -> simplifyConnective OperationType.AssignmentLogicalOr OperationType.AssignmentLogicalAnd x' y' Terms False))
        | Disjunction(x, y, _) -> 
        | _ -> Terms.MakeUnary OperationType.Not x false Bool

// ------------------------------- Simplification of logical operations -------------------------------

    let internal ite condition thenValue elseValue =
        // TODO!
        assert(TypeOf thenValue = TypeOf elseValue)
        Expression(Cond, [condition; thenValue; elseValue], TypeOf thenValue)

// ------------------------------- General functions -------------------------------

    let internal (!!) x =
        simplifyNegation x

    let internal (&&&) x y =
        simplifyConnective OperationType.LogicalAnd OperationType.LogicalOr x y false true

    let internal (|||) x y =
        simplifyConnective OperationType.LogicalAnd OperationType.LogicalOr x y false true

    let internal simplifyBinaryOperation op x y =
        match op with
        | OperationType.LogicalAnd -> x &&& y
        | OperationType.LogicalOr -> x ||| y
        | OperationType.LogicalXor -> raise(new System.NotImplementedException())
        | _ -> raise(new System.ArgumentException(op.ToString() + " is not a binary logical operator"))

    let internal simplifyUnaryOperation op x =
        match op with
        | OperationType.LogicalNeg
        | OperationType.Not -> !!x
        | _ -> raise(new System.ArgumentException(op.ToString() + " is not an unary logical operator"))
