namespace VSharp.Core.Symbolic

open JetBrains.Decompiler.Ast
open VSharp.CSharpUtils
open VSharp.Core.Symbolic.Terms

[<AutoOpen>]
module internal Propositional =

// ------------------------------- Simplification of logical operations -------------------------------
    let internal x2b (x : obj) = x :?> bool

    let rec private checkConjuctionInside x y =
        match x with
        | Conjunction(_, y, _)
        | Conjunction(y, _, _) -> true
        | Conjunction(x', x'', _) -> checkConjuctionInside x' y || checkConjuctionInside x'' y
        | _ -> false

    let rec private checkDisjunctionInside x y =
        match x with
        | Disjunction(_, y, _)
        | Disjunction(y, _, _) -> true
        | Disjunction(x', x'', _) -> checkDisjunctionInside x' y || checkDisjunctionInside x'' y
        | _ -> false

    let rec private simplifyConnective operation opposite x y stopValue ignoreValue k =
        match x, y with
        | _, _ when x = y -> k x
        | Negation(y,_), _ ->
            if operation = OperationType.LogicalOr 
            then k Terms.MakeTrue
            else k Terms.MakeFalse
        | _, _ when  operation = OperationType.LogicalAnd ->
            match x, y with
            | x, y when checkDisjunctionInside x y -> k y
            | x, y when checkDisjunctionInside y x -> k x
            | x, y when checkConjuctionInside x y -> k x
            | x, y when checkConjuctionInside y x -> k y
            | Negation(x,_), y when checkDisjunctionInside x y -> k Terms.MakeFalse
            | x, Negation(y,_) when checkDisjunctionInside y x -> k Terms.MakeFalse
            | Negation(x,_), y when checkDisjunctionInside x (Negate y) -> Negate x |> fun x -> simplifyNegation x k
            | x, Negation(y,_) when checkDisjunctionInside y (Negate x) -> Negate y |> fun x -> simplifyNegation x k
            | Negation(x,_), y when checkConjuctionInside x (Negate y) -> k y //to do simpl Op
            | x, Negation(y,_) when checkConjuctionInside y (Negate x) -> k x //to do simpl Op
            | Negation(x,_), y when checkConjuctionInside y x -> k Terms.MakeFalse
            | x, Negation(y,_) when checkConjuctionInside x y -> k Terms.MakeFalse
            | _, _ -> k Terms.MakeFalse //to do simplify first scnd -> both
        | _, _ when operation = OperationType.LogicalOr -> 
            match x, y with
            | x, y when checkConjuctionInside x y -> k x //to do simpl Op
            | x, y when checkConjuctionInside y x -> k y //to do simpl Op
            | x, y when checkDisjunctionInside x y -> k x //to do simpl Op
            | x, y when checkDisjunctionInside y x -> k y //to do simpl Op
            | Negation(x, _), y when checkDisjunctionInside x y -> k Terms.MakeFalse
            | x, Negation(y,_) when checkDisjunctionInside y x -> k Terms.MakeFalse
            | Negation(x,_), y when checkDisjunctionInside x (Negate y) -> Negate x |> fun x -> simplifyNegation x k
            | x, Negation(y,_) when checkDisjunctionInside y (Negate x) -> Negate y |> fun x -> simplifyNegation x k
            | Negation(x,_), y when checkConjuctionInside x (Negate y) -> k y
            | x, Negation(y,_) when checkConjuctionInside y (Negate x) -> k x
            | Negation(x,_), y when checkConjuctionInside y x -> k Terms.MakeFalse
            | x, Negation(y,_) when checkConjuctionInside x y -> k Terms.MakeFalse
            | _, _ -> k Terms.MakeFalse //to do simplify first scnd -> both
        | _, _ -> Terms.MakeBinary operation x y false Bool |> k

    and private simplifyAnd x y k =
        simplifyConnective OperationType.LogicalAnd OperationType.LogicalOr x y false true k

    and private simplifyOr x y k =
        simplifyConnective OperationType.LogicalOr OperationType.LogicalAnd x y false true k

    and internal simplifyNegation x k =
        match x with
        | Union _ -> failwith "Unexpected symbolic union in boolean operation (negation)"
        | Concrete(b,t) -> not (x2b b) |> fun b' -> Concrete(b',t) |> k
        | Negation (x, _)  -> k x
        | Conjunction(x, y, _) -> simplifyNegation x (fun x' -> simplifyNegation y (fun y' -> simplifyOr x' y' k))
        | Disjunction(x, y, _) -> simplifyNegation x (fun x' -> simplifyNegation y (fun y' -> simplifyAnd x' y' k))
        | _ -> Terms.MakeUnary OperationType.LogicalNeg x false Bool |> k

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
        | OperationType.LogicalNeg -> simplifyNegation x k
        | _ -> raise(new System.ArgumentException(op.ToString() + " is not an unary logical operator"))

    let internal isLogicalOperation op =
        match op with
        | OperationType.LogicalAnd
        | OperationType.LogicalOr
        | OperationType.LogicalXor
        | OperationType.LogicalNeg -> true
        | _ -> false
