namespace VSharp

open JetBrains.Decompiler.Ast
open VSharp.CSharpUtils
open VSharp.Terms

[<AutoOpen>]
module internal Propositional =

// ------------------------------- Simplification of logical operations -------------------------------
    let internal x2b (x : obj) = x :?> bool

    let private makeOp op x y =
        MakeBinary op x y false Bool

    // Trying to simplify pairwise combinations of x- and y-operands.
    // For example, it tries to simplify (a + b) + (c + d) or (a * b) * (c * d)
    // by successively trying to combine (a * c), (a * d), (b * c) and (b * d).
    let internal simplifyPairwiseCombinations xs ys simplify reduce matched unmatched =
        let initialYs = ys

        let rec combineOne x ys failed k =
            match ys with
            | [] -> k x failed
            | y::ys' ->
                simplify x y
                    (fun x -> combineOne x ys' failed k)
                    (fun _ -> combineOne x ys' (y::failed) k)

        let rec combine xs ys acc =
            match xs with
            | [] ->
                // Here we traversed all xs, checking for something matched...
                if List.length ys = List.length initialYs then unmatched () // Nothing matched, the whole process is failed
                else
                    // Something matched, the work is done, just combining results together...
                    let toReduce = List.append (List.rev acc) ys in
                    // TODO: care about different types...
                    (toReduce |> List.reduce reduce) |> matched
            | x::xs ->
                combineOne x ys [] (fun res ys -> combine xs ys (res::acc))

        combine xs ys []

    let rec private simplifyConnective operation opposite x y stopValue ignoreValue k =
        let defaultCase () = 
            Terms.MakeBinary operation x y false Bool |> k
        in
        match x, y with
        | _ when y = ignoreValue -> k x
        | _ when x = ignoreValue -> k y
        | _ when y = stopValue -> k stopValue
        | _ when x = stopValue-> k stopValue
        | _ when x = y -> k x
        | Negation(x,_), _ when x = y -> k stopValue
        | _, Negation(y,_) when x = y -> k stopValue
        | Error _, _ -> k x
        | _, Error _ -> k y
        | Nop, _ -> raise(new System.ArgumentException(sprintf "Invalid left operand of %s!" (operation.ToString())))
        | _, Nop -> raise(new System.ArgumentException(sprintf "Invalid right operand of %s!" (operation.ToString())))
        | Expression _, Expression _ ->
            simplifyExpression x y operation opposite stopValue ignoreValue k (fun () -> 
            simplifyExpression y x operation opposite stopValue ignoreValue k defaultCase)
        | Expression _, _ -> simplifyExpression x y operation opposite stopValue ignoreValue k defaultCase
        |  _, Expression _ -> simplifyExpression y x operation opposite stopValue ignoreValue k defaultCase
        | Union _, _ -> failwith (sprintf "Unexpected symbolic union in boolean operation (%s)" (operation.ToString()))
        | _, Union _ -> failwith (sprintf "Unexpected symbolic union in boolean operation (%s)" (operation.ToString()))
        | _ -> Terms.MakeBinary operation x y false Bool |> k

    and private simplifyExpression x y op co stopValue ignoreValue matched unmatched =
        match x with
        | Expression(Operator(op', false), [a;b], _) when op = op'->
            simplifyOpOp x a b y op co stopValue ignoreValue matched unmatched
        | Expression(Operator(co', false), [a;b], _) when co = co'->
            simplifyCoOp x a b y op co stopValue ignoreValue matched unmatched
        | _ -> unmatched ()

    and simplifyOpOp x a b y op co stopValue ignoreValue matched unmatched =
        // simplifying (a op b) op y at this step
        match a, b, y with
        | _ when a = y -> matched x
        | _ when b = y -> matched x
        | a, b, Negation(y,_) when a = y -> matched stopValue
        | a, b, Negation(y,_) when b = y -> matched stopValue
        | Negation(a,_), _, _ when a = y -> matched stopValue
        | a, Negation(b,_), _ when b = y -> matched stopValue
        | _ -> 
            // Trying to simplify pairwise combinations of x- and y-summands
            let summandsOfY =
                match y with
                | Expression(Operator(op', false), [c;d], _) when op = op'-> [c; d]
                | _ -> [y]
            simplifyPairwiseCombinations [a; b] summandsOfY (simplifyExt op co stopValue ignoreValue) (simplifyOp op co stopValue ignoreValue id) matched unmatched

    and simplifyCoOp x a b y op co stopValue ignoreValue matched unmatched =
        // simplifying (a co b) op y at this step
        match a, b, y with
        | _ when a = y -> matched y
        | _ when b = y -> matched y
        | a, b, Negation(y,_) when a = y -> simplifyNegation a (fun a' -> simplifyConnective op co a' b stopValue ignoreValue matched)
        | a, b, Negation(y,_) when b = y -> simplifyNegation b (fun b' -> simplifyConnective op co a b' stopValue ignoreValue matched)
        | Negation(a,_), _, _ when a = y -> simplifyConnective op co b y stopValue ignoreValue matched
        | a, Negation(b,_), _ when b = y -> simplifyConnective op co a y stopValue ignoreValue matched
        | a, b, Expression(Operator(co', false), [c;d], _) when co' = co ->
            simplifyConnective op co a c stopValue ignoreValue (fun t1 ->
            simplifyConnective op co a d stopValue ignoreValue (fun t2 ->
            simplifyConnective op co b c stopValue ignoreValue (fun t3 ->
            simplifyConnective op co b d stopValue ignoreValue (fun t4 ->
            simplifyPairwiseCombinations [t1] [t2; t3; t4] (simplifyExt co op ignoreValue stopValue) (simplifyOp co op ignoreValue stopValue id) matched unmatched))))
        | _ -> 
            simplifyConnective op co a y stopValue ignoreValue (fun t1 ->
            simplifyConnective op co b y stopValue ignoreValue (fun t2 ->
            simplifyPairwiseCombinations [t1] [t2] (simplifyExt co op ignoreValue stopValue) (simplifyOp co op ignoreValue stopValue id) matched unmatched))

    and private simplifyExt op co stopValue ignoreValue x y matched unmatched =
        match x, y with
        | _ when y = ignoreValue -> matched x
        | _ when x = ignoreValue -> matched y
        | _ when y = stopValue -> matched stopValue
        | _ when x = stopValue-> matched stopValue
        | _ when x = y -> matched x
        | Negation(x,_), _ when x = y -> matched stopValue
        | _, Negation(y,_) when x = y -> matched stopValue
        | Expression _, Expression _ ->
            simplifyOpToExpr x y op co stopValue ignoreValue matched (fun () ->
            simplifyOpToExpr y x op co stopValue ignoreValue matched unmatched)
        | Expression _, _ -> simplifyOpToExpr x y op co stopValue ignoreValue matched unmatched
        | _, Expression _  -> simplifyOpToExpr y x op co stopValue ignoreValue matched unmatched
        | _ -> unmatched ()

    and private simplifyOpToExpr x y op co stopValue ignoreValue matched unmatched =
        match x with 
        | Expression(Operator(op', false), [a;b], _) when op = op'->
            simplifyOpOp x a b y op co stopValue ignoreValue matched unmatched
        // TO DO: x to expr
        | _ -> unmatched ()

    and private simplifyAnd x y k =
        simplifyConnective OperationType.LogicalAnd OperationType.LogicalOr x y Terms.MakeFalse Terms.MakeTrue k

    and private simplifyOr x y k =
        simplifyConnective OperationType.LogicalOr OperationType.LogicalAnd x y Terms.MakeTrue Terms.MakeFalse k

    and internal simplifyNegation x k =
        match x with
        | Concrete(b,t) -> not (x2b b) |> fun b' -> Concrete(b',t) |> k
        | Negation(x, _)  -> k x
        | Conjunction(x, y, _) -> simplifyNegation x (fun x' -> simplifyNegation y (fun y' -> simplifyOr x' y' k))
        | Disjunction(x, y, _) -> simplifyNegation x (fun x' -> simplifyNegation y (fun y' -> simplifyAnd x' y' k))
        | Union _ -> failwith "Unexpected symbolic union in boolean operation (negation)"
        | _ -> Terms.MakeUnary OperationType.LogicalNeg x false Bool |> k

    and private simplifyOp op co stopValue ignoreValue k x y =
        simplifyConnective op co stopValue ignoreValue x y k

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
