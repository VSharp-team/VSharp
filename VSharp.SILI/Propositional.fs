namespace VSharp

open JetBrains.Decompiler.Ast
open VSharp.CSharpUtils
open VSharp.Terms

[<AutoOpen>]
module internal Propositional =

// ------------------------------- Simplification of logical operations -------------------------------
    let internal makeBin op x y = MakeBinary op x y false Bool

    let internal makeBinaryTerm x list listOp op = 
        match list with
        | [] -> x
        | [y] -> makeBin op x y
        | _ -> makeBin op (Expression(Operator(listOp, false), list, Bool)) x

    let public (|IntersectionExceptOne|_|) list1 list2 = 
        if List.length list1 = List.length list2 then
            let s1 = System.Collections.Generic.HashSet<Term>(list1) in
            let s2 = System.Collections.Generic.HashSet<Term>(list2) in
            s1.SymmetricExceptWith(s2)
            let symmetricExcept = List.ofSeq s1 in 
            match symmetricExcept with 
            | [x; Negation(y,_)] when x = y -> s2.ExceptWith(s1); Some(List.ofSeq s2)
            | [Negation(y,_); x] when x = y -> s2.ExceptWith(s1); Some(List.ofSeq s2)
            | _ -> None
        else None

    // Trying to simplify pairwise combinations of x- and y-operands.
    // For example, it tries to simplify (a + b) + (c + d) or (a * b) * (c * d)
    // by successively trying to combine (a * c), (a * d), (b * c) and (b * d).
    let internal simplifyPairwiseCombinations xs ys t simplify reduce matched unmatched =
        let initialYs = ys

        let rec combineOne x ys failed k =
            match ys with
            | [] -> k x failed
            | h :: tl ->
                simplify x h false t
                    (fun x  -> combineOne x tl failed k)
                    (fun () -> combineOne x tl (h::failed) k)

        let rec combine xs ys acc =
            match xs with
            | [] ->
                // Here we traversed all xs, checking for something matched...
                if List.length ys = List.length initialYs then unmatched () // Nothing matched, the whole process is failed
                else
                    // Something matched, the work is done, just combining results together...
                    let toReduce = List.append (List.rev acc) ys in
                    // TODO: care about different types...
                    Cps.List.reducek reduce toReduce matched
            | x::xs ->
                combineOne x ys [] (fun res ys -> combine xs ys (res::acc))

        combine xs ys []

    let rec private simplifyConnective operation opposite stopValue ignoreValue x y k =
        let defaultCase () = 
            match x, y with
            | Expression(Operator(op', false), list', _), Expression(Operator(op'', false), list'', _) when op' = operation && op'' = operation ->
                Terms.MakeBinaryOverList operation (List.append list' list'') false Bool |> k
            | Expression(Operator(op', false), list', _), Constant _ when op' = operation ->
                Terms.MakeBinaryOverList operation (y::list') false Bool |> k
            | Expression(Operator(op', false), list', _), Negation _ when op' = operation ->
                Terms.MakeBinaryOverList operation (y::list') false Bool |> k
            | Constant _, Expression(Operator(op', false), list', _) when op' = operation ->
                Terms.MakeBinaryOverList operation (x::list') false Bool |> k
            | Negation _, Expression(Operator(op', false), list', _) when op' = operation ->
                Terms.MakeBinaryOverList operation (x::list') false Bool |> k
            | Expression(Operator(op', false), list', _), _ when list'.IsEmpty -> k y
            | _, Expression(Operator(op', false), list', _) when list'.IsEmpty -> k y
            | _ -> Terms.MakeBinaryOverList operation [x; y] false Bool |> k

        match x, y with
        | Error _, _ -> k x
        | _, Error _ -> k y
        | Nop, _ -> raise(new System.ArgumentException(sprintf "Invalid left operand of %s!" (operation.ToString())))
        | _, Nop -> raise(new System.ArgumentException(sprintf "Invalid right operand of %s!" (operation.ToString())))
        | Union _, _ -> failwith (sprintf "Unexpected symbolic union in boolean operation (%s)" (operation.ToString()))
        | _, Union _ -> failwith (sprintf "Unexpected symbolic union in boolean operation (%s)" (operation.ToString()))
        | _ -> simplifyExt operation opposite stopValue ignoreValue x y k defaultCase

    and private simplifyExt op co stopValue ignoreValue x y matched unmatched =
        match x, y with
        | _ when y = ignoreValue -> matched x
        | _ when x = ignoreValue -> matched y
        | _ when y = stopValue -> matched stopValue
        | _ when x = stopValue-> matched stopValue
        | _ when x = y -> matched x
        | Negation(x, _), _ when x = y -> matched stopValue
        | _, Negation(y, _) when x = y -> matched stopValue
        | Expression _, Expression _ ->
            simplifyExpression op co stopValue ignoreValue x y matched (fun () ->
            simplifyExpression op co stopValue ignoreValue y x matched unmatched)
        | Expression _, _ -> simplifyOpToExpr x y op co stopValue ignoreValue matched unmatched
        | _, Expression _  -> simplifyOpToExpr y x op co stopValue ignoreValue matched unmatched
        | _ -> unmatched ()

    and private simplifyExpression op co stopValue ignoreValue x y matched unmatched =
        match x with
        | Expression(Operator(op', false), list, _) when op = op'->
            simplifyOpOp op co stopValue ignoreValue x list y matched unmatched
        | Expression(Operator(co', false), list, _) when co = co'->
            simplifyCoOp op co stopValue ignoreValue x list y matched unmatched
        | _ -> unmatched ()

    and simplifyOpOp op co stopValue ignoreValue x list y matched unmatched =
        // simplifying (OP list) op y at this step
        match list, y with
        | [x], y -> simplifyExt op co stopValue ignoreValue x y matched unmatched
        | _ -> 
            // Trying to simplify pairwise combinations of x- and y-summands
            let summandsOfY =
                match y with
                | Expression(Operator(op', false), y', _) when op = op'-> y'
                | _ -> [y]
            simplifyPairwiseCombinations list summandsOfY Bool (simplifyExtWithType op co stopValue ignoreValue) (simplifyConnective op co stopValue ignoreValue) matched unmatched

    and simplifyCoOp op co stopValue ignoreValue x list y matched unmatched =
        match list, y with
        | [x], y -> simplifyExt op co stopValue ignoreValue x y matched unmatched
        // Co(... y ...) op y
        | _ when List.contains y list -> matched y
        // Co(... y ...) op !y
        | _, Negation(y,_) when List.contains y list -> matched (makeBinaryTerm (Negate y) (List.except [y] list) co op)
        // Co(... !y ...) op y
        | _ when List.contains (Negate y) list -> matched (makeBinaryTerm y (List.except [Negate y] list) co op)
        // Co(!x xs) op Co(x xs) -> ys
        | _, Expression(Operator(co', false), IntersectionExceptOne list ys, _)  when co' = co-> matched (MakeBinaryOverList co ys false Bool)
        // Co(...) op OP(...) -> pairwise
        | _, Expression(Operator(op', false), y', _) when op = op'->
            // Trying to simplify pairwise combinations of x- and y-summands
            simplifyPairwiseCombinations [x] y' Bool (simplifyExtWithType op co stopValue ignoreValue) (simplifyConnective op co stopValue ignoreValue) matched unmatched
        | _ -> unmatched ()

    and private simplifyOpToExpr x y op co stopValue ignoreValue matched unmatched =
        match x with 
        | Expression(Operator(op', false), xs, _) when op = op'->
            simplifyOpOp op co stopValue ignoreValue x xs y matched unmatched
        | Expression(Operator(op', false), xs, _) when co = op'->
            simplifyCoOp op co stopValue ignoreValue x xs y matched unmatched
        | _ -> unmatched ()

    and private simplifyAnd x y k =
        simplifyConnective OperationType.LogicalAnd OperationType.LogicalOr Terms.MakeFalse Terms.MakeTrue x y k

    and private simplifyOr x y k =
        simplifyConnective OperationType.LogicalOr OperationType.LogicalAnd Terms.MakeTrue Terms.MakeFalse x y k

    and internal simplifyNegation x k =
        match x with
        | Concrete(b, t) -> Concrete(not (b :?> bool),t) |> k
        | Negation(x, _)  -> k x
        | ConjunctionList(x, _) -> Cps.List.mapk simplifyNegation x (fun l -> MakeBinaryOverList OperationType.LogicalOr l false Bool |> k)
        | DisjunctionList(x, _) -> Cps.List.mapk simplifyNegation x (fun l -> MakeBinaryOverList OperationType.LogicalAnd l false Bool |> k)
        | Union _ -> failwith "Unexpected symbolic union in boolean operation (negation)"
        | _ -> Terms.MakeUnary OperationType.LogicalNeg x false Bool |> k

    and private simplifyExtWithType op co stopValue ignoreValue x y isChecked t matched unmatched =
        simplifyExt op co stopValue ignoreValue x y matched unmatched

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
