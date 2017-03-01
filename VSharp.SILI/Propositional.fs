namespace VSharp

open JetBrains.Decompiler.Ast
open VSharp.CSharpUtils
open VSharp.Terms

[<AutoOpen>]
module internal Propositional =

// ------------------------------- Simplification of logical operations -------------------------------
    let internal x2b (x : obj) = x :?> bool

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
            simplifyExpression x y op co stopValue ignoreValue matched (fun () ->
            simplifyExpression y x op co stopValue ignoreValue matched unmatched)
        | Expression _, _ -> simplifyOpToExpr x y op co stopValue ignoreValue matched unmatched
        | _, Expression _  -> simplifyOpToExpr y x op co stopValue ignoreValue matched unmatched
        | _ -> unmatched ()

    and private simplifyExpression x y op co stopValue ignoreValue matched unmatched =
        match x with
        | Expression(Operator(op', false), list, _) when op = op'->
            simplifyOpOp x list y op co stopValue ignoreValue matched unmatched
        | Expression(Operator(co', false), list, _) when co = co'->
            simplifyCoOp x list y op co stopValue ignoreValue matched unmatched
        | _ -> unmatched ()

    and simplifyOpOp x list y op co stopValue ignoreValue matched unmatched =
        // simplifying (OP list) op y at this step
        match list, y with
        | [x], y -> simplifyExt op co stopValue ignoreValue x y matched unmatched
        | _ -> 
            // Trying to simplify pairwise combinations of x- and y-summands
            let summandsOfY =
                match y with
                | Expression(Operator(op', false), y', _) when op = op'-> y'
                | _ -> [y]
            simplifyPairwiseCombinations list summandsOfY (simplifyExt op co stopValue ignoreValue) (simplifyOp op co stopValue ignoreValue id) matched unmatched

    and simplifyCoOp x list y op co stopValue ignoreValue matched unmatched =
        match list, y with
        | [x], y -> simplifyExt op co stopValue ignoreValue x y matched unmatched
        // Co(... y ...) op y
        | _ when List.contains y list -> matched y
        // Co(... y ...) op !y
        | _, Negation(y,_) when List.contains y list -> matched (makeBinaryTerm (Negate y) (List.except [y] list) co op)
        // Co(... !y ...) op y
        | _ when 
            let noty = (Negate y) in
            List.contains noty list -> matched (makeBinaryTerm y (List.except [Negate y] list) co op)
        // Co(!x xs) op Co(x xs) -> ys
        | _, Expression(Operator(co', false), IntersectionExceptOne list ys, _)  when co' = co-> matched (MakeBinaryOverList co ys false Bool)
        // Co(...) op OP(...) -> pairwise
        | _, Expression(Operator(op', false), y', _) when op = op'->
            // Trying to simplify pairwise combinations of x- and y-summands
            simplifyPairwiseCombinations [x] y' (simplifyExt op co stopValue ignoreValue) (simplifyOp op co stopValue ignoreValue id) matched unmatched
        | _ -> unmatched ()

    and private simplifyOpToExpr x y op co stopValue ignoreValue matched unmatched =
        match x with 
        | Expression(Operator(op', false), xs, _) when op = op'->
            simplifyOpOp x xs y op co stopValue ignoreValue matched unmatched
        | Expression(Operator(op', false), xs, _) when co = op'->
            simplifyCoOp x xs y op co stopValue ignoreValue matched unmatched
        | _ -> unmatched ()

    and private simplifyAnd x y k =
//        printfn "\nget %s AND %s" (toString x) (toString y)
//        let k res = printf "ret %s \n"  (toString res); k res
        simplifyConnective OperationType.LogicalAnd OperationType.LogicalOr x y Terms.MakeFalse Terms.MakeTrue k

    and private simplifyOr x y k =
//        printfn "\n get %s OR %s" (toString x) (toString y)
//        let k res = printf "ret %s \n" (toString res); k res
        simplifyConnective OperationType.LogicalOr OperationType.LogicalAnd x y Terms.MakeTrue Terms.MakeFalse k

    and internal simplifyNegation x k =
//        let k res = printf "\nget NOT %s \n ret %s \n" (toString x) (toString res); k res
        match x with
        | Concrete(b, t) -> not (x2b b) |> fun b' -> Concrete(b',t) |> k
        | Negation(x, _)  -> k x
        | ConjunctionList(x, _) -> Cps.List.mapk simplifyNegation x (fun l -> MakeBinaryOverList OperationType.LogicalOr l false Bool |> k)
        | DisjunctionList(x, _) -> Cps.List.mapk simplifyNegation x (fun l -> MakeBinaryOverList OperationType.LogicalAnd l false Bool |> k)
        | Union _ -> failwith "Unexpected symbolic union in boolean operation (negation)"
        | _ -> Terms.MakeUnary OperationType.LogicalNeg x false Bool |> k

    and private simplifyOp op co stopValue ignoreValue k x y =
        simplifyConnective op co x y stopValue ignoreValue k

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
