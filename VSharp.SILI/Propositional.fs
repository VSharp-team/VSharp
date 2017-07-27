namespace VSharp

open JetBrains.Decompiler.Ast
open VSharp.CSharpUtils
open VSharp.Terms

[<AutoOpen>]
module internal Propositional =

// ------------------------------- Simplification of logical operations -------------------------------

    let internal makeBin operation x y =
        match x, y with
        | Expression(Operator(op', false), list', _), Expression(Operator(op'', false), list'', _) when op' = operation && op'' = operation ->
            Terms.MakeNAry operation (List.append list' list'') false Bool
        | Expression(Operator(op', false), list', _), _ when list'.IsEmpty -> y
        | _, Expression(Operator(op', false), list', _) when list'.IsEmpty -> y
        | Expression(Operator(op', false), list', _), _ when op' = operation ->
            Terms.MakeNAry operation (y::list') false Bool
        | _, Expression(Operator(op', false), list', _) when op' = operation ->
            Terms.MakeNAry operation (x::list') false Bool
        | _ -> Terms.MakeNAry operation [x; y] false Bool


    let internal makeCoOpBinaryTerm x list listOp op =
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

    let internal shuffled list1 list2 =
        if List.length list1 <> List.length list2 then false
        else
            let s1 = System.Collections.Generic.HashSet<Term>(list1) in
            let s2 = System.Collections.Generic.HashSet<Term>(list2) in
            s1.SymmetricExceptWith(s2); Seq.isEmpty s1

    // Trying to simplify pairwise combinations of x- and y-operands.
    // For example, it tries to simplify (a + b) + (c + d) or (a * b) * (c * d)
    // by successively trying to combine (a * c), (a * d), (b * c) and (b * d).
    let internal simplifyPairwiseCombinations xs ys t operand simplify reduce matched unmatched =
        let initialYs = ys in

        let reduce t1 t2 = reduce (operand t1) (operand t2) in

        let rec combineOne x ys failed k =
            match ys with
            | [] -> k x failed
            | h :: tl ->
                simplify t (operand x) (operand h)
                    (fun x -> combineOne x tl failed k)
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
                    Cps.List.reducek reduce toReduce (operand >> matched)
            | x::xs ->
                combineOne x ys [] (fun res ys -> combine xs ys (res::acc))

        combine xs ys []

    let rec private simplifyConnective operation opposite stopValue ignoreValue x y k =
        let defaultCase () = makeBin operation x y |> k in
        match x, y with
        | Error _, _ -> k x
        | _, Error _ -> k y
        | Nop, _ -> raise(new System.ArgumentException(sprintf "Invalid left operand of %s!" (operation.ToString())))
        | _, Nop -> raise(new System.ArgumentException(sprintf "Invalid right operand of %s!" (operation.ToString())))
        | Union gvs1, Union gvs2 ->
            Cps.List.mapk
                (fun (g1, v1) k ->
                    Cps.List.mapk
                        (fun (g2, v2) k ->
                            simplifyConnective OperationType.LogicalAnd OperationType.LogicalOr Terms.MakeFalse Terms.MakeTrue g1 g2 (fun g ->
                            simplifyConnective operation opposite stopValue ignoreValue v1 v2 (withFst g >> k)))
                        gvs2 k)
                gvs1
                (fun gvss -> List.concat gvss |> Union |> k)
        | Terms.GuardedValues(gs, vs), _ ->
            Cps.List.mapk (simplifyConnective operation opposite stopValue ignoreValue y) vs (fun xys ->
            List.zip gs xys |> Union |> k)
        | _, Terms.GuardedValues(gs, vs) ->
            Cps.List.mapk (simplifyConnective operation opposite stopValue ignoreValue x) vs (fun xys ->
            List.zip gs xys |> Union |> k)
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
            simplifyPairwiseCombinations list summandsOfY Bool id (simplifyExtWithType op co stopValue ignoreValue false) (simplifyConnective op co stopValue ignoreValue) matched unmatched

    and simplifyCoOp op co stopValue ignoreValue x list y matched unmatched =
        match list, y with
        | [x], y -> simplifyExt op co stopValue ignoreValue x y matched unmatched
        // Co(... y ...) op y
        | _ when List.contains y list -> matched y
        // Co(... y ...) op !y
        | _, Negation(y,_) when List.contains y list -> matched (makeCoOpBinaryTerm (Negate y) (List.except [y] list) co op)
        // Co(... !y ...) op y
        | _ when List.contains (Negate y) list -> matched (makeCoOpBinaryTerm y (List.except [Negate y] list) co op)
        // Co(!x xs) op Co(x xs) -> ys
        | _, Expression(Operator(co', false), IntersectionExceptOne list ys, _)  when co' = co -> matched (MakeNAry co ys false Bool)
        // Co(list) op Co(shuffled list) -> x
        | _, Expression(Operator(co', false), ys, _)  when co' = co && shuffled list ys -> matched x
        // Co(...) op OP(...) -> pairwise
        | _, Expression(Operator(op', false), y', _) when op = op'->
            // Trying to simplify pairwise combinations of x- and y-summands
            simplifyPairwiseCombinations [x] y' Bool id (simplifyExtWithType op co stopValue ignoreValue false) (simplifyConnective op co stopValue ignoreValue) matched unmatched
        | _ -> unmatched ()

    and private simplifyOpToExpr x y op co stopValue ignoreValue matched unmatched =
        match x with
        | Expression(Operator(op', false), xs, _) when op = op'->
            simplifyOpOp op co stopValue ignoreValue x xs y matched unmatched
        | Expression(Operator(op', false), xs, _) when co = op'->
            simplifyCoOp op co stopValue ignoreValue x xs y matched unmatched
        | _ -> unmatched ()

    and internal simplifyAnd x y k =
        simplifyConnective OperationType.LogicalAnd OperationType.LogicalOr Terms.MakeFalse Terms.MakeTrue x y k

    and internal simplifyOr x y k =
        simplifyConnective OperationType.LogicalOr OperationType.LogicalAnd Terms.MakeTrue Terms.MakeFalse x y k

    and internal simplifyNegation x k =
        match x with
        | Error _ -> k x
        | Concrete(b, t) -> Concrete(not (b :?> bool), t) |> k
        | Negation(x, _)  -> k x
        | ConjunctionList(x, _) -> Cps.List.mapk simplifyNegation x (fun l -> MakeNAry OperationType.LogicalOr  l false Bool |> k)
        | DisjunctionList(x, _) -> Cps.List.mapk simplifyNegation x (fun l -> MakeNAry OperationType.LogicalAnd l false Bool |> k)
        | Terms.GuardedValues(gs, vs) ->
            Cps.List.mapk simplifyNegation vs (fun nvs ->
            List.zip gs nvs |> Union |> k)
        | _ -> Terms.MakeUnary OperationType.LogicalNeg x false Bool |> k

    and private simplifyExtWithType op co stopValue ignoreValue isChecked t x (y:Term) matched unmatched =
        simplifyExt op co stopValue ignoreValue x y matched unmatched

// ------------------------------- General functions -------------------------------

    let internal (!!) x =
        simplifyNegation x id

    let internal (&&&) x y =
        simplifyAnd x y id

    let internal (|||) x y =
        simplifyOr x y id

    let internal (===) x y =
        simplifyOr !!x y (fun x' -> simplifyOr x !!y (fun y' -> simplifyAnd x' y' id))

    let internal (!==) x y =
        !! (x === y)

    let internal (==>) x y =
        !!x ||| y

    let internal conjunction = function
        | SeqNode(x, xs) ->
            if Seq.isEmpty xs then x
            else Seq.fold (&&&) x xs
        | _ -> Terms.MakeTrue

    let internal disjunction = function
        | SeqNode(x, xs) ->
            if Seq.isEmpty xs then x
            else Seq.fold (|||) x xs
        | _ -> Terms.MakeFalse

    let internal simplifyBinaryConnective op x y k =
        match op with
        | OperationType.LogicalAnd -> simplifyAnd x y k
        | OperationType.LogicalOr -> simplifyOr x y k
        | OperationType.LogicalXor ->
            simplifyNegation x (fun x' -> simplifyNegation y (fun y' ->
            simplifyOr x' y' (fun x' -> simplifyOr x y (fun y' -> simplifyAnd x' y' k))))
        | OperationType.Equal -> simplifyOr !!x y (fun x' -> simplifyOr x !!y (fun y' -> simplifyAnd x' y' k))
        | OperationType.NotEqual -> simplifyOr !!x y (fun x' -> simplifyOr x !!y (fun y' -> simplifyAnd x' y' (fun res -> simplifyNegation res k)))
        | _ -> raise(new System.ArgumentException(op.ToString() + " is not a binary logical operator"))

    let internal simplifyUnaryConnective op x k =
        match op with
        | OperationType.LogicalNeg -> simplifyNegation x k
        | _ -> raise(new System.ArgumentException(op.ToString() + " is not an unary logical operator"))

    let internal isLogicalOperation op t1 t2 =
        Types.IsBool t1 && Types.IsBool t2 &&
        match op with
        | OperationType.LogicalAnd
        | OperationType.LogicalOr
        | OperationType.LogicalXor
        | OperationType.LogicalNeg
        | OperationType.Equal
        | OperationType.NotEqual -> true
        | _ -> false

    let internal isConditionalOperation op =
        match op with
        | OperationType.ConditionalAnd
        | OperationType.ConditionalOr -> true
        | _ -> false
