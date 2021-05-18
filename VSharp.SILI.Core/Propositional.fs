namespace VSharp.Core

open VSharp

type IPropositionalSimplifier =
    abstract member Simplify : term -> term

[<AutoOpen>]
module internal Propositional =

    let mutable private simplifier : IPropositionalSimplifier option = None
    let configureSimplifier s = simplifier <- Some s

// ------------------------------- Utilities -------------------------------

    let makeBin operation x y =
        match x.term, y.term with
        | Expression(Operator op', list', _), Expression(Operator op'', list'', _) when op' = operation && op'' = operation ->
            makeNAry operation (List.append list' list'') Bool
        | Expression(Operator _, [], _), _ -> y
        | _, Expression(Operator _, [], _) -> y
        | Expression(Operator op', list', _), _ when op' = operation ->
            makeNAry operation (y::list') Bool
        | _, Expression(Operator op', list', _) when op' = operation ->
            makeNAry operation (x::list') Bool
        | _ -> makeNAry operation [x; y] Bool


    let private makeCoOpBinaryTerm x list listOp op =
        match list with
        | [] -> x
        | [y] -> makeBin op x y
        | _ -> makeBin op (Expression (Operator listOp) list Bool) x


    let private (|IntersectionExceptOneNegation|_|) (list1 : term list) (list2 : term list) =
        let s1 = System.Collections.Generic.HashSet<term>(list1)
        let s2 = System.Collections.Generic.HashSet<term>(list2)
        let intersection = list2 |> Seq.fold (fun acc x -> if s1.Remove(x) then s2.Remove(x) |> ignore; x::acc else acc) []
        if s1.Count <> 1 then None
        else
            match Seq.head s1 with
            | NegationT y as x when s2.RemoveWhere(System.Predicate<term>((=)y)) > 0 -> Some(x, intersection, List.ofSeq s2)
            | x when s2.RemoveWhere(System.Predicate<term>(function | NegationT y when x = y -> true | _ -> false)) > 0 -> Some(x, intersection, List.ofSeq s2)
            | _ -> None

    let private isPermutationOf list1 list2 =
        if List.length list1 <> List.length list2 then false
        else
            let s1 = System.Collections.Generic.HashSet<term>(list1)
            let s2 = System.Collections.Generic.HashSet<term>(list2)
            s1.SymmetricExceptWith(s2); Seq.isEmpty s1

// ------------------------------- Simplification of logical operations -------------------------------

    // Trying to simplify pairwise combinations of x- and y-operands.
    // For example, it tries to simplify (a + b) + (c + d) or (a * b) * (c * d)
    // by successively trying to combine (a * c), (a * d), (b * c) and (b * d).
    let simplifyPairwiseCombinations xs ys t operand simplify reduce matched unmatched =
        let initialYs = ys

        let reduce t1 t2 = reduce (operand t1) (operand t2)

        let rec combineOne x ys failed k =
            match ys with
            | [] -> k x failed
            | h :: tl ->
                simplify t (operand x) (operand h)
                    (fun x -> combineOne x tl failed k)
                    (fun () -> combineOne x tl (h::failed) k)

        let rec combine xs ys acc k =
            match xs with
            | [] ->
                // Here we traversed all xs, checking for something matched...
                if List.length ys = List.length initialYs then unmatched () |> k // Nothing matched, the whole process is failed
                else
                    // Something matched, the work is done, just combining results together...
                    let toReduce = List.append (List.rev acc) ys
                    // TODO: care about different types...
                    Cps.List.reducek reduce toReduce (operand >> matched >> k)
            | x::xs ->
                combineOne x ys [] (fun res ys ->
                combine xs ys (res::acc) k)

        combine xs ys [] id

    let rec private simplifyConnective operation opposite stopValue ignoreValue x y k =
        let defaultCase () = makeBin operation x y |> k in
        match simplifier with
        | Some simplifier -> simplifier.Simplify(makeBin operation x y) |> k
        | None ->
            match x.term, y.term with
            | Nop, _ -> internalfailf "Invalid left operand of %O!" operation
            | _, Nop -> internalfailf "Invalid right operand of %O!" operation
            | Union gvs1, Union gvs2 ->
                Cps.List.mapk
                    (fun (g1, v1) k ->
                        Cps.List.mapk
                            (fun (g2, v2) k ->
                                simplifyAnd g1 g2 (fun g ->
                                simplifyConnective operation opposite stopValue ignoreValue v1 v2 (withFst g >> k)))
                            gvs2 k)
                    gvs1
                    (List.concat >> Union >> k)
            | GuardedValues(gs, vs), _ ->
                Cps.List.mapk (simplifyConnective operation opposite stopValue ignoreValue y) vs (fun xys ->
                List.zip gs xys |> Union |> k)
            | _, GuardedValues(gs, vs) ->
                Cps.List.mapk (simplifyConnective operation opposite stopValue ignoreValue x) vs (fun xys ->
                List.zip gs xys |> Union |> k)
            | _ -> simplifyExt operation opposite stopValue ignoreValue x y k defaultCase

    and private simplifyExt op co stopValue ignoreValue x y matched unmatched =
        match x.term, y.term with
        | _ when y = ignoreValue -> matched x
        | _ when x = ignoreValue -> matched y
        | _ when y = stopValue -> matched stopValue
        | _ when x = stopValue-> matched stopValue
        | _ when x = y -> matched x
        | Negation x, _ when x = y -> matched stopValue
        | _, Negation y when x = y -> matched stopValue
        | Expression _, Expression _ ->
            simplifyExpression op co stopValue ignoreValue x y matched (fun () ->
            simplifyExpression op co stopValue ignoreValue y x matched unmatched)
        | Expression _, _ -> simplifyOpToExpr x y op co stopValue ignoreValue matched unmatched
        | _, Expression _ -> simplifyOpToExpr y x op co stopValue ignoreValue matched unmatched
        | _ -> unmatched ()

    and private simplifyExpression op co stopValue ignoreValue x y matched unmatched =
        match x.term with
        | Expression(Operator op', list, _) when op = op'->
            simplifyOpOp op co stopValue ignoreValue list y matched unmatched
        | Expression(Operator co', list, _) when co = co'->
            simplifyCoOp op co stopValue ignoreValue x list y matched unmatched
        | _ -> unmatched ()

    and simplifyOpOp op co stopValue ignoreValue xargs y matched unmatched =
        // simplifying (OP list) op y at this step
        match xargs, y with
        | [x], y -> simplifyExt op co stopValue ignoreValue x y matched unmatched
        | _ -> unmatched ()
            // Trying to simplify pairwise combinations of x- and y-summands
//            let yargs =
//                match y.term with
//                | Expression(Operator op', y', _) when op = op'-> y'
//                | _ -> [y]
//            simplifyPairwiseCombinations xargs yargs Bool id (simplifyExtWithType op co stopValue ignoreValue) (simplifyConnective op co stopValue ignoreValue) matched unmatched

    and simplifyCoOp op co stopValue ignoreValue x list y matched unmatched =
        match list, y.term with
        | [x], _ -> simplifyExt op co stopValue ignoreValue x y matched unmatched
        // Co(... y ...) op y = y
        | _ when List.contains y list -> matched y
        // Co(... y ...) op !y = Co(... ...) op !y
        | _, Negation y' when List.contains y' list -> matched (makeCoOpBinaryTerm y (List.except [y'] list) co op)
        // Co(... !y ...) op y = Co(... ...) op y
        | _ when List.contains (negate y) list -> matched (makeCoOpBinaryTerm y (List.except [negate y] list) co op)
        // Co(!a ys) op Co(a ys) = ys
        // Co(a ys) op Co(!a ys zs) = ys co (a op Co(zs)) if (a op Co(zs)) simplifies
        // Co(a ys) op Co(!a ys zs) = Co(a ys) op Co(ys zs)
        | _, Expression(Operator co', IntersectionExceptOneNegation list (a, ys, zs), _) when co' = co ->
            if zs.IsEmpty then makeNAry co ys Bool |> matched
            else
                let coZs = makeNAry co zs Bool
                simplifyExt op co stopValue ignoreValue a coZs
                    (fun aOpZs -> makeNAry co (aOpZs::ys) Bool |> matched)
                    (fun () ->
                        let y' = makeNAry co (List.append ys zs) Bool
                        simplifyCoOp op co stopValue ignoreValue x list y' matched (fun () ->
                        makeNAry op [x; y'] Bool |> matched))
        // Co(list) op Co(permutation of list) -> Co(list)
        // TODO: sort terms to avoid permutation checking
        | _, Expression(Operator co', ys, _)  when co' = co && isPermutationOf list ys -> matched x
        // Co(...) op OP(...) -> pairwise
        | _, Expression(Operator op', y', _) when op = op' ->
            // Trying to simplify pairwise combinations of x- and y-summands
//            simplifyPairwiseCombinations [x] y' Bool id (simplifyExtWithType op co stopValue ignoreValue) (simplifyConnective op co stopValue ignoreValue) matched unmatched
            unmatched ()
        | _ -> unmatched ()

    and private simplifyOpToExpr x y op co stopValue ignoreValue matched unmatched =
        match x.term with
        | Expression(Operator op', xs, _) when op = op'->
            simplifyOpOp op co stopValue ignoreValue xs y matched unmatched
        | Expression(Operator op', xs, _) when co = op'->
            simplifyCoOp op co stopValue ignoreValue x xs y matched unmatched
        | _ -> unmatched ()

    and simplifyAnd x y k =
        simplifyConnective OperationType.LogicalAnd OperationType.LogicalOr False True x y k

    and simplifyOr x y k =
        simplifyConnective OperationType.LogicalOr OperationType.LogicalAnd True False x y k

    and internal simplifyNegation x k =
        match simplifier with
        | Some simplifier -> simplifier.Simplify(makeUnary OperationType.LogicalNot x Bool) |> k
        | None ->
            match x.term with
            | Concrete(b, t) -> Concrete (not (b :?> bool)) t |> k
            | Negation x -> k x
            | Conjunction xs -> Cps.List.mapk simplifyNegation xs (fun l -> makeNAry OperationType.LogicalOr l Bool |> k)
            | Disjunction xs -> Cps.List.mapk simplifyNegation xs (fun l -> makeNAry OperationType.LogicalAnd l Bool |> k)
            | Terms.GuardedValues(gs, vs) ->
                Cps.List.mapk simplifyNegation vs (List.zip gs >> Union >> k)
            | _ -> makeUnary OperationType.LogicalNot x Bool |> k

    and private simplifyExtWithType op co stopValue ignoreValue _ x y matched unmatched =
        simplifyExt op co stopValue ignoreValue x y matched unmatched

// ------------------------------- General functions -------------------------------

    let (!!) x =
        simplifyNegation x id

    let (&&&) x y =
        simplifyAnd x y id

    let (|||) x y =
        simplifyOr x y id

    let eq x y =
        simplifyOr !!x y (fun x' -> simplifyOr x !!y (fun y' -> simplifyAnd x' y' id))

    let neq x y =
        !! (eq x y)

    let implies x y =
        simplifyNegation x (fun notX ->
        simplifyOr notX y id)

    let lazyAnd x y k =
        match x with
        | False -> x
        | _ -> simplifyAnd x (y()) k

    let lazyOr x y k =
        match x with
        | False -> x
        | _ -> simplifyOr x (y()) k

    let conjunction = function
        | Seq.Cons(x, xs) ->
            if Seq.isEmpty xs then x
            else Seq.fold (&&&) x xs
        | _ -> True

    let disjunction = function
        | Seq.Cons(x, xs) ->
            if Seq.isEmpty xs then x
            else Seq.fold (|||) x xs
        | _ -> False

    let lazyConjunction xs =
        Cps.Seq.foldlk lazyAnd True xs id

    let lazyDisjunction xs k =
        Cps.Seq.foldlk lazyOr False xs k

    let simplifyBinaryConnective op x y k =
        match op with
        | LogicalAnd -> simplifyAnd x y k
        | LogicalOr -> simplifyOr x y k
        | LogicalXor ->
            simplifyNegation x (fun x' -> simplifyNegation y (fun y' ->
            simplifyOr x' y' (fun x' -> simplifyOr x y (fun y' -> simplifyAnd x' y' k))))
        | Equal -> simplifyOr !!x y (fun x' -> simplifyOr x !!y (fun y' -> simplifyAnd x' y' k))
        | NotEqual -> simplifyOr !!x y (fun x' -> simplifyOr x !!y (fun y' -> simplifyAnd x' y' (fun res -> simplifyNegation res k)))
        | _ -> internalfailf "%O is not a binary logical operator" op

    let simplifyUnaryConnective op x k =
        match op with
        | LogicalNot -> simplifyNegation x k
        | _ -> internalfailf "%O is not an unary logical operator" op

    let isLogicalOperation op t1 t2 =
        Types.isBool t1 && Types.isBool t2 &&
        match op with
        | LogicalAnd
        | LogicalOr
        | LogicalXor
        | LogicalNot
        | Equal
        | NotEqual -> true
        | _ -> false
