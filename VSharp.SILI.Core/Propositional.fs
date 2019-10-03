namespace VSharp.Core

open VSharp

type IPropositionalSimplifier =
    abstract member Simplify : term -> term

[<AutoOpen>]
module internal Propositional =

    let mutable private simplifier : IPropositionalSimplifier option = None
    let configureSimplifier s = simplifier <- Some s

// ------------------------------- Utilities -------------------------------

    let makeBin metadata operation x y =
        match x.term, y.term with
        | Expression(Operator(op', false), list', _), Expression(Operator(op'', false), list'', _) when op' = operation && op'' = operation ->
            makeNAry operation (List.append list' list'') false Bool metadata
        | Expression(Operator(_, false), [], _), _ -> y
        | _, Expression(Operator(_, false), [], _) -> y
        | Expression(Operator(op', false), list', _), _ when op' = operation ->
            makeNAry operation (y::list') false Bool metadata
        | _, Expression(Operator(op', false), list', _) when op' = operation ->
            makeNAry operation (x::list') false Bool metadata
        | _ -> makeNAry operation [x; y] false Bool metadata


    let private makeCoOpBinaryTerm metadata listMetadata x list listOp op =
        match list with
        | [] -> x
        | [y] -> makeBin metadata op x y
        | _ -> makeBin metadata op (Expression listMetadata (Operator(listOp, false)) list Bool) x


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

        let rec combine xs ys acc =
            match xs with
            | [] ->
                // Here we traversed all xs, checking for something matched...
                if List.length ys = List.length initialYs then unmatched () // Nothing matched, the whole process is failed
                else
                    // Something matched, the work is done, just combining results together...
                    let toReduce = List.append (List.rev acc) ys
                    // TODO: care about different types...
                    Cps.List.reducek reduce toReduce (operand >> matched)
            | x::xs ->
                combineOne x ys [] (fun res ys -> combine xs ys (res::acc))

        combine xs ys []

    let rec private simplifyConnective mtd operation opposite stopValue ignoreValue x y k =
        let defaultCase () = makeBin mtd operation x y |> k in
        match simplifier with
        | Some simplifier -> simplifier.Simplify(makeBin mtd operation x y) |> k
        | None ->
            match x.term, y.term with
            | Error _, _ -> k x
            | _, Error _ -> k y
            | Nop, _ -> internalfailf "Invalid left operand of %O!" operation
            | _, Nop -> internalfailf "Invalid right operand of %O!" operation
            | Union gvs1, Union gvs2 ->
                Cps.List.mapk
                    (fun (g1, v1) k ->
                        Cps.List.mapk
                            (fun (g2, v2) k ->
                                simplifyAnd mtd g1 g2 (fun g ->
                                simplifyConnective mtd operation opposite stopValue ignoreValue v1 v2 (withFst g >> k)))
                            gvs2 k)
                    gvs1
                    (List.concat >> Union mtd >> k)
            | GuardedValues(gs, vs), _ ->
                Cps.List.mapk (simplifyConnective mtd operation opposite stopValue ignoreValue y) vs (fun xys ->
                List.zip gs xys |> Union mtd |> k)
            | _, GuardedValues(gs, vs) ->
                Cps.List.mapk (simplifyConnective mtd operation opposite stopValue ignoreValue x) vs (fun xys ->
                List.zip gs xys |> Union mtd |> k)
            | _ -> simplifyExt mtd operation opposite stopValue ignoreValue x y k defaultCase

    and private simplifyExt mtd op co stopValue ignoreValue x y matched unmatched =
        match x.term, y.term with
        | _ when y = ignoreValue -> matched x
        | _ when x = ignoreValue -> matched y
        | _ when y = stopValue -> matched stopValue
        | _ when x = stopValue-> matched stopValue
        | _ when x = y -> matched x
        | Negation x, _ when x = y -> matched stopValue
        | _, Negation y when x = y -> matched stopValue
        | Expression _, Expression _ ->
            simplifyExpression mtd op co stopValue ignoreValue x y matched (fun () ->
            simplifyExpression mtd op co stopValue ignoreValue y x matched unmatched)
        | Expression _, _ -> simplifyOpToExpr mtd x y op co stopValue ignoreValue matched unmatched
        | _, Expression _ -> simplifyOpToExpr mtd y x op co stopValue ignoreValue matched unmatched
        | _ -> unmatched ()

    and private simplifyExpression mtd op co stopValue ignoreValue x y matched unmatched =
        match x.term with
        | Expression(Operator(op', false), list, _) when op = op'->
            simplifyOpOp mtd op co stopValue ignoreValue x.metadata list y matched unmatched
        | Expression(Operator(co', false), list, _) when co = co'->
            simplifyCoOp mtd op co stopValue ignoreValue x list y matched unmatched
        | _ -> unmatched ()

    and simplifyOpOp mtd op co stopValue ignoreValue xmtd xargs y matched unmatched =
        // simplifying (OP list) op y at this step
        match xargs, y with
        | [x], y -> simplifyExt mtd op co stopValue ignoreValue x y matched unmatched
        | _ ->
            // Trying to simplify pairwise combinations of x- and y-summands
            let yargs, mtd' =
                match y.term with
                | Expression(Operator(op', false), y', _) when op = op'-> y', Metadata.combine3 mtd xmtd y.metadata
                | _ -> [y], Metadata.combine xmtd mtd
            simplifyPairwiseCombinations xargs yargs Bool id (simplifyExtWithType mtd' op co stopValue ignoreValue) (simplifyConnective mtd' op co stopValue ignoreValue) matched unmatched

    and simplifyCoOp mtd op co stopValue ignoreValue x list y matched unmatched =
        match list, y.term with
        | [x], _ -> simplifyExt mtd op co stopValue ignoreValue x y matched unmatched
        // Co(... y ...) op y = y
        | _ when List.contains y list -> matched y
        // Co(... y ...) op !y = Co(... ...) op !y
        | _, Negation y' when List.contains y' list -> matched (makeCoOpBinaryTerm mtd y.metadata y (List.except [y'] list) co op)
        // Co(... !y ...) op y = Co(... ...) op y
        | _ when List.contains (negate y Metadata.empty) list -> matched (makeCoOpBinaryTerm mtd y.metadata y (List.except [negate y Metadata.empty] list) co op)
        // Co(!a ys) op Co(a ys) = ys
        // Co(a ys) op Co(!a ys zs) = ys co (a op Co(zs)) if (a op Co(zs)) simplifies
        // Co(a ys) op Co(!a ys zs) = Co(a ys) op Co(ys zs)
        | _, Expression(Operator(co', false), IntersectionExceptOneNegation list (a, ys, zs), _) when co' = co ->
            if zs.IsEmpty then makeNAry co ys false Bool (Metadata.combine3 mtd x.metadata y.metadata) |> matched
            else
                let xymtd = Metadata.combine x.metadata y.metadata
                let coZs = makeNAry co zs false Bool xymtd
                simplifyExt mtd op co stopValue ignoreValue a coZs
                    (fun aOpZs -> makeNAry co (aOpZs::ys) false Bool xymtd |> matched)
                    (fun () ->
                        let y' = makeNAry co (List.append ys zs) false Bool y.metadata
                        simplifyCoOp mtd op co stopValue ignoreValue x list y' matched (fun () ->
                        makeNAry op [x; y'] false Bool mtd |> matched))
        // Co(list) op Co(permutation of list) -> Co(list)
        // TODO: sort terms to avoid permutation checking
        | _, Expression(Operator(co', false), ys, _)  when co' = co && isPermutationOf list ys -> matched x
        // Co(...) op OP(...) -> pairwise
        | _, Expression(Operator(op', false), y', _) when op = op' ->
            let mtd' = Metadata.combine3 mtd x.metadata y.metadata
            // Trying to simplify pairwise combinations of x- and y-summands
            simplifyPairwiseCombinations [x] y' Bool id (simplifyExtWithType mtd' op co stopValue ignoreValue) (simplifyConnective mtd' op co stopValue ignoreValue) matched unmatched
        | _ -> unmatched ()

    and private simplifyOpToExpr mtd x y op co stopValue ignoreValue matched unmatched =
        match x.term with
        | Expression(Operator(op', false), xs, _) when op = op'->
            simplifyOpOp mtd op co stopValue ignoreValue x.metadata xs y matched unmatched
        | Expression(Operator(op', false), xs, _) when co = op'->
            simplifyCoOp mtd op co stopValue ignoreValue x xs y matched unmatched
        | _ -> unmatched ()

    and simplifyAnd mtd x y k =
        simplifyConnective mtd OperationType.LogicalAnd OperationType.LogicalOr False True x y k

    and simplifyOr mtd x y k =
        simplifyConnective mtd OperationType.LogicalOr OperationType.LogicalAnd True False x y k

    and internal simplifyNegation mtd x k =
        match simplifier with
        | Some simplifier -> simplifier.Simplify(makeUnary OperationType.LogicalNeg x false Bool mtd) |> k
        | None ->
            match x.term with
            | Error _ -> k x
            | Concrete(b, t) -> Concrete (Metadata.combine x.metadata mtd) (not (b :?> bool)) t |> k
            | Negation x -> k x
            | Conjunction xs -> Cps.List.mapk (simplifyNegation mtd) xs (fun l -> makeNAry OperationType.LogicalOr l false Bool x.metadata |> k)
            | Disjunction xs -> Cps.List.mapk (simplifyNegation mtd) xs (fun l -> makeNAry OperationType.LogicalAnd l false Bool x.metadata |> k)
            | Terms.GuardedValues(gs, vs) ->
                Cps.List.mapk (simplifyNegation mtd) vs (List.zip gs >> Union mtd >> k)
            | _ -> makeUnary OperationType.LogicalNeg x false Bool mtd |> k

    and private simplifyExtWithType mtd op co stopValue ignoreValue _ x y matched unmatched =
        simplifyExt mtd op co stopValue ignoreValue x y matched unmatched

// ------------------------------- General functions -------------------------------

    let (!!) x =
        simplifyNegation Metadata.empty x id

    let (&&&) x y =
        simplifyAnd Metadata.empty x y id

    let (|||) x y =
        simplifyOr Metadata.empty x y id

    let eq x y =
        simplifyOr Metadata.empty !!x y (fun x' -> simplifyOr Metadata.empty x !!y (fun y' -> simplifyAnd Metadata.empty x' y' id))

    let neq x y =
        !! (eq x y)

    let implies x y mtd =
        simplifyNegation mtd x (fun notX ->
        simplifyOr mtd notX y id)

    let lazyAnd mtd x y k =
        match x with
        | False -> x
        | _ -> simplifyAnd mtd x (y()) k

    let lazyOr mtd x y k =
        match x with
        | False -> x
        | _ -> simplifyOr mtd x (y()) k

    let conjunction mtd = function
        | Seq.Cons(x, xs) ->
            if Seq.isEmpty xs then x
            else Seq.fold (&&&) x xs
        | _ -> makeTrue mtd

    let disjunction mtd = function
        | Seq.Cons(x, xs) ->
            if Seq.isEmpty xs then x
            else Seq.fold (|||) x xs
        | _ -> makeFalse mtd

    let lazyConjunction mtd xs =
        Cps.Seq.foldlk (lazyAnd mtd) (makeTrue mtd) xs id

    let lazyDisjunction mtd xs k =
        Cps.Seq.foldlk (lazyOr mtd) (makeFalse mtd) xs k

    let simplifyBinaryConnective mtd op x y k =
        match op with
        | OperationType.LogicalAnd -> simplifyAnd mtd x y k
        | OperationType.LogicalOr -> simplifyOr mtd x y k
        | OperationType.LogicalXor ->
            simplifyNegation mtd x (fun x' -> simplifyNegation mtd y (fun y' ->
            simplifyOr mtd x' y' (fun x' -> simplifyOr mtd x y (fun y' -> simplifyAnd mtd x' y' k))))
        | OperationType.Equal -> simplifyOr mtd !!x y (fun x' -> simplifyOr mtd x !!y (fun y' -> simplifyAnd mtd x' y' k))
        | OperationType.NotEqual -> simplifyOr mtd !!x y (fun x' -> simplifyOr mtd x !!y (fun y' -> simplifyAnd mtd x' y' (fun res -> simplifyNegation mtd res k)))
        | _ -> internalfailf "%O is not a binary logical operator" op

    let simplifyUnaryConnective mtd op x k =
        match op with
        | OperationType.LogicalNeg -> simplifyNegation mtd x k
        | _ -> internalfailf "%O is not an unary logical operator" op

    let isLogicalOperation op t1 t2 =
        Types.isBool t1 && Types.isBool t2 &&
        match op with
        | OperationType.LogicalAnd
        | OperationType.LogicalOr
        | OperationType.LogicalXor
        | OperationType.LogicalNeg
        | OperationType.Equal
        | OperationType.NotEqual -> true
        | _ -> false
