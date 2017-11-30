namespace VSharp

open JetBrains.Decompiler.Ast

[<AutoOpen>]
module internal Operators =

    let rec internal refToInt term =
        match term.term with
        | Error _ -> term
        | Concrete(null, _) -> Concrete 0 Types.pointerType term.metadata
        | HeapRef(((addr, _), _), _, _) -> addr
        | Union gvs -> Merging.guardedMap refToInt gvs
        | _ -> term

    let simplifyBinaryOperation mtd op isChecked state t left right k =
        let t1 = Terms.TypeOf left in
        let t2 = Terms.TypeOf right in
        match op with
        | op when Propositional.isLogicalOperation op t1 t2 ->
            Propositional.simplifyBinaryConnective mtd op left right (withSnd state >> k)
        | op when Arithmetics.isArithmeticalOperation op t1 t2 ->
            Arithmetics.simplifyBinaryOperation mtd op state left right isChecked t k
        | op when Strings.isStringOperation op t1 t2 ->
            Strings.simplifyOperation mtd op left right |> (withSnd state >> k)
        | op when Pointers.isPointerOperation op t1 t2 ->
            Pointers.simplifyBinaryOperation mtd op state left right k
        | _ -> __notImplemented__()

    let ksimplifyEquality mtd x y k =
        simplifyBinaryOperation mtd JetBrains.Decompiler.Ast.OperationType.Equal false State.empty typeof<bool> x y (fst >> k)

    let simplifyEquality mtd x y =
        ksimplifyEquality mtd x y id

    let (===) x y = ksimplifyEquality Metadata.empty x y id
    let (!==) x y = ksimplifyEquality Metadata.empty x y (!!)

    let simplifyUnaryOperation mtd op isChecked state t arg k =
        match t with
        | Bool -> Propositional.simplifyUnaryConnective mtd op arg (withSnd state >> k)
        | Numeric t -> Arithmetics.simplifyUnaryOperation mtd op state arg isChecked t k
        | String -> __notImplemented__()
        | _ -> __notImplemented__()

    let simplifyHeapPointwiseEquality mtd h1 h2 =
        Heap.unify (Terms.MakeTrue mtd) h1 h2 (fun s k v1 v2 ->
            match v1, v2 with
            | Some v1, Some v2 -> simplifyAnd mtd s (simplifyEquality mtd v1.value v2.value) id
            | None, Some v2 -> s
            | _ -> __notImplemented__())

    let simplifyArraysEquality mtd x y =

        let inline eqTypes mtd type1 type2 k = simplifyAnd mtd (Common.is mtd type1 type2) (Common.is mtd type2 type1) k

        let simplifyGInstantiatorEquality mtd gInstor1 gInstor2 =

            let instorEq mtd x y k =
                match x, y with
                | DefaultInstantiator(_, typ1), DefaultInstantiator(_, typ2) -> eqTypes mtd typ1 typ2 k
                | LazyInstantiator(term1, typ1), LazyInstantiator(term2, typ2)
                | DefaultInstantiator(term1, typ1), LazyInstantiator(term2, typ2)
                | LazyInstantiator(term1, typ1), DefaultInstantiator(term2, typ2) ->
                    eqTypes mtd typ1 typ2 (fun equalTypes ->
                    simplifyAnd mtd equalTypes (MakeBinary (OperationType.Equal) term1 term2 false Bool mtd) k)

            List.fold (fun acc (g1, instor1) ->
                simplifyOr mtd acc (List.fold (fun acc (g2, instor2) ->
                    simplifyAnd mtd g1 g2 (fun guardsEq ->
                    instorEq mtd instor1 instor2 (fun instantiatorEq ->
                    simplifyAnd mtd acc (implies guardsEq instantiatorEq mtd) id))) (Terms.MakeTrue mtd) gInstor2) id)
                (Terms.MakeTrue mtd)
                gInstor1

        match x.term, y.term with
        | Array(dim1, len1, lb1, instor1, content1, l1, t1), Array(dim2, len2, lb2, instor2, content2, l2, t2) ->
            Propositional.conjunction mtd <|
                seq[
                    simplifyEquality mtd dim1 dim2;
                    simplifyEquality mtd len1 len2;
                    simplifyHeapPointwiseEquality mtd lb1 lb2;
                    simplifyGInstantiatorEquality mtd instor1 instor2;
                    simplifyHeapPointwiseEquality mtd content1 content2;
                    simplifyHeapPointwiseEquality mtd l1 l2;
                    eqTypes mtd t1 t2 id
                ]
        | _ -> internalfail "not array!"
