namespace VSharp.Core

#nowarn "69"

open VSharp


module internal Common =
//    let mutable private solver : ISolver option = None
//    let configureSolver s = solver <- Some s
//    let private solve term =
//        match solver with
//        | Some s -> s.Solve term
//        | None -> Unknown
//    let private solvePC term pc =
//        match solver with
//        | Some s -> s.SolvePathCondition term pc
//        | None -> Unknown

// ------------------------------- Simplification -------------------------------

    let simplifyPairwiseCombinations = Propositional.simplifyPairwiseCombinations

    let simplifyConcreteBinary simplify t _ _ xval yval _ _ =
        simplify t xval yval

    let rec simplifyGenericUnary name x matched concrete unmatched =
        match x.term with
        | Concrete(xval, typeofX) -> concrete x xval typeofX |> matched
        | GuardedValues(guards, values) ->
            Cps.List.mapk (fun term matched -> simplifyGenericUnary name term matched concrete unmatched) values (fun values' ->
            (List.zip guards values') |> GenericIteType.IteFromGvs |> Merging.merge |> matched)
        | _ -> unmatched x matched

    let rec simplifyGenericBinary _ x y matched concrete unmatched repeat =
        match x.term, y.term with
        | Concrete(xval, typeOfX), Concrete(yval, typeOfY) -> concrete x y xval yval typeOfX typeOfY |> matched
        | Gvs gvsx, Gvs gvsy ->
            let compose (gx, vx) (gy, vy) matched = repeat vx vy (fun xy -> (gx &&& gy, xy) |> matched)
            let join (gx, vx) k = Cps.List.mapk (compose (gx, vx)) gvsy k
            Cps.List.mapk join gvsx (List.concat >> GenericIteType.IteFromGvs >> Merging.merge >> matched)
        | Ite {ite = ite; elseValue = e}, _ ->
            Cps.List.mapk (fun (g, x) matched -> repeat x y (fun x' -> matched (g, x'))) ite (fun ite' ->
            repeat e y (fun ey ->
            {ite = ite'; elseValue = ey} |> Merging.merge |> matched))
        | _, Ite {ite = ite; elseValue = e} ->
            Cps.List.mapk (fun (g, y) matched -> repeat x y (fun x' -> matched (g, x'))) ite (fun ite' ->
            repeat x e (fun xe ->
            {ite = ite'; elseValue = xe} |> Merging.merge |> matched))
        | _ -> unmatched x y matched

// ---------------------------------------- Branching ---------------------------------------

//    let commonStatelessConditionalExecutionk pc conditionInvocation thenBranch elseBranch merge merge2 errorHandler k =
//        let execution condition k =
//            thenBranch (fun thenResult ->
//            elseBranch (fun elseResult ->
//            k <| merge2 condition !!condition thenResult elseResult))
//        let chooseBranch condition k =
//            match condition with
//            | Terms.True ->  thenBranch k
//            | Terms.False -> elseBranch k
//            | condition ->
//                match solvePC condition pc with
//                | Unsat -> elseBranch k
//                | _ ->
//                    match solvePC (!!condition) pc with
//                    | Unsat -> thenBranch k
//                    | _ -> execution condition k
//        conditionInvocation (fun condition ->
//        Merging.commonGuardedErroredApplyk chooseBranch errorHandler condition merge k)

    let commonStatelessConditionalExecutionk conditionInvocation thenBranch elseBranch merge2 k =
        let execution condition k =
            thenBranch (fun thenResult ->
            elseBranch (fun elseResult ->
            k <| merge2 condition !!condition thenResult elseResult))
        conditionInvocation (fun condition ->
        match condition with
        | Terms.True ->  thenBranch k
        | Terms.False -> elseBranch k
        | _ -> execution condition k)

    let statelessConditionalExecutionWithMergek conditionInvocation thenBranch elseBranch k = commonStatelessConditionalExecutionk conditionInvocation thenBranch elseBranch Merging.merge2Terms k
