namespace VSharp

open VSharp.Terms

module internal Simplify =

    let internal simplifyPairwiseCombinations = Propositional.simplifyPairwiseCombinations

    let rec internal simplifyGenericUnary name x matched concrete unmatched =
        match x with
        | Error _ -> matched x
        | Concrete(x, typeofX) -> concrete x typeofX |> matched
        | GuardedValues(guards, values) ->
            Cps.List.mapk (fun term matched -> simplifyGenericUnary name term matched concrete unmatched) values (fun values' ->
                Merging.merge (List.zip guards values') |> matched)
        | _ -> unmatched x matched

    let rec internal simplifyGenericBinary name x y matched concrete unmatched repeat =
        match x, y with
        | Error _, _ -> matched x
        | _, Error _ -> matched y
        | Concrete(x, typeOfX), Concrete(y, typeOfY) -> concrete x y typeOfX typeOfY |> matched
        | Union(gvsx), Union(gvsy) ->
            let compose (gx, vx) (gy, vy) matched = repeat vx vy (fun xy -> (gx &&& gy, xy) |> matched) in
                let join (gx, vx) k = Cps.List.mapk (compose (gx, vx)) gvsy k in
                    Cps.List.mapk join gvsx (fun gvss -> Merging.merge (List.concat gvss) |> matched)
        | GuardedValues(guardsX, valuesX), _ ->
            Cps.List.mapk (fun x matched -> repeat x y matched) valuesX (fun values' ->
            Merging.merge (List.zip guardsX values') |> matched)
        | _, GuardedValues(guardsY, valuesY) ->
            Cps.List.mapk (fun y matched -> repeat x y matched) valuesY (fun values' ->
            Merging.merge (List.zip guardsY values') |> matched)
        | _ -> unmatched x y matched
