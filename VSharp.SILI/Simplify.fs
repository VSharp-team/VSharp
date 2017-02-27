namespace VSharp

open VSharp.Terms

module internal Simplify =

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
                    toReduce |> List.reduce reduce |> matched
            | x::xs ->
                combineOne x ys [] (fun res ys -> combine xs ys (res::acc))

        combine xs ys []

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
