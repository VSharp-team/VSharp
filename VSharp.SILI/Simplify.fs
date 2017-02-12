namespace VSharp.Core.Symbolic

open VSharp
open VSharp.Core.Symbolic
open VSharp.Core.Symbolic.Terms

module internal Simplify =

    // Trying to simplify pairwise combinations of x- and y-operands.
    // For example, it tries to simplify (a + b) + (c + d) or (a * b) * (c * d)
    // by successively trying to combine (a * c), (a * d), (b * c) and (b * d).
    let internal simplifyPairwiseCombinations xs ys state t simplify reduce matched unmatched =
        let initialYs = ys

        let rec combineOne x ys failed state k =
            match ys with
            | [] -> k x failed state
            | h :: tl ->
                simplify x h state false t
                    (fun (x, state) -> combineOne x tl failed state k)
                    (fun state -> combineOne x tl (h::failed) state k)

        let rec combine xs ys acc state =
            match xs with
            | [] ->
                // Here we traversed all xs, checking for something matched...
                if List.length ys = List.length initialYs then unmatched state // Nothing matched, the whole process is failed
                else
                    // Something matched, the work is done, just combining results together...
                    let toReduce = List.append (List.rev acc) ys in
                    // TODO: care about different types...
                    (toReduce |> List.reduce reduce, state) |> matched
            | x::xs ->
                combineOne x ys [] state (fun res ys state -> combine xs ys (res::acc) state)

        combine xs ys [] state

    let rec internal simplifyGenericUnary name x state matched concrete unmatched =
        match x with
        | Error _ -> matched (x, state)
        | Nop -> raise(new System.ArgumentException(sprintf "Invalid operand of %s!" name))
        | Concrete(x, typeofX) -> (concrete x typeofX, state) |> matched
        | GuardedValues(guards, values) ->
            Cps.List.mapFoldk (fun state term matched -> simplifyGenericUnary name term state matched concrete unmatched) state values (fun (values', state') ->
                Merging.merge (List.zip guards values') state' |> matched)
        | _ -> unmatched x state matched

    let rec internal simplifyGenericBinary name x y state matched concrete unmatched =
        match x, y with
        | Error _, _ -> matched (x, state)
        | _, Error _ -> matched (y, state)
        | Nop, _ -> raise(new System.ArgumentException(sprintf "Invalid left operand of %s!" name))
        | _, Nop -> raise(new System.ArgumentException(sprintf "Invalid right operand of %s!" name))
        | Concrete(x, typeOfX), Concrete(y, typeOfY) -> (concrete x y typeOfX typeOfY, state) |> matched
        | Union(gvsx), Union(gvsy) ->
            let compose (gx, vx) state (gy, vy) matched = simplifyGenericBinary name vx vy state (fun (xy, state) -> ((gx &&& gy, xy), state) |> matched) concrete unmatched in
                let join state (gx, vx) k = Cps.List.mapFoldk (compose (gx, vx)) state gvsy k in
                    Cps.List.mapFoldk join state gvsx (fun (gvss, state) -> Merging.merge (List.concat gvss) state |> matched)
        | GuardedValues(guardsX, valuesX), _ ->
            Cps.List.mapFoldk (fun state x matched -> simplifyGenericBinary name x y state matched concrete unmatched) state valuesX (fun (values', state') ->
            Merging.merge (List.zip guardsX values') state' |> matched)
        | _, GuardedValues(guardsY, valuesY) ->
            Cps.List.mapFoldk (fun state y matched -> simplifyGenericBinary name x y state matched concrete unmatched) state valuesY (fun (values', state') ->
            Merging.merge (List.zip guardsY values') state' |> matched)
        | _ -> unmatched x y state matched
