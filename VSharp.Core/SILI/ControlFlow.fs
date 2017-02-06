namespace VSharp.Core.Symbolic

open VSharp.Core.Symbolic

type StatementResult = NoResult | Break | Continue | Return of Term | Guarded of (Term * StatementResult) list

module internal ControlFlow =

    let rec internal mergeResults condition thenRes elseRes =
        match thenRes, elseRes with
        | _, _ when thenRes = elseRes -> thenRes
        | Return thenVal, Return elseVal -> Return (Merging.mergeTerms condition thenVal elseVal)
        | Guarded gvs1, Guarded gvs2 ->
            gvs1 |> List.map (fun (g1, v1) -> mergeGuarded gvs2 condition (fun g -> g1 &&& g) v1 fst snd) |> List.concat |> Guarded
        | Guarded gvs1, _ -> mergeGuarded gvs1 condition id elseRes snd fst |> Guarded
        | _, Guarded gvs2 -> mergeGuarded gvs2 condition id thenRes fst snd |> Guarded
        | _, _ -> Guarded [(condition, thenRes); (!!condition, elseRes)]

    and private mergeGuarded gvs cond guard other thenArg elseArg =
        let mergeOne (g, v) =
            match mergeResults cond (thenArg (v, other)) (elseArg (v, other)) with
            | Guarded gvs -> gvs |> List.map (fun (g2, v2) -> (guard(g &&& g2), v2))
            | v -> List.singleton (guard(g), v)
        gvs |> List.map mergeOne |> List.concat

    let rec composeSequentially newRes oldRes =
        match oldRes, newRes with
        | NoResult, _ -> newRes
        | Break, _
        | Continue, _
        | Return _, _ -> oldRes
        | Guarded gvs, _ ->
            let gs, vs = List.unzip gvs in
            Guarded (List.zip gs (List.map (composeSequentially newRes) vs))

    let resultToTerm result = 
        match result with
        | Return term -> term
        | _ -> Nop
