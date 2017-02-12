namespace VSharp

type StatementResult = NoResult | Break | Continue | Return of Term | Guarded of (Term * StatementResult) list

module internal ControlFlow =

    let rec internal mergeResults condition thenRes elseRes =
        match thenRes, elseRes with
        | _, _ when thenRes = elseRes -> thenRes
        | Return thenVal, Return elseVal -> Return (Merging.merge2Terms condition thenVal elseVal)
        | Guarded gvs1, Guarded gvs2 ->
            gvs1
                |> List.map (fun (g1, v1) -> mergeGuarded gvs2 condition ((&&&) g1) v1 fst snd)
                |> List.concat
                |> Guarded
        | Guarded gvs1, _ -> mergeGuarded gvs1 condition id elseRes snd fst |> Guarded
        | _, Guarded gvs2 -> mergeGuarded gvs2 condition id thenRes fst snd |> Guarded
        | _, _ -> Guarded [(condition, thenRes); (!!condition, elseRes)]

    and private mergeGuarded gvs cond guard other thenArg elseArg =
        let mergeOne (g, v) =
            match mergeResults cond (thenArg (v, other)) (elseArg (v, other)) with
            | Guarded gvs -> gvs |> List.map (fun (g2, v2) -> (guard(g &&& g2), v2))
            | v -> List.singleton (guard(g), v)
        gvs |> List.map mergeOne |> List.concat

    let rec composeSequentially oldRes newRes oldState newState =
        let conservative = function
            | NoResult -> false
            | _ -> true
        let rec composeFlat newRes oldRes =
            match oldRes with
            | NoResult -> newRes
            | _ -> oldRes
        match oldRes, newRes with
        | NoResult, _ -> newRes, newState
        | Break, _
        | Continue, _
        | Return _, _ -> oldRes, oldState
        | Guarded gvs, _ ->
            let conservativeGuard = List.fold (fun acc (g, v) -> if conservative v then acc &&& g else acc) Terms.MakeTrue gvs in
            let result =
                match newRes with
                | Guarded gvs' ->
                    let composeOne (g, v) = List.map (fun (g', v') -> (g &&& g', composeFlat v v')) gvs' in
                    gvs |> List.map composeOne |> List.concat |> Merging.mergeSame
                | _ ->
                    let gs, vs = List.unzip gvs in
                    List.zip gs (List.map (composeFlat newRes) vs)
            in
            Guarded result, Merging.mergeStates conservativeGuard oldState newState

    let resultToTerm (result, state) =
        let isReturn = function
            | Return _ -> true
            | _ -> false
        let same v1 (_, v2) =
            match v1, v2 with
            | Return _, Return _ -> true
            | NoResult, NoResult -> true
            | _, _ -> false
        let getTerm = function
            | g, Return t -> g, t
            | g, _ -> g, Nop
        match result with
        | Return term -> (term, state)
        | Guarded gvs ->
            match gvs with
            | [] -> (Nop, state)
            | (g, v)::gvs' ->
                assert(List.forall (same v) gvs')
                if isReturn v
                then Merging.merge (List.map getTerm gvs) state
                else (Nop, state)
        | _ -> (Nop, state)
