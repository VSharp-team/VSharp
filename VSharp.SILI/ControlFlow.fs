namespace VSharp

type StatementResult =
    | NoResult
    | Break
    | Continue
    | Return of Term
    | Throw of Term
    | Guarded of (Term * StatementResult) list

module internal ControlFlow =

    let rec internal mergeResults condition thenRes elseRes =
        match thenRes, elseRes with
        | _, _ when thenRes = elseRes -> thenRes
        | Return thenVal, Return elseVal -> Return (Merging.merge2Terms condition !!condition thenVal elseVal)
        | Throw thenVal, Throw elseVal -> Throw (Merging.merge2Terms condition !!condition thenVal elseVal)
        | Guarded gvs1, Guarded gvs2 ->
            gvs1
                |> List.map (fun (g1, v1) -> mergeGuarded gvs2 condition ((&&&) g1) v1 fst snd)
                |> List.concat
                |> Guarded
        | Guarded gvs1, _ -> mergeGuarded gvs1 condition id elseRes snd fst |> Guarded
        | _, Guarded gvs2 -> mergeGuarded gvs2 !!condition id thenRes fst snd |> Guarded
        | _, _ -> Guarded [(condition, thenRes); (!!condition, elseRes)]

    and private mergeGuarded gvs cond guard other thenArg elseArg =
        let mergeOne (g, v) =
            match mergeResults cond (thenArg (v, other)) (elseArg (v, other)) with
            | Guarded gvs -> gvs |> List.map (fun (g2, v2) -> (guard(g &&& g2), v2))
            | v -> List.singleton (guard(g), v)
        gvs |> List.map mergeOne |> List.concat

    let rec calculationDone (statement : JetBrains.Decompiler.Ast.IStatement) = function
        | NoResult -> false
        | Continue -> Transformations.isContinueConsumer statement
        | Guarded gvs ->
            List.forall (snd >> (calculationDone statement)) gvs
        | _ -> true

    let rec consumeContinue = function
        | Continue -> NoResult
        | Guarded gvs -> gvs |> List.map (fun (g, v) -> (g, consumeContinue v)) |> Guarded
        | r -> r

    let rec consumeBreak = function
        | Break -> NoResult
        | Guarded gvs -> gvs |> List.map (fun (g, v) -> (g, consumeBreak v)) |> Guarded
        | r -> r

    let rec throwOrIgnore = function
        | Error t -> Throw t
        | Terms.GuardedValues(gs, vs) -> vs |> List.map throwOrIgnore |> List.zip gs |> Guarded
        | t -> NoResult

    let rec throwOrReturn = function
        | Error t -> Throw t
        | Terms.GuardedValues(gs, vs) -> vs |> List.map throwOrReturn |> List.zip gs |> Guarded
        | Nop -> NoResult
        | t -> Return t

    let rec consumeErrorOrReturn consumer = function
        | Error t -> consumer t
        | Nop -> NoResult
        | Terms.GuardedValues(gs, vs) -> vs |> List.map (consumeErrorOrReturn consumer) |> List.zip gs |> Guarded
        | t -> Return t

    let rec composeSequentially oldRes newRes oldState newState =
        let calculationDone = function
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
        | Throw _, _
        | Return _, _ -> oldRes, oldState
        | Guarded gvs, _ ->
            let conservativeGuard = List.fold (fun acc (g, v) -> if calculationDone v then acc &&& g else acc) Terms.MakeTrue gvs in
            let result =
                match newRes with
                | Guarded gvs' ->
                    let composeOne (g, v) = List.map (fun (g', v') -> (g &&& g', composeFlat v' v)) gvs' in
                    gvs |> List.map composeOne |> List.concat |> Merging.mergeSame
                | _ ->
                    let gs, vs = List.unzip gvs in
                    List.zip gs (List.map (composeFlat newRes) vs)
            in
            Guarded result, Merging.merge2States conservativeGuard !!conservativeGuard oldState newState

    let rec resultToTerm = function
        | Return term -> term
        | Throw err -> Error err
        | Guarded gvs ->
            let gs, vs = List.unzip gvs in
            vs |> List.map resultToTerm |> List.zip gs |> Merging.merge
        | _ -> Nop

    let pickOutExceptions result =
        let gvs =
            match result with
            | Throw e -> [(Terms.MakeTrue, result)]
            | Guarded gvs -> gvs
            | _ -> [(Terms.MakeTrue, result)]
        in
        let thrown, normal = mappedPartition (function | g, Throw e -> Some (g, e) | _ -> None) gvs in
        match thrown with
        | [] -> None, normal
        | gvs ->
            let gs, vs = List.unzip gvs in
            let mergedGuard = List.fold (|||) Terms.MakeFalse gs in
            let mergedValue = Merging.merge gvs in
            Some(mergedGuard, mergedValue), normal

    let npe () =
        Throw(Terms.MakeConcrete (new System.NullReferenceException()) typedefof<System.NullReferenceException>)
