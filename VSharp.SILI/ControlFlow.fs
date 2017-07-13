namespace VSharp

type StatementResult =
    | NoResult
    | Break
    | Continue
    | Rollback of FunctionIdentifier
    | Return of Term
    | Throw of Term
    | Guarded of (Term * StatementResult) list

module internal ControlFlow =

    let rec internal mergeResults condition thenRes elseRes =
        match thenRes, elseRes with
        | _, _ when thenRes = elseRes -> thenRes
        | Return thenVal, Return elseVal -> Return (Merging.merge2Terms condition !!condition thenVal elseVal)
        | Throw thenVal, Throw elseVal -> Throw (Merging.merge2Terms condition !!condition thenVal elseVal)
        | Rollback _, _ -> thenRes
        | _, Rollback _ -> elseRes
        | Guarded gvs1, Guarded gvs2 ->
            gvs1
                |> List.map (fun (g1, v1) -> mergeGuarded gvs2 condition ((&&&) g1) v1 fst snd)
                |> List.concat
                |> Guarded
        | Guarded gvs1, _ -> mergeGuarded gvs1 condition id elseRes fst snd |> Merging.mergeSame |> Guarded
        | _, Guarded gvs2 -> mergeGuarded gvs2 condition id thenRes snd fst |> Merging.mergeSame |> Guarded
        | _, _ -> Guarded [(condition, thenRes); (!!condition, elseRes)]

    and private mergeGuarded gvs cond guard other thenArg elseArg =
        let mergeOne (g, v) =
            match mergeResults cond (thenArg (v, other)) (elseArg (v, other)) with
            | Guarded gvs -> gvs |> List.map (fun (g2, v2) -> (guard(g &&& g2), v2))
            | v -> List.singleton (guard(g), v)
        gvs |> List.map mergeOne |> List.concat |> List.filter (fst >> Terms.IsFalse >> not)

    let rec private createImplicitPathCondition (statement : JetBrains.Decompiler.Ast.IStatement) accTerm (term, statementResult) =
        match statementResult with
        | NoResult -> term ||| accTerm
        | Continue -> if Transformations.isContinueConsumer statement then term ||| accTerm else accTerm
        | Guarded gvs ->
            List.fold (createImplicitPathCondition statement) accTerm gvs
        | _ -> accTerm

    let internal currentCalculationPathCondition (statement : JetBrains.Decompiler.Ast.IStatement) statementResult =
         createImplicitPathCondition statement Terms.MakeFalse (Terms.MakeTrue, statementResult)

    let internal isRollback = function
        | Error(Concrete(:? FunctionIdentifier, Bottom)) -> true
        | _ -> false

    let rec internal consumeContinue = function
        | Continue -> NoResult
        | Guarded gvs -> gvs |> List.map (fun (g, v) -> (g, consumeContinue v)) |> Guarded
        | r -> r

    let rec internal consumeBreak = function
        | Break -> NoResult
        | Guarded gvs -> gvs |> List.map (fun (g, v) -> (g, consumeBreak v)) |> Guarded
        | r -> r

    let rec internal throwOrIgnore = function
        | Error(Concrete(id, Bottom)) when (id :? FunctionIdentifier) -> Rollback (id :?> FunctionIdentifier)
        | Error t -> Throw t
        | Terms.GuardedValues(gs, vs) -> vs |> List.map throwOrIgnore |> List.zip gs |> Guarded
        | t -> NoResult

    let rec internal throwOrReturn = function
        | Error(Concrete(id, Bottom)) when (id :? FunctionIdentifier) -> Rollback (id :?> FunctionIdentifier)
        | Error t -> Throw t
        | Terms.GuardedValues(gs, vs) -> vs |> List.map throwOrReturn |> List.zip gs |> Guarded
        | Nop -> NoResult
        | t -> Return t

    let rec internal consumeErrorOrReturn consumer = function
        | Error(Concrete(id, Bottom)) when (id :? FunctionIdentifier) -> Rollback (id :?> FunctionIdentifier)
        | Error t -> consumer t
        | Nop -> NoResult
        | Terms.GuardedValues(gs, vs) -> vs |> List.map (consumeErrorOrReturn consumer) |> List.zip gs |> Guarded
        | t -> Return t

    let rec internal composeSequentially oldRes newRes oldState newState =
        let calculationDone = function
            | NoResult -> false
            | _ -> true
        let rec composeFlat newRes oldRes =
            match oldRes with
            | NoResult -> newRes
            | _ -> oldRes
        match oldRes, newRes with
        | NoResult, _
        | _, Rollback _ ->
            newRes, newState
        | Break, _
        | Continue, _
        | Throw _, _
        | Rollback _, _
        | Return _, _ -> oldRes, oldState
        | Guarded gvs, _ ->
            let conservativeGuard = List.fold (fun acc (g, v) -> if calculationDone v then acc &&& g else acc) Terms.MakeTrue gvs in
            let result =
                match newRes with
                | Guarded gvs' ->
                    let composeOne (g, v) = List.map (fun (g', v') -> (g &&& g', composeFlat v' v)) gvs' in
                    gvs |> List.map composeOne |> List.concat |> List.filter (fst >> Terms.IsFalse >> not) |> Merging.mergeSame
                | _ ->
                    let gs, vs = List.unzip gvs in
                    List.zip gs (List.map (composeFlat newRes) vs)
            in
            Guarded result, Merging.merge2States conservativeGuard !!conservativeGuard oldState newState

    let rec internal resultToTerm = function
        | Return term -> term
        | Throw err -> Error err
        | Guarded gvs -> Merging.guardedMap resultToTerm gvs
        | Rollback id -> Error(Concrete(id, Bottom)) // A bit hacky encoding of rollback to avoid introducing of special Term constructor
        | _ -> Nop

    let internal pickOutExceptions result =
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
            let mergedGuard = disjunction gs in
            let mergedValue = Merging.merge gvs in
            Some(mergedGuard, mergedValue), normal

    let internal throw exn =
        Throw(Terms.MakeConcrete exn (exn.GetType()))

    let internal npe () =
        throw (new System.NullReferenceException())
