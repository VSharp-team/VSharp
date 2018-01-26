namespace VSharp.Core

open VSharp

type statementResultNode =
    | NoResult
    | Break
    | Continue
    | Return of term
    | Throw of term
    | Guarded of (term * statementResult) list

and
    [<CustomEquality;NoComparison>]
    statementResult =
        {result : statementResultNode; metadata : termMetadata}
        override x.ToString() =
            x.result.ToString()
        override x.GetHashCode() =
            x.result.GetHashCode()
        override x.Equals(o : obj) =
            match o with
            | :? statementResult as other -> x.result.Equals(other.result)
            | _ -> false

[<AutoOpen>]
module internal ControlFlowConstructors =
    let NoResult metadata = { result = NoResult; metadata = metadata }
    let Break metadata = { result = Break; metadata = metadata }
    let Continue metadata = { result = Continue; metadata = metadata }
    let Return metadata term = { result = Return term; metadata = metadata }
    let Throw metadata term = { result = Throw term; metadata = metadata }
    let Guarded metadata grs = { result = Guarded grs; metadata = metadata }

module internal ControlFlow =

    type private ReturnMarker() = class end

    let rec merge2Results condition1 condition2 thenRes elseRes =
        let metadata = Metadata.combine thenRes.metadata elseRes.metadata
        match thenRes.result, elseRes.result with
        | _, _ when thenRes = elseRes -> thenRes
        | Return thenVal, Return elseVal -> Return metadata (Merging.merge2Terms condition1 condition2 thenVal elseVal)
        | Throw thenVal, Throw elseVal -> Throw metadata (Merging.merge2Terms condition1 condition2 thenVal elseVal)
        | Guarded gvs1, Guarded gvs2 ->
            gvs1
                |> List.map (fun (g1, v1) -> mergeGuarded gvs2 condition1 condition2 ((&&&) g1) v1 fst snd)
                |> List.concat
                |> Guarded metadata
        | Guarded gvs1, _ -> mergeGuarded gvs1 condition1 condition2 id elseRes fst snd |> Merging.mergeSame |> Guarded metadata
        | _, Guarded gvs2 -> mergeGuarded gvs2 condition1 condition2 id thenRes snd fst |> Merging.mergeSame |> Guarded metadata
        | _, _ -> Guarded metadata [(condition1, thenRes); (condition2, elseRes)]

    and private mergeGuarded gvs cond1 cond2 guard other thenArg elseArg =
        let mergeOne (g, v) =
            let merged = merge2Results cond1 cond2 (thenArg (v, other)) (elseArg (v, other))
            match merged.result with
            | Guarded gvs -> gvs |> List.map (fun (g2, v2) -> (guard (g &&& g2), v2))
            | _ -> List.singleton (guard(g), merged)
        gvs |> List.map mergeOne |> List.concat |> List.filter (fst >> Terms.isFalse >> not)

    let rec private createImplicitPathCondition consumeContinue accTerm (term, statementResult) =
        match statementResult.result with
        | NoResult -> term ||| accTerm
        | Continue when consumeContinue -> term ||| accTerm
        | Guarded gvs ->
            List.fold (createImplicitPathCondition consumeContinue) accTerm gvs
        | _ -> accTerm

    let currentCalculationPathCondition consumeContinue statementResult =
         createImplicitPathCondition consumeContinue False (True, statementResult)

    let rec consumeContinue result =
        match result.result with
        | Continue -> NoResult result.metadata
        | Guarded gvs -> gvs |> List.map (fun (g, v) -> (g, consumeContinue v)) |> Guarded result.metadata
        | _ -> result

    let rec consumeBreak result =
        match result.result with
        | Break -> NoResult result.metadata
        | Guarded gvs -> gvs |> List.map (fun (g, v) -> (g, consumeBreak v)) |> Guarded result.metadata
        | _ -> result

    let rec throwOrIgnore term =
        match term.term with
        | Error t -> Throw term.metadata t
        | GuardedValues(gs, vs) -> vs |> List.map throwOrIgnore |> List.zip gs |> Guarded term.metadata
        | _ -> NoResult term.metadata

    let rec throwOrReturn term =
        match term.term with
        | Error t -> Throw term.metadata t
        | GuardedValues(gs, vs) -> vs |> List.map throwOrReturn |> List.zip gs |> Guarded term.metadata
        | Nop when not <| Metadata.miscContains term (ReturnMarker()) -> NoResult term.metadata
        | _ -> Return term.metadata term

    let rec consumeErrorOrReturn consumer term =
        match term.term with
        | Error t -> consumer t
        | Nop -> NoResult term.metadata
        | Terms.GuardedValues(gs, vs) -> vs |> List.map (consumeErrorOrReturn consumer) |> List.zip gs |> Guarded term.metadata
        | _ -> Return term.metadata term

    let rec composeSequentially oldRes newRes oldState newState =
        let calculationDone result =
            match result.result with
            | NoResult -> false
            | _ -> true
        let rec composeFlat newRes oldRes =
            match oldRes.result with
            | NoResult -> newRes
            | _ -> oldRes
        match oldRes.result, newRes.result with
        | NoResult, _ ->
            newRes, newState
        | Break, _
        | Continue, _
        | Throw _, _
        | Return _, _ -> oldRes, oldState
        | Guarded gvs, _ ->
            let conservativeGuard = List.fold (fun acc (g, v) -> if calculationDone v then acc ||| g else acc) False gvs
            let result =
                match newRes.result with
                | Guarded gvs' ->
                    let composeOne (g, v) =
                        List.map (fun (g', v') -> (g &&& g', composeFlat v' v)) gvs'
                    gvs |> List.map composeOne |> List.concat |> List.filter (fst >> Terms.isFalse >> not) |> Merging.mergeSame
                | _ ->
                    let gs, vs = List.unzip gvs
                    List.zip gs (List.map (composeFlat newRes) vs)
            let commonMetadata = Metadata.combine oldRes.metadata newRes.metadata
            Guarded commonMetadata result, Merging.merge2States conservativeGuard !!conservativeGuard oldState newState

    let invokeAfter consumeContinue (result, state) statement k =
        let pathCondition = currentCalculationPathCondition consumeContinue result
        match pathCondition with
        | Terms.True -> statement state (fun (newRes, newState) -> k (composeSequentially result newRes state newState))
        | Terms.False -> k (result, state)
        | _ ->
            statement
                (State.withPathCondition state pathCondition)
                (fun (newRes, newState) ->
                    let newState = State.popPathCondition newState
                    k (composeSequentially result newRes state newState))

    let rec resultToTerm result =
        match result.result with
        | Return term -> Metadata.addMisc term (ReturnMarker()); { term = term.term; metadata = result.metadata }
        | Throw err -> Error result.metadata err
        | Guarded gvs -> Merging.guardedMap resultToTerm gvs
        | _ -> Nop

    let pickOutExceptions result =
        let gvs =
            match result.result with
            | Throw _ -> [(True, result)]
            | Guarded gvs -> gvs
            | _ -> [(True, result)]
        let pickThrown (g, result) =
            match result.result with
            | Throw e -> Some(g, e)
            | _ -> None
        let thrown, normal = List.mappedPartition pickThrown gvs
        match thrown with
        | [] -> None, normal
        | gvs ->
            let gs, _ = List.unzip gvs
            let mergedGuard = disjunction result.metadata gs
            let mergedValue = Merging.merge gvs
            Some(mergedGuard, mergedValue), normal

    let mergeResults grs =
        Merging.guardedMap resultToTerm grs |> throwOrReturn

    let unguardResults gvs =
        let unguard gres =
            match gres with
            | g, {result = Guarded gvs} -> gvs  |> List.map (fun (g', v) -> g &&& g', v)
            | _ -> [gres]
        gvs |> List.map unguard |> List.concat
