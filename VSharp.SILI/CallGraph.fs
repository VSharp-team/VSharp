namespace VSharp

module CallGraph =
    type private Frame = FunctionIdentifier * Term list

    let private detectUnboundRecursion id ((_, _, _, (frames, _), pathCondition) : State.state) =
        let isRecursiveFrame (metadata, _, _) =
            match metadata with
            | Some(id', _) when id = id' -> true
            | _ -> false
        in
        let bottomOccurence = Stack.tryFindBottom isRecursiveFrame frames in
        match bottomOccurence with
        | None -> false
        | Some(Some(_, p'), _, _) when pathCondition = p' ->
            match Options.RecursionUnrollingMode() with
            | Options.AlwaysDisableUnrolling -> true
            | _ -> false
        | _ ->
            match Options.RecursionUnrollingMode() with
            | Options.AlwaysEnableUnrolling -> false
            | _ -> true

    let internal callOrApplyEffect mtd areWeStuck body id state k =
        if areWeStuck then
            Functions.UnboundedRecursionExplorer.markAsRecursive id
            Functions.UnboundedRecursionExplorer.exploreIfShould id (fun () ->
            Functions.UnboundedRecursionExplorer.reproduceEffect mtd id state k)
        else
            body state k

    let internal call mtd funcId state body k =
        let shouldStopUnrolling = detectUnboundRecursion funcId state in
        callOrApplyEffect mtd shouldStopUnrolling body funcId state (fun (result, state) ->
        k (result, State.popStack state))
