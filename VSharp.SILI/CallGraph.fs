namespace VSharp

module CallGraph =

    let private detectUnboundRecursion id (s : State.state) =
        let isRecursiveFrame (frame : State.stackFrame) =
            match frame.func with
            | Some(id', _) when id = id' -> true
            | _ -> false
        in
        let bottomOccurence = Stack.tryFindBottom isRecursiveFrame s.frames.f in
        match bottomOccurence with
        | None -> false
        | Some { func = Some(_, p'); entries = _; time =  _ } when s.pc = p' ->
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
            Functions.UnboundedRecursionExplorer.exploreIfShould id
            Functions.UnboundedRecursionExplorer.reproduceEffect mtd id state k
        else
            body state k

    let internal call mtd funcId state body k =
        let shouldStopUnrolling = detectUnboundRecursion funcId state in
        callOrApplyEffect mtd shouldStopUnrolling body funcId state (fun (result, state) ->
        k (result, State.popStack state))
