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

    let internal callOrApplyEffect areWeStuck body id state k =
        if areWeStuck then
            Functions.UnboundedRecursionExplorer.exploreIfShould id (fun () ->
            Functions.UnboundedRecursionExplorer.reproduceEffect id state k)
        else
            body state k

    let internal call state funcId body returnType k =
        // TODO: Non-actual comment, split and remove it
        // This is one of the most important moments for the unbound recursion encoding.
        // Every (mutually-)recursive function must be either invoked concretely if possible
        // (for example, for (int i = 0; i < 8; ++i) ...), or encode its body into a symbolic term
        // in a completely symbolic environment (every read-dependence of function like arguments
        // or global fields are symbolic), invoking it just once and then solving it with some
        // fancy inductive reasoning engines.
        // Hovewer there are 3 problems:
        // 1. We can understand that concrete execution is impossible only when reach the recursive call.
        //    If we see that path-condition differs from the initial call point, we risk to stuck in an
        //    infinite symbolic recursion and thus should rollback to the initial call point and try the
        //    second strategy (encoding the whole body into one symbolic term).
        // 2. In this second mode we still experience some difficulties. Meeting unbound (mutually-)recursive
        //    call, we should artificially mutate all the locations it can potentially mutate. But during the
        //    first execution we do not know what those locations are yet. To deal with it, we approximate read-
        //    and write-dependencies iteratively. We perform one execution of function body, thus obtaining a
        //    symbolic term with incorrectly mutated state (where all locations that can potentially be mutated ARE
        //    mutated, though probably with incorrect values) and calculate read- and write-dependencies of our function.
        //    Then we repeat the process with new write-dependencies and do it until dependencies set converges.
        // 3. For mutual recursion we can't just do these steps for each function separately: read- and
        //    write-dependencies of a mutually recursive component is a union of dependencies of each function.
        let shouldStopUnrolling = detectUnboundRecursion funcId state in
        callOrApplyEffect shouldStopUnrolling body funcId state (fun (result, state) ->
        k (result, State.popStack state))
