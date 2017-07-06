namespace VSharp

module CallGraph =
    type private Frame = FunctionIdentifier * Term list
    let mutable callStack : Stack.stack<Frame> = Stack.empty

    let private detectUnboundRecursion (f, p) =
        let bottomOccurence = Stack.tryFindBottom (fst >> ((=) f)) callStack in
        match bottomOccurence with
        | None -> false
        | Some (f', p') when p = p' ->
            match Options.RecursionUnrollingMode() with
            | Options.AlwaysDisableUnrolling -> true
            | _ -> false
        | _ ->
            match Options.RecursionUnrollingMode() with
            | Options.AlwaysEnableUnrolling -> false
            | _ -> true

    let rec private approximateIteratively initialState symbolicState body funcId k (result, finalState) =
        match result with
        | Rollback funcId' when funcId' = funcId ->
            let symbolicState = Functions.UnboundedRecursionCache.symbolizedState funcId in
            body symbolicState (approximateIteratively initialState symbolicState body funcId k)
        | _ when not (Functions.UnboundedRecursionCache.isUnboundedApproximationStarted funcId) ->
            callStack <- Stack.pop callStack
            k (result, State.pop finalState)
        | _ when Functions.UnboundedRecursionCache.approximate funcId result finalState ->
            callStack <- Stack.pop callStack
            Functions.UnboundedRecursionCache.invokeUnboundedRecursion
                initialState
                funcId
                (fun (result, state) -> k (result, State.pop finalState))
        | _ ->
            body symbolicState (approximateIteratively initialState symbolicState body funcId k)

    let internal call state funcId body returnType k =
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
        let pathCondition = State.pathCondition state in
        let frame = (funcId, pathCondition) in
        let shouldStopUnrolling = detectUnboundRecursion frame in
        if shouldStopUnrolling then
            if Functions.UnboundedRecursionCache.isUnboundedApproximationStarted funcId then
                Functions.UnboundedRecursionCache.invokeUnboundedRecursion state funcId k
            else
                Functions.UnboundedRecursionCache.startUnboundedApproximation state funcId returnType
                k (Rollback funcId, state)
        else
            callStack <- Stack.push callStack frame
            body state (approximateIteratively state State.empty body funcId k)
