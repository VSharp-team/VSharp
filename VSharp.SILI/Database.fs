namespace VSharp

open System.Collections.Generic

module Database =
    let private exploredResults= new Dictionary<FunctionIdentifier, StatementResult>()
    let private exploredExceptionGuards = new Dictionary<FunctionIdentifier, Term>()
    let private exploredExceptions = new Dictionary<FunctionIdentifier, Term>()
    let private exploredStates = new Dictionary<FunctionIdentifier, State.state>()

    let internal report id (result, state) =
        exploredResults.Add(id, result) |> ignore
        exploredStates.Add(id, state) |> ignore
        let thrown, _ = ControlFlow.pickOutExceptions result in
        match thrown with
        | Some(g, e) ->
            exploredExceptionGuards.Add(id, g)
            exploredExceptions.Add(id, e)
        | None -> ()

    let internal query id =
        assert(exploredResults.ContainsKey id = exploredStates.ContainsKey id)
        if exploredResults.ContainsKey id then Some(exploredResults.[id], exploredStates.[id]) else None

    let internal queryState id =
        if exploredStates.ContainsKey id then Some exploredStates.[id] else None

    let internal queryExceptionsGuard id =
        if exploredExceptionGuards.ContainsKey id then Some exploredExceptionGuards.[id] else None

    let internal queryExceptions id =
        if exploredExceptions.ContainsKey id then Some exploredExceptions.[id] else None
