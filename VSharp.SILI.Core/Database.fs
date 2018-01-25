namespace VSharp.Core

open System.Collections.Generic

module internal Database =
    let private exploredResults= new Dictionary<IFunctionIdentifier, statementResult>()
    let private exploredExceptionGuards = new Dictionary<IFunctionIdentifier, term>()
    let private exploredExceptions = new Dictionary<IFunctionIdentifier, term>()
    let private exploredStates = new Dictionary<IFunctionIdentifier, state>()

    let report id (result, state) =
        exploredResults.Add(id, result) |> ignore
        exploredStates.Add(id, state) |> ignore
        let thrown, _ = ControlFlow.pickOutExceptions result
        match thrown with
        | Some(g, e) ->
            exploredExceptionGuards.Add(id, g)
            exploredExceptions.Add(id, e)
        | None -> ()

    let query id =
        assert(exploredResults.ContainsKey id = exploredStates.ContainsKey id)
        if exploredResults.ContainsKey id then Some(exploredResults.[id], exploredStates.[id]) else None

    let queryState id =
        if exploredStates.ContainsKey id then Some exploredStates.[id] else None

    let queryExceptionsGuard id =
        if exploredExceptionGuards.ContainsKey id then Some exploredExceptionGuards.[id] else None

    let queryExceptions id =
        if exploredExceptions.ContainsKey id then Some exploredExceptions.[id] else None
